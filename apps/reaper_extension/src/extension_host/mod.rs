//! SHM Extension Host - Manages ROAM SHM guest processes
//!
//! This module implements the host side of the SHM extension architecture:
//! - Creates and manages the shared memory segment
//! - Spawns extension guest processes
//! - Provides ROAM clients for communicating with guests
//! - Handles hot-reload by respawning processes
//!
//! Each extension runs as a separate process (SHM guest) for crash isolation.
//! Communication happens via ROAM over shared memory.

mod shm_hub;
mod extension_registry;
mod hot_reload;
mod host_service_impl;
mod tracing_aggregator;
mod peer_registry;

pub use shm_hub::ShmHub;
pub use extension_registry::{ExtensionRegistry, ExtensionGuest, ExtensionMetadata};
pub use hot_reload::{HotReloadWatcher, WatchEvent};
pub use host_service_impl::{HostServiceImpl, create_host_dispatcher, init_command_queue, process_host_commands};
pub use tracing_aggregator::TracingAggregator;
pub use peer_registry::{PeerRegistry, global_peer_registry};

use anyhow::{Context, Result};
use crossbeam_channel::{bounded, Receiver, Sender};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::time::Duration;
use tracing::{info, warn, error, debug};

/// Extension operation requested by the file watcher
enum ExtensionOperation {
    Load(PathBuf),
    Reload(String, std::time::SystemTime),
    Unload(String),
}

/// Main extension host - manages all SHM guest extensions
pub struct ExtensionHost {
    /// Shared memory hub for guest communication
    shm_hub: ShmHub,

    /// Registry of loaded extensions
    registry: ExtensionRegistry,

    /// File watcher for hot-reload
    watcher: HotReloadWatcher,

    /// Extension directory path
    extension_dir: PathBuf,

    /// Sender for extension operations (from watcher thread)
    operation_tx: Sender<ExtensionOperation>,

    /// Receiver for extension operations (processed on main thread)
    operation_rx: Receiver<ExtensionOperation>,

    /// Last modification times for extensions (to avoid reload loops)
    last_modified: HashMap<String, std::time::SystemTime>,

    /// Extensions that were unloaded and should be reloaded if they reappear
    pending_reload: std::collections::HashSet<String>,

    /// Tokio runtime handle for async operations
    runtime_handle: tokio::runtime::Handle,
}

impl std::fmt::Debug for ExtensionHost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ExtensionHost")
            .field("extension_dir", &self.extension_dir)
            .field("registry", &self.registry)
            .field("runtime_handle", &"<tokio::runtime::Handle>")
            .finish()
    }
}

impl ExtensionHost {
    /// Create a new extension host with the given SHM path and extension directory
    pub fn new(shm_path: &Path, extension_dir: &Path, runtime_handle: &tokio::runtime::Handle) -> Result<Self> {
        info!("Initializing SHM Extension Host");
        info!("  SHM path: {}", shm_path.display());
        info!("  Extension directory: {}", extension_dir.display());

        // Create shared memory hub
        let shm_hub = ShmHub::create(shm_path, runtime_handle)
            .context("Failed to create SHM hub")?;

        // Create extension registry
        let registry = ExtensionRegistry::new();

        // Create hot-reload watcher
        let watcher = HotReloadWatcher::new()?;

        // Create channel for extension operations
        let (operation_tx, operation_rx) = bounded(100);

        Ok(Self {
            shm_hub,
            registry,
            watcher,
            extension_dir: extension_dir.to_path_buf(),
            operation_tx,
            operation_rx,
            last_modified: HashMap::new(),
            pending_reload: HashSet::new(),
            runtime_handle: runtime_handle.clone(),
        })
    }

    /// Discover and load all extensions from the extension directory (recursive)
    pub fn discover_extensions(&mut self) -> Result<()> {
        info!("Discovering extensions in: {}", self.extension_dir.display());

        if !self.extension_dir.exists() {
            info!("Extension directory does not exist, creating: {}", self.extension_dir.display());
            std::fs::create_dir_all(&self.extension_dir)?;
        }

        // Clone the path to avoid borrow checker issues
        let extension_dir = self.extension_dir.clone();

        // Recursively scan for extension binaries
        if extension_dir.exists() {
            self.scan_directory_recursive(&extension_dir)?;
        }

        Ok(())
    }

    /// Start watching the extension directory for changes (call after discover_extensions)
    pub fn start_watching(&mut self) -> Result<()> {
        let extension_dir = self.extension_dir.clone();
        let operation_tx = self.operation_tx.clone();

        info!("🔄 Starting hot-reload watcher for: {}", extension_dir.display());

        self.watcher.watch_directory(&extension_dir, move |event| {
            match event {
                WatchEvent::FileCreated(path) => {
                    info!("🆕 File created: {}", path.display());
                    debug!("   Queuing Load operation for: {}", path.display());
                    // Queue load operation
                    if let Err(e) = operation_tx.send(ExtensionOperation::Load(path.clone())) {
                        error!("Failed to queue Load operation for {}: {}", path.display(), e);
                    }
                }
                WatchEvent::FileModified(path) => {
                    // Get file's modification time to avoid reload loops
                    if let Ok(metadata) = std::fs::metadata(&path) {
                        if let Ok(modified) = metadata.modified() {
                            debug!("File modified: {} (mtime: {:?})", path.display(), modified);

                            // Extract extension name from path
                            if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                                // Send reload with modification time for deduplication
                                let _ = operation_tx.send(ExtensionOperation::Reload(name.to_string(), modified));
                            }
                        }
                    }
                }
                WatchEvent::FileRemoved(path) => {
                    info!("🗑️  File removed: {}", path.display());
                    // Extract extension name from path
                    if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                        let _ = operation_tx.send(ExtensionOperation::Unload(name.to_string()));
                    }
                }
            }
        })?;

        info!("✅ Hot-reload watcher started");

        Ok(())
    }

    /// Process pending extension operations (call periodically from main thread)
    pub fn process_operations(&mut self) {
        // Check if any pending_reload extensions have reappeared (symlink rebuild detection)
        self.check_pending_reloads();

        // Process all pending operations
        while let Ok(operation) = self.operation_rx.try_recv() {
            match operation {
                ExtensionOperation::Load(path) => {
                    // Extract extension name
                    let extension_name = path.file_stem()
                        .and_then(|s| s.to_str())
                        .map(|s| s.to_string());

                    // Check if this is a reload scenario (extension was previously loaded)
                    let is_reload = extension_name.as_ref()
                        .map(|name| self.pending_reload.contains(name))
                        .unwrap_or(false);

                    // Check if file is executable before loading
                    if self.is_executable(&path) {
                        if is_reload {
                            info!("🔄 Reloading extension after rebuild: {}", path.display());
                        } else {
                            info!("📦 Loading new extension: {}", path.display());
                        }

                        if let Err(e) = self.load_extension(&path) {
                            error!("❌ Failed to load extension {}: {:#}", path.display(), e);
                        } else {
                            // Track modification time for this extension
                            if let Some(ref name) = extension_name {
                                if let Ok(metadata) = std::fs::metadata(&path) {
                                    if let Ok(modified) = metadata.modified() {
                                        self.last_modified.insert(name.clone(), modified);
                                    }
                                }
                                // Remove from pending_reload if it was there
                                self.pending_reload.remove(name);
                            }
                        }
                    } else {
                        debug!("Ignoring non-executable file: {}", path.display());
                    }
                }
                ExtensionOperation::Reload(name, modified_time) => {
                    // Check if this is actually a new modification
                    let should_reload = self.last_modified
                        .get(&name)
                        .map(|last_time| modified_time > *last_time)
                        .unwrap_or(true); // Reload if we don't have a recorded time

                    if should_reload {
                        info!("🔄 Reloading extension: {} (mtime changed)", name);
                        if let Err(e) = self.reload_extension(&name) {
                            error!("❌ Failed to reload extension {}: {:#}", name, e);
                        } else {
                            // Update last modification time
                            self.last_modified.insert(name.clone(), modified_time);
                        }
                    } else {
                        debug!("Ignoring duplicate reload event for: {} (mtime unchanged)", name);
                    }
                }
                ExtensionOperation::Unload(name) => {
                    info!("🗑️  Unloading extension: {} (will auto-reload if file reappears)", name);
                    if let Some(mut guest) = self.registry.unregister(&name) {
                        let _ = guest.process.kill();
                        let _ = guest.process.wait();

                        // Unregister from peer registry
                        global_peer_registry().unregister_peer(&name);

                        // Mark for reload when file reappears (rebuild scenario)
                        self.pending_reload.insert(name.clone());

                        info!("✅ Extension unloaded: {} (watching for reload)", name);
                    }
                }
            }
        }
    }

    /// Recursively scan a directory for extension binaries
    fn scan_directory_recursive(&mut self, dir: &std::path::Path) -> Result<()> {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let path = entry.path();

            if path.is_dir() {
                // Recursively scan subdirectories
                if let Err(e) = self.scan_directory_recursive(&path) {
                    warn!("Failed to scan directory {}: {}", path.display(), e);
                }
            } else if path.is_file() {
                // Check if file is executable
                if self.is_executable(&path) {
                    debug!("Found potential extension: {}", path.display());

                    if let Err(e) = self.load_extension(&path) {
                        warn!("Failed to load extension {}: {}", path.display(), e);
                    }
                }
            }
        }

        Ok(())
    }

    /// Check if a file is executable
    fn is_executable(&self, path: &std::path::Path) -> bool {
        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt;
            if let Ok(metadata) = std::fs::metadata(path) {
                let permissions = metadata.permissions();
                return permissions.mode() & 0o111 != 0;
            }
            false
        }

        #[cfg(windows)]
        {
            // On Windows, check for .exe extension
            path.extension().and_then(|s| s.to_str()) == Some("exe")
        }
    }

    /// Load a single extension from the given path (async version)
    pub async fn load_extension_async(&mut self, extension_path: &Path) -> Result<()> {
        info!("Loading extension: {}", extension_path.display());

        // Step 1: Spawn the extension process (allocates peer ID internally)
        let (peer_id, child) = self.shm_hub.spawn_guest(extension_path).await
            .context("Failed to spawn guest process")?;

        info!("  Extension spawned with peer ID: {}", peer_id);

        // Step 2: Extract extension name for tracing
        let extension_name = extension_path.file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();

        // Step 3: Register the peer and get connection handle
        let handle = self.shm_hub.register_peer(peer_id, Some(extension_name.clone())).await
            .context("Failed to register peer")?;

        info!("  Extension registered successfully");

        // Step 3.5: Register peer handle in global registry for cross-extension calls
        global_peer_registry().register_peer(extension_name.clone(), handle.clone());

        // Step 4: Register in our local registry

        let metadata = ExtensionMetadata {
            name: extension_name.clone(),
            path: extension_path.to_path_buf(),
            peer_id,
        };

        let guest = ExtensionGuest {
            metadata,
            process: child,
        };

        self.registry.register(extension_name.clone(), guest);

        // Track modification time to avoid reload loops
        if let Ok(metadata) = std::fs::metadata(extension_path) {
            if let Ok(modified) = metadata.modified() {
                self.last_modified.insert(extension_name.clone(), modified);
                debug!("Tracked mtime for {}: {:?}", extension_name, modified);
            }
        }

        info!("✅ Extension loaded successfully: {}", extension_name);

        Ok(())
    }

    /// Load a single extension from the given path (sync wrapper for backward compatibility)
    pub fn load_extension(&mut self, extension_path: &Path) -> Result<()> {
        // Clone the runtime handle to avoid borrow checker issues
        let runtime_handle = self.runtime_handle.clone();
        // Use our runtime handle to run the async version
        runtime_handle.block_on(self.load_extension_async(extension_path))
    }

    /// Reload a extension by name
    pub fn reload_extension(&mut self, name: &str) -> Result<()> {
        info!("Reloading extension: {}", name);

        // For now, just unload and reload
        // TODO: Implement hot-reload with state preservation
        let mut guest = self.registry.unregister(name)
            .context("Extension not found")?;

        let extension_path = guest.metadata.path.clone();

        // Kill old process
        let child = &mut guest.process;
        child.kill()
            .context("Failed to kill old process")?;
        child.wait()
            .context("Failed to wait for old process")?;

        info!("  Old process terminated");

        // Unregister from peer registry
        global_peer_registry().unregister_peer(name);

        // Free old peer ID
        self.shm_hub.free_peer_id(guest.metadata.peer_id);

        // Reload as new extension (will get new peer ID)
        self.load_extension(&extension_path)
            .context("Guest failed to reattach within timeout")?;

        info!("  New process attached");

        // TODO: Restore extension state via ROAM call

        info!("Extension reloaded successfully: {}", name);

        Ok(())
    }

    /// Get list of loaded extension names
    pub fn list_extensions(&self) -> Vec<String> {
        self.registry.list()
    }

    /// Check if any pending_reload extensions have reappeared on disk
    /// This handles the case where file watcher doesn't detect symlink target changes
    fn check_pending_reloads(&mut self) {
        if self.pending_reload.is_empty() {
            return;
        }

        // Clone the set to avoid borrow issues
        let pending: Vec<String> = self.pending_reload.iter().cloned().collect();

        for extension_name in pending {
            // Construct expected path
            let extension_path = self.extension_dir.join(&extension_name);

            // Check if file exists now
            if extension_path.exists() {
                // Get modification time
                if let Ok(metadata) = std::fs::metadata(&extension_path) {
                    if let Ok(modified) = metadata.modified() {
                        // Check if this is actually newer than what we had before
                        let is_newer = self.last_modified
                            .get(&extension_name)
                            .map(|last_time| modified > *last_time)
                            .unwrap_or(true);

                        if is_newer && self.is_executable(&extension_path) {
                            info!("🔄 Auto-reload detected: {} has reappeared with newer mtime", extension_name);

                            // Reload the extension
                            if let Err(e) = self.load_extension(&extension_path) {
                                error!("❌ Failed to auto-reload extension {}: {:#}", extension_name, e);
                            } else {
                                // Update modification time and remove from pending
                                self.last_modified.insert(extension_name.clone(), modified);
                                self.pending_reload.remove(&extension_name);
                            }
                        }
                    }
                }
            }
        }
    }

    /// Shutdown the extension host and all guests
    pub fn shutdown(&mut self) -> Result<()> {
        info!("Shutting down extension host");

        for name in self.registry.list() {
            if let Some(mut guest) = self.registry.unregister(&name) {
                info!("  Terminating extension: {}", name);

                // Unregister from peer registry
                global_peer_registry().unregister_peer(&name);

                let _ = guest.process.kill();
                let _ = guest.process.wait();
            }
        }

        info!("Extension host shutdown complete");

        Ok(())
    }
}

impl Drop for ExtensionHost {
    fn drop(&mut self) {
        let _ = self.shutdown();
    }
}
