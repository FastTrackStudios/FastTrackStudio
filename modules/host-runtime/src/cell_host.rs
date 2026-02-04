//! Cell Host - Manages cell lifecycle with hot-reload support
//!
//! This module provides:
//! - Cell discovery and loading from a watched directory
//! - Hot-reload support via file watching
//! - Cell lifecycle management (spawn, reload, unload)
//!
//! Cells are spawned as separate processes communicating via ROAM over shared memory.
//! When a cell binary is modified, it's automatically reloaded.
//!
//! # Example
//!
//! ```ignore
//! use host_runtime::{CellHost, Host};
//! use std::path::PathBuf;
//!
//! let cell_dir = PathBuf::from("target/debug");
//! let runtime_handle = tokio::runtime::Handle::current();
//!
//! let mut cell_host = CellHost::new(cell_dir, runtime_handle)?;
//!
//! // Optionally register a callback for when cells are spawned
//! cell_host.set_on_cell_spawned(|cell_name, handle| {
//!     println!("Cell {} spawned with handle", cell_name);
//! });
//!
//! cell_host.start_watching()?;
//!
//! // In your event loop:
//! cell_host.process_operations();
//! ```

use crate::host::Host;
use crate::hot_reload::{HotReloadWatcher, WatchEvent};
use anyhow::Result;
use crossbeam_channel::{bounded, Receiver, Sender};
use roam::session::ConnectionHandle;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::SystemTime;
use tracing::{debug, error, info, warn};

/// Cell operation requested by the file watcher
#[derive(Debug)]
pub enum CellOperation {
    /// Load a new cell from the given path
    Load(PathBuf),
    /// Reload an existing cell (name, modification time for deduplication)
    Reload(String, SystemTime),
    /// Unload a cell by name
    Unload(String),
}

/// Callback invoked when a cell is spawned or reloaded.
/// Receives the cell name and its connection handle.
pub type OnCellSpawnedCallback = Arc<dyn Fn(&str, ConnectionHandle) + Send + Sync + 'static>;

/// Manages cell lifecycle with hot-reload support
pub struct CellHost {
    /// File watcher for the cell directory (Extensions/FTS2)
    watcher: Option<HotReloadWatcher>,

    /// File watcher for the source directory (target/debug) - for development
    source_watcher: Option<HotReloadWatcher>,

    /// Cell directory path (where cells are loaded from)
    cell_dir: PathBuf,

    /// Source directory path (where cells are built) - for symlink-based dev workflow
    source_dir: Option<PathBuf>,

    /// Sender for cell operations (from watcher thread)
    operation_tx: Sender<CellOperation>,

    /// Receiver for cell operations (processed on main thread)
    operation_rx: Receiver<CellOperation>,

    /// Last modification times for cells (to avoid reload loops)
    last_modified: HashMap<String, SystemTime>,

    /// Cells that were unloaded and should be reloaded if they reappear
    pending_reload: HashSet<String>,

    /// Currently loaded cell names
    loaded_cells: HashSet<String>,

    /// Tokio runtime handle for async operations
    runtime_handle: tokio::runtime::Handle,

    /// Optional callback invoked when a cell is spawned
    on_cell_spawned: Option<OnCellSpawnedCallback>,
}

impl std::fmt::Debug for CellHost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("CellHost")
            .field("cell_dir", &self.cell_dir)
            .field("loaded_cells", &self.loaded_cells)
            .field("pending_reload", &self.pending_reload)
            .finish()
    }
}

impl CellHost {
    /// Create a new cell host for the given directory
    pub fn new(cell_dir: PathBuf, runtime_handle: tokio::runtime::Handle) -> Result<Self> {
        info!("Initializing Cell Host");
        info!("  Cell directory: {}", cell_dir.display());

        // Create channel for cell operations
        let (operation_tx, operation_rx) = bounded(100);

        Ok(Self {
            watcher: None,
            source_watcher: None,
            cell_dir,
            source_dir: None,
            operation_tx,
            operation_rx,
            last_modified: HashMap::new(),
            pending_reload: HashSet::new(),
            loaded_cells: HashSet::new(),
            runtime_handle,
            on_cell_spawned: None,
        })
    }

    /// Set a callback to be invoked when a cell is spawned or reloaded.
    ///
    /// This is useful for registering cell-specific functionality like actions.
    ///
    /// # Example
    ///
    /// ```ignore
    /// cell_host.set_on_cell_spawned(|cell_name, handle| {
    ///     // Register actions for this cell
    ///     action_registry::register_cell(cell_name, handle).await;
    /// });
    /// ```
    pub fn set_on_cell_spawned<F>(&mut self, callback: F)
    where
        F: Fn(&str, ConnectionHandle) + Send + Sync + 'static,
    {
        self.on_cell_spawned = Some(Arc::new(callback));
    }

    /// Get the cell directory path
    pub fn cell_dir(&self) -> &Path {
        &self.cell_dir
    }

    /// Start watching the cell directory for changes
    pub fn start_watching(&mut self) -> Result<()> {
        // Ensure the directory exists
        if !self.cell_dir.exists() {
            info!(
                "Cell directory does not exist, creating: {}",
                self.cell_dir.display()
            );
            std::fs::create_dir_all(&self.cell_dir)?;
        }

        let cell_dir = self.cell_dir.clone();
        let operation_tx = self.operation_tx.clone();

        info!("Starting hot-reload watcher for: {}", cell_dir.display());

        let mut watcher = HotReloadWatcher::new()?;

        watcher.watch_directory(&cell_dir, move |event| {
            match event {
                WatchEvent::FileCreated(path) => {
                    // Only process executable files
                    if is_executable(&path) {
                        info!("New cell binary detected: {}", path.display());
                        if let Err(e) = operation_tx.send(CellOperation::Load(path.clone())) {
                            error!("Failed to queue Load operation: {}", e);
                        }
                    }
                }
                WatchEvent::FileModified(path) => {
                    // Get modification time for deduplication
                    if is_executable(&path) {
                        if let Ok(metadata) = std::fs::metadata(&path) {
                            if let Ok(modified) = metadata.modified() {
                                if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                                    debug!(
                                        "Cell binary modified: {} (mtime: {:?})",
                                        name, modified
                                    );
                                    let _ = operation_tx
                                        .send(CellOperation::Reload(name.to_string(), modified));
                                }
                            }
                        }
                    }
                }
                WatchEvent::FileRemoved(path) => {
                    if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                        info!("Cell binary removed: {}", name);
                        let _ = operation_tx.send(CellOperation::Unload(name.to_string()));
                    }
                }
            }
        })?;

        self.watcher = Some(watcher);
        info!("Hot-reload watcher started");

        Ok(())
    }

    /// Watch specific source binaries for development workflow.
    ///
    /// When using symlinks (e.g., `Extensions/FTS2/session` -> `target/debug/session`),
    /// file system watchers may not detect changes to the symlink target. This method
    /// discovers symlinked cells and watches only those specific binaries.
    ///
    /// This is more efficient than watching the entire `target/debug` directory.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let mut cell_host = CellHost::new(cell_dir, runtime_handle)?;
    /// cell_host.start_watching()?;
    /// cell_host.watch_source_binaries()?;
    /// ```
    pub fn watch_source_binaries(&mut self) -> Result<()> {
        // Discover all symlinked cells and their source binaries
        let mut watched_names: std::collections::HashSet<String> = std::collections::HashSet::new();
        let mut source_dir: Option<PathBuf> = None;

        if let Ok(entries) = std::fs::read_dir(&self.cell_dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_symlink() {
                    if let Ok(target) = std::fs::read_link(&path) {
                        let absolute_target = if target.is_absolute() {
                            target
                        } else {
                            self.cell_dir.join(&target)
                        };

                        if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                            info!(
                                "Will watch symlinked cell: {} -> {}",
                                name,
                                absolute_target.display()
                            );
                            watched_names.insert(name.to_string());

                            // Remember the source directory (they should all be in the same dir)
                            if source_dir.is_none() {
                                source_dir = absolute_target.parent().map(|p| p.to_path_buf());
                            }
                        }
                    }
                }
            }
        }

        if watched_names.is_empty() {
            info!("No symlinked cells found, skipping source binary watching");
            return Ok(());
        }

        let source_dir = match source_dir {
            Some(dir) => dir,
            None => {
                warn!("Could not determine source directory");
                return Ok(());
            }
        };

        info!(
            "Watching {} source binaries in: {}",
            watched_names.len(),
            source_dir.display()
        );
        for name in &watched_names {
            info!("  - {}", name);
        }

        let operation_tx = self.operation_tx.clone();
        let source_dir_clone = source_dir.clone();

        let mut watcher = HotReloadWatcher::new()?;

        // Watch the source directory but filter to only our binaries
        watcher.watch_directory(&source_dir, move |event| {
            let handle_event = |path: &Path, event_type: &str| {
                // Only process files directly in the source dir (not subdirs like build/)
                if path.parent() != Some(source_dir_clone.as_path()) {
                    return;
                }

                if let Some(name) = path.file_stem().and_then(|s| s.to_str()) {
                    // Only process binaries we're watching
                    if !watched_names.contains(name) {
                        return;
                    }

                    // Skip non-executable files (like .d files)
                    if !is_executable(path) {
                        return;
                    }

                    if let Ok(metadata) = std::fs::metadata(path) {
                        if let Ok(modified) = metadata.modified() {
                            info!("Source binary {}: {} (triggering reload)", event_type, name);
                            let _ = operation_tx
                                .send(CellOperation::Reload(name.to_string(), modified));
                        }
                    }
                }
            };

            match &event {
                WatchEvent::FileModified(path) => handle_event(path, "modified"),
                WatchEvent::FileCreated(path) => handle_event(path, "created"),
                WatchEvent::FileRemoved(_) => {
                    // Ignore removals - cargo does remove+create for atomic writes
                }
            }
        })?;

        self.source_watcher = Some(watcher);
        self.source_dir = Some(source_dir);
        info!("Source binary watcher started");

        Ok(())
    }

    /// Process pending cell operations (call periodically from main thread timer)
    pub fn process_operations(&mut self) {
        // Check if any pending_reload cells have reappeared (symlink rebuild detection)
        self.check_pending_reloads();

        // Process all pending operations
        while let Ok(operation) = self.operation_rx.try_recv() {
            match operation {
                CellOperation::Load(path) => {
                    self.handle_load(&path);
                }
                CellOperation::Reload(name, modified_time) => {
                    self.handle_reload(&name, modified_time);
                }
                CellOperation::Unload(name) => {
                    self.handle_unload(&name);
                }
            }
        }
    }

    /// Handle loading a new cell
    fn handle_load(&mut self, path: &Path) {
        let cell_name = match path.file_stem().and_then(|s| s.to_str()) {
            Some(name) => name.to_string(),
            None => {
                warn!("Could not extract cell name from path: {}", path.display());
                return;
            }
        };

        // Check if this is a reload scenario
        let is_reload = self.pending_reload.contains(&cell_name);

        if is_reload {
            info!("Reloading cell after rebuild: {}", cell_name);
        } else if self.loaded_cells.contains(&cell_name) {
            debug!("Cell {} already loaded, treating as reload", cell_name);
            // Treat as a reload
            if let Ok(metadata) = std::fs::metadata(path) {
                if let Ok(modified) = metadata.modified() {
                    self.handle_reload(&cell_name, modified);
                }
            }
            return;
        } else {
            info!("Loading new cell: {}", cell_name);
        }

        // Spawn the cell
        let runtime_handle = self.runtime_handle.clone();
        let cell_name_clone = cell_name.clone();
        let on_spawned = self.on_cell_spawned.clone();

        runtime_handle.block_on(async {
            match Host::get().spawn_pending_cell(&cell_name_clone).await {
                Some(handle) => {
                    info!("Cell {} spawned successfully", cell_name_clone);

                    // Invoke the callback if set
                    if let Some(ref callback) = on_spawned {
                        callback(&cell_name_clone, handle);
                    }
                }
                None => {
                    warn!("Failed to spawn cell: {}", cell_name_clone);
                }
            }
        });

        // Track the cell
        self.loaded_cells.insert(cell_name.clone());
        self.pending_reload.remove(&cell_name);

        // Track modification time
        if let Ok(metadata) = std::fs::metadata(path) {
            if let Ok(modified) = metadata.modified() {
                self.last_modified.insert(cell_name, modified);
            }
        }
    }

    /// Handle reloading an existing cell
    fn handle_reload(&mut self, name: &str, modified_time: SystemTime) {
        // Check if this is actually a new modification (debounce)
        let should_reload = self
            .last_modified
            .get(name)
            .map(|last_time| modified_time > *last_time)
            .unwrap_or(true);

        if !should_reload {
            debug!(
                "Ignoring duplicate reload event for: {} (mtime unchanged)",
                name
            );
            return;
        }

        info!("Reloading cell: {}", name);

        // Kill the existing cell process
        let runtime_handle = self.runtime_handle.clone();
        let name_owned = name.to_string();
        let on_spawned = self.on_cell_spawned.clone();

        runtime_handle.block_on(async {
            // Kill the old cell
            if let Err(e) = Host::get().kill_cell(&name_owned).await {
                warn!("Failed to kill old cell {}: {}", name_owned, e);
            }

            // Small delay to ensure process cleanup
            tokio::time::sleep(std::time::Duration::from_millis(100)).await;

            // Spawn the new cell
            match Host::get().spawn_pending_cell(&name_owned).await {
                Some(handle) => {
                    info!("Cell {} reloaded successfully", name_owned);

                    // Invoke the callback if set
                    if let Some(ref callback) = on_spawned {
                        callback(&name_owned, handle);
                    }
                }
                None => {
                    warn!("Failed to respawn cell: {}", name_owned);
                }
            }
        });

        // Update modification time
        self.last_modified.insert(name.to_string(), modified_time);
    }

    /// Handle unloading a cell
    fn handle_unload(&mut self, name: &str) {
        info!(
            "Unloading cell: {} (will auto-reload if file reappears)",
            name
        );

        let runtime_handle = self.runtime_handle.clone();
        let name_owned = name.to_string();

        runtime_handle.block_on(async {
            if let Err(e) = Host::get().kill_cell(&name_owned).await {
                warn!("Failed to kill cell {}: {}", name_owned, e);
            }
        });

        // Mark for reload when file reappears
        self.pending_reload.insert(name.to_string());
        self.loaded_cells.remove(name);

        info!("Cell unloaded: {} (watching for reload)", name);
    }

    /// Check if any pending_reload cells have reappeared on disk
    fn check_pending_reloads(&mut self) {
        if self.pending_reload.is_empty() {
            return;
        }

        let pending: Vec<String> = self.pending_reload.iter().cloned().collect();

        for cell_name in pending {
            let cell_path = self.cell_dir.join(&cell_name);

            if cell_path.exists() && is_executable(&cell_path) {
                if let Ok(metadata) = std::fs::metadata(&cell_path) {
                    if let Ok(modified) = metadata.modified() {
                        let is_newer = self
                            .last_modified
                            .get(&cell_name)
                            .map(|last_time| modified > *last_time)
                            .unwrap_or(true);

                        if is_newer {
                            info!(
                                "Auto-reload detected: {} has reappeared with newer mtime",
                                cell_name
                            );
                            self.handle_load(&cell_path);
                        }
                    }
                }
            }
        }
    }

    /// Get list of loaded cell names
    pub fn list_cells(&self) -> Vec<String> {
        self.loaded_cells.iter().cloned().collect()
    }

    /// Check if a cell is currently loaded
    pub fn is_cell_loaded(&self, name: &str) -> bool {
        self.loaded_cells.contains(name)
    }
}

/// Check if a file is executable
fn is_executable(path: &Path) -> bool {
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
        path.extension().and_then(|s| s.to_str()) == Some("exe")
    }
}
