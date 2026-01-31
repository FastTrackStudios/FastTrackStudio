//! SHM Hub - Manages the ROAM shared memory segment and guest spawning
//!
//! Implements the host side of the ROAM SHM specification using true lazy spawning:
//! - Creates and manages the SHM segment
//! - Spawns guest processes dynamically with spawn tickets
//! - Tracks peer connections via MultiPeerHostDriver
//! - Provides connection handles for RPC communication
//! - Aggregates tracing from all extensions

use anyhow::{Context, Result};
use roam_session::{ConnectionHandle, RoutedDispatcher};
use roam_shm::layout::SegmentConfig;
use roam_shm::driver::{MultiPeerHostDriver, MultiPeerHostDriverHandle};
use roam_shm::host::ShmHost;
use roam_shm::spawn::{AddPeerOptions, SpawnTicket};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::process::{Child, Command};
use std::sync::Arc;
use parking_lot::RwLock;
use tracing::{debug, info, warn};

use super::tracing_aggregator::TracingAggregator;
use extension_runtime::ServiceRegistry;
use daw_reaper::{ReaperTransport, TransportDispatcher};

/// SHM Hub - host side of ROAM shared memory communication
pub struct ShmHub {
    /// Path to the SHM segment file
    shm_path: PathBuf,

    /// Driver handle for dynamic peer management
    driver_handle: MultiPeerHostDriverHandle,

    /// Join handle for the driver task
    driver_task: tokio::task::JoinHandle<()>,

    /// Connection handles per peer ID
    /// These are used to make RPC calls to extensions
    connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,

    /// Extension name registry (name → peer_id)
    /// Used for service discovery and forwarding
    extension_names: Arc<RwLock<HashMap<String, u8>>>,

    /// Tracing aggregator for collecting events from all extensions
    tracing: Arc<TracingAggregator>,

    /// Spawn tickets (kept alive to prevent doorbell FD from closing)
    /// CRITICAL: Must not be dropped until child process has established connection
    spawn_tickets: Arc<RwLock<HashMap<u8, SpawnTicket>>>,

    /// Service registry for managing services across extensions
    service_registry: ServiceRegistry,
}

impl ShmHub {
    /// Create a new SHM hub at the given path
    pub fn create(shm_path: &Path, runtime_handle: &tokio::runtime::Handle) -> Result<Self> {
        info!("Creating ROAM SHM hub at: {}", shm_path.display());

        // Ensure parent directory exists
        if let Some(parent) = shm_path.parent() {
            std::fs::create_dir_all(parent)
                .context("Failed to create SHM directory")?;
        }

        // Clean up stale SHM files from previous runs (crashed/killed processes)
        info!("Cleaning up stale SHM files...");
        roam_shm::cleanup::cleanup_stale_shm_files("fts-extensions")
            .context("Failed to cleanup stale SHM files")?;
        info!("  Stale SHM cleanup complete");

        // SHM configuration based on dodeca defaults, scaled for extensions
        let config = SegmentConfig {
            max_guests: 16,                      // Support up to 16 concurrent extensions
            ring_size: 128,                      // Ring buffer entries per guest
            slots_per_guest: 16,                 // Message slots per extension
            slot_size: (4 * 1024 * 1024) + 8,    // 4MB per slot + header
            max_payload_size: 4 * 1024 * 1024,   // 4MB max payload
            ..Default::default()
        };

        info!("  max_guests: {}", config.max_guests);
        info!("  ring_size: {}", config.ring_size);
        info!("  slots_per_guest: {}", config.slots_per_guest);
        info!("  slot_size: {} bytes", config.slot_size);

        // Create ROAM SHM host
        let host = ShmHost::create(shm_path, config)
            .context("Failed to create ROAM SHM host")?;

        info!("✅ ROAM SHM segment created successfully");

        // Write meta file for PID tracking (enables cleanup of stale files)
        roam_shm::cleanup::write_meta_file(shm_path)
            .context("Failed to write meta file")?;
        info!("  Meta file written for PID tracking");

        // Spawn watchdog process that cleans up SHM on parent death (SIGKILL/crash)
        #[cfg(unix)]
        {
            roam_shm::cleanup::spawn_watchdog(shm_path.to_path_buf())
                .context("Failed to spawn cleanup watchdog")?;
            info!("  Cleanup watchdog spawned (auto-cleanup on crash)");
        }

        // Build MultiPeerHostDriver with no initial peers (true lazy spawning)
        let builder = MultiPeerHostDriver::builder(host);
        let (driver, _handles, _incoming, driver_handle) = builder.build();

        info!("  MultiPeerHostDriver created (true lazy spawning)");

        // Spawn driver task on tokio runtime using provided handle
        let driver_task = runtime_handle.spawn(async move {
            info!("ROAM SHM driver task starting...");
            if let Err(e) = driver.run().await {
                warn!("ROAM SHM driver error: {:?}", e);
            }
            info!("ROAM SHM driver task exited");
        });

        info!("✅ ROAM SHM driver spawned");

        // Create tracing aggregator for all extensions
        let tracing = Arc::new(TracingAggregator::new(4096));
        tracing.start_consumer(runtime_handle);
        info!("✅ Tracing aggregator started");

        Ok(Self {
            shm_path: shm_path.to_path_buf(),
            driver_handle,
            driver_task,
            connections: Arc::new(RwLock::new(HashMap::new())),
            extension_names: Arc::new(RwLock::new(HashMap::new())),
            tracing,
            spawn_tickets: Arc::new(RwLock::new(HashMap::new())),
            service_registry: ServiceRegistry::new(),
        })
    }

    /// Allocate a peer slot and spawn a guest process
    ///
    /// Uses true lazy spawning pattern:
    /// 1. Create peer slot with driver_handle.create_peer()
    /// 2. Spawn process with ticket.spawn()
    /// 3. Register peer with driver_handle.add_peer()
    ///
    /// Returns the peer ID and the spawned Child process
    pub async fn spawn_guest(&self, extension_path: &Path) -> Result<(u8, Child)> {
        debug!("Spawning guest: {}", extension_path.display());

        // Step 1: Create peer slot and get spawn ticket
        let ticket = self
            .driver_handle
            .create_peer(AddPeerOptions::default())
            .await
            .context("Failed to create peer slot")?;

        let peer_id = ticket.peer_id.get();
        info!("  Allocated peer ID: {}", peer_id);
        info!("  SHM path: {}", self.shm_path.display());

        // Step 2: Build command and spawn process with ticket
        let mut cmd = Command::new(extension_path);

        // Pass through RUST_LOG for extension logging
        if let Ok(rust_log) = std::env::var("RUST_LOG") {
            cmd.env("RUST_LOG", rust_log);
        }

        // TEMPORARY: Force TRACING_PASSTHROUGH for debugging
        // This makes extensions log to stderr instead of RPC
        cmd.env("TRACING_PASSTHROUGH", "1");

        let child = ticket
            .spawn(cmd)
            .with_context(|| format!("Failed to spawn extension: {}", extension_path.display()))?;

        info!("✅ Guest spawned: PID {}", child.id());

        // CRITICAL: Store the ticket to keep doorbell FD alive
        // The ticket owns the doorbell file descriptor. If we drop it, the FD closes
        // and the child process cannot establish the SHM connection.
        self.spawn_tickets.write().insert(peer_id, ticket);

        Ok((peer_id, child))
    }

    /// Register a spawned peer and get its connection handle
    ///
    /// This should be called after spawn_guest() to register the peer's dispatcher
    /// and receive a ConnectionHandle for RPC calls.
    ///
    /// The extension_name is used for tracing aggregation to identify which extension
    /// emitted which log messages.
    pub async fn register_peer(&self, peer_id: u8, extension_name: Option<String>) -> Result<ConnectionHandle> {
        debug!("Registering peer {} (name: {:?})", peer_id, extension_name);

        // Create extension host dispatcher for lifecycle management
        let extension_host_dispatcher = super::extension_host_impl::create_extension_host_dispatcher(
            self.connections.clone(),
            self.extension_names.clone(),
            self.service_registry.clone(),
        );

        // Create DAW transport dispatcher
        // Uses ReaperTransport directly (REAPER API)
        let transport = ReaperTransport::new();
        let transport_dispatcher = TransportDispatcher::new(transport);

        // Create tracing dispatcher for this peer
        let tracing_dispatcher = self.tracing.dispatcher_for_peer(peer_id, extension_name.clone());

        // Combine dispatchers:
        // Tracing (primary) handles tracing calls
        // Transport (fallback 1) handles DAW service calls
        // ExtensionHost (fallback 2) handles lifecycle calls
        let daw_dispatcher = RoutedDispatcher::new(
            transport_dispatcher,
            extension_host_dispatcher,
        );

        let combined_dispatcher = RoutedDispatcher::new(
            tracing_dispatcher,
            daw_dispatcher,
        );

        // Register peer with driver and get connection handle
        use roam_shm::peer::PeerId;
        let roam_peer_id = PeerId::new(peer_id)
            .ok_or_else(|| anyhow::anyhow!("Invalid peer ID: {}", peer_id))?;

        // add_peer returns (ConnectionHandle, IncomingConnections)
        let (handle, _incoming): (ConnectionHandle, _) = self
            .driver_handle
            .add_peer(roam_peer_id, combined_dispatcher)
            .await
            .context("Failed to register peer")?;

        info!("✅ Peer {} registered successfully", peer_id);

        // Store connection handle
        self.connections.write().insert(peer_id, handle.clone());

        // Store extension name mapping if provided
        if let Some(ref name) = extension_name {
            self.extension_names.write().insert(name.clone(), peer_id);
            info!("  Registered service name: {} → peer {}", name, peer_id);
        }

        Ok(handle)
    }

    /// Get a connection handle for a peer (for making RPC calls)
    pub fn get_connection(&self, peer_id: u8) -> Option<ConnectionHandle> {
        self.connections.read().get(&peer_id).cloned()
    }

    /// Get a connection handle for a service by name
    pub fn get_connection_by_name(&self, service_name: &str) -> Option<ConnectionHandle> {
        let peer_id = self.extension_names.read().get(service_name).copied()?;
        self.connections.read().get(&peer_id).cloned()
    }

    /// Free a peer ID when a guest disconnects
    pub fn free_peer_id(&mut self, peer_id: u8) {
        self.connections.write().remove(&peer_id);

        // Remove extension name mapping
        self.extension_names.write().retain(|_, &mut id| id != peer_id);

        debug!("Freed peer ID: {}", peer_id);
    }

    /// Get the SHM path
    pub fn path(&self) -> &Path {
        &self.shm_path
    }
}

impl Drop for ShmHub {
    fn drop(&mut self) {
        info!("Shutting down ROAM SHM hub...");

        // Abort driver task
        self.driver_task.abort();

        // Clean up SHM segment
        if self.shm_path.exists() {
            let _ = std::fs::remove_file(&self.shm_path);
            debug!("SHM segment removed: {}", self.shm_path.display());
        }

        info!("✅ ROAM SHM hub shutdown complete");
    }
}
