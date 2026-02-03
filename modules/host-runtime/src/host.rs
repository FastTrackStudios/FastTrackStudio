//! Unified Host singleton for the DAW system.
//!
//! The Host owns all shared state:
//! - Cell infrastructure (SHM, connection handles)
//! - Pending cells (lazy spawning)
//! - Cell tracing
//!
//! Access via `Host::get()`. Get typed cell clients via `Host::client_async::<C>()`.

use std::collections::HashMap;
use std::path::PathBuf;
use std::process::Stdio;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, OnceLock};
use std::time::Duration;

use dashmap::DashMap;
use roam::session::{ConnectionHandle, LateBoundHandle, RoutedDispatcher};
use roam_shm::driver::MultiPeerHostDriverHandle;
use roam_shm::layout::SegmentConfig;
use roam_shm::spawn::AddPeerOptions;
use roam_shm::ShmHost;
use roam_tracing::{HostTracingDispatcher, HostTracingState, TaggedRecord};
use tokio::process::Command;
use tokio::sync::Notify;
use tracing::{debug, error, info};

use crate::cells::{cell_ready_registry, HostServiceImpl};

// ============================================================================
// SHM Infrastructure
// ============================================================================

/// Initialize the SHM host infrastructure and driver.
///
/// Creates a temporary SHM segment and starts the multi-peer driver.
/// Returns the temp directory handle (keep alive for the duration of the program).
///
/// # Example
///
/// ```ignore
/// let _temp_dir = init_shm_infrastructure().await?;
/// // temp_dir keeps the SHM segment alive
/// ```
pub async fn init_shm_infrastructure() -> Result<tempfile::TempDir, Box<dyn std::error::Error>> {
    // Create temp directory for SHM segment
    let temp_dir = tempfile::tempdir()?;
    let shm_path = temp_dir.path().join("daw-hub.shm");

    // Create SHM host
    let config = SegmentConfig::default();
    let shm_host = ShmHost::create(&shm_path, config)?;
    info!("SHM host created at: {}", shm_path.display());

    // Build the driver with no initial peers (lazy spawning)
    let builder = roam_shm::driver::MultiPeerHostDriver::builder(shm_host);
    let (driver, _handles, _incoming, driver_handle) = builder.build();

    // Store driver handle in Host singleton for lazy spawning
    Host::get().set_driver_handle(driver_handle);

    // Spawn the driver task
    tokio::spawn(async move {
        info!("MultiPeerHostDriver starting");
        if let Err(e) = driver.run().await {
            error!(error = ?e, "MultiPeerHostDriver exited with error");
        }
    });

    Ok(temp_dir)
}

/// Get the default cell binary directory (target/debug or target/release).
///
/// Uses CARGO_MANIFEST_DIR at compile time to find the workspace root,
/// or falls back to searching from the current executable's location.
pub fn default_cell_dir() -> PathBuf {
    // CARGO_MANIFEST_DIR for host-runtime is modules/host-runtime
    // We need to go up to workspace root (../../) then into target/debug
    let manifest_based = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent() // modules/
        .and_then(|p| p.parent()) // workspace root
        .map(|p| p.join("target/debug"));

    if let Some(path) = manifest_based {
        if path.exists() {
            return path;
        }
    }

    // Fallback: try to find target/debug relative to current exe
    if let Ok(exe_path) = std::env::current_exe() {
        // exe is in target/debug/, so parent twice then back in
        if let Some(target_debug) = exe_path.parent() {
            if target_debug.ends_with("target/debug") || target_debug.ends_with("target/release") {
                return target_debug.to_path_buf();
            }
        }
    }

    // Final fallback: current directory + target/debug
    PathBuf::from("target/debug")
}

// ============================================================================
// Pending Cell (Lazy Spawning)
// ============================================================================

/// A forwarding target with its method IDs.
#[derive(Clone)]
pub struct ForwardTarget {
    /// Name of the target cell
    pub cell_name: String,
    /// Method IDs that should be forwarded to this target
    pub method_ids: Vec<u64>,
}

/// A cell that has been registered but not yet spawned.
pub struct PendingCell {
    /// Path to the cell binary
    pub binary_path: PathBuf,
    /// Whether the cell inherits stdio (e.g., TUI)
    pub inherit_stdio: bool,
    /// Forwarding targets with their method IDs
    pub forward_targets: Vec<ForwardTarget>,
}

// ============================================================================
// Cell Configuration Builder
// ============================================================================

/// Builder for configuring cells with a fluent API.
///
/// # Example
///
/// ```ignore
/// use daw_proto::{TransportServiceDispatcher, ProjectServiceDispatcher};
/// use session_proto::SetlistServiceDispatcher;
///
/// let cell_dir = PathBuf::from("target/debug");
///
/// CellConfig::new("daw-standalone", &cell_dir)
///     .register();
///
/// CellConfig::new("session", &cell_dir)
///     .forwards_to_with_methods("daw-standalone", || {
///         TransportServiceDispatcher::<()>::method_ids()
///             .into_iter()
///             .chain(ProjectServiceDispatcher::<()>::method_ids())
///             .collect()
///     })
///     .register();
///
/// CellConfig::new("gateway-ws", &cell_dir)
///     .forwards_to_with_methods("daw-standalone", || {
///         TransportServiceDispatcher::<()>::method_ids()
///             .into_iter()
///             .chain(ProjectServiceDispatcher::<()>::method_ids())
///             .collect()
///     })
///     .forwards_to_with_methods("session", || {
///         SetlistServiceDispatcher::<()>::method_ids()
///     })
///     .register();
/// ```
pub struct CellConfig {
    name: String,
    binary_path: PathBuf,
    inherit_stdio: bool,
    forward_targets: Vec<ForwardTarget>,
}

impl CellConfig {
    /// Create a new cell configuration.
    ///
    /// The binary path is constructed as `cell_dir/name`.
    pub fn new(name: &str, cell_dir: &std::path::Path) -> Self {
        Self {
            name: name.to_string(),
            binary_path: cell_dir.join(name),
            inherit_stdio: false,
            forward_targets: Vec::new(),
        }
    }

    /// Add a forwarding target with explicit method IDs.
    ///
    /// The closure is called to get the method IDs, allowing you to use
    /// generated dispatcher method_ids() functions.
    ///
    /// # Example
    ///
    /// ```ignore
    /// CellConfig::new("gateway-ws", &cell_dir)
    ///     .forwards_to_with_methods("daw-standalone", || {
    ///         TransportServiceDispatcher::<()>::method_ids()
    ///     })
    ///     .forwards_to_with_methods("session", || {
    ///         SetlistServiceDispatcher::<()>::method_ids()
    ///     })
    ///     .register();
    /// ```
    pub fn forwards_to_with_methods<F>(mut self, target: &str, method_ids_fn: F) -> Self
    where
        F: FnOnce() -> Vec<u64>,
    {
        self.forward_targets.push(ForwardTarget {
            cell_name: target.to_string(),
            method_ids: method_ids_fn(),
        });
        self
    }

    /// Set the cells this cell forwards calls to (legacy - no method routing).
    ///
    /// **DEPRECATED**: Use `forwards_to_with_methods` for explicit method routing.
    /// This method creates targets with empty method IDs, which falls back to
    /// the first-available behavior.
    #[deprecated(note = "Use forwards_to_with_methods for explicit method routing")]
    pub fn forwards_to(mut self, targets: &[&str]) -> Self {
        for target in targets {
            self.forward_targets.push(ForwardTarget {
                cell_name: target.to_string(),
                method_ids: Vec::new(),
            });
        }
        self
    }

    /// Set whether the cell inherits stdio (for TUI cells).
    #[allow(dead_code)]
    pub fn inherit_stdio(mut self, inherit: bool) -> Self {
        self.inherit_stdio = inherit;
        self
    }

    /// Register this cell with the Host singleton.
    pub fn register(self) {
        Host::get().register_pending_cell(
            self.name,
            PendingCell {
                binary_path: self.binary_path,
                inherit_stdio: self.inherit_stdio,
                forward_targets: self.forward_targets,
            },
        );
    }
}

// ============================================================================
// Host Singleton
// ============================================================================

/// A boxed service dispatcher that can be stored and cloned.
pub type BoxedDispatcher = Arc<dyn roam::session::ServiceDispatcher>;

/// The unified Host that owns all shared state.
pub struct Host {
    /// Signaled when exit is requested.
    exit_notify: Notify,

    /// Connection handles for cells, keyed by logical name.
    cell_handles: DashMap<String, ConnectionHandle>,

    /// Cells that have been registered but not yet spawned.
    pending_cells: Mutex<HashMap<String, PendingCell>>,

    /// Whether quiet mode is enabled (suppress cell output).
    quiet_mode: AtomicBool,

    /// MultiPeerHostDriver handle for dynamically creating peers.
    driver_handle: OnceLock<MultiPeerHostDriverHandle>,

    /// Shared state for receiving tracing records from cells.
    tracing_state: Arc<HostTracingState>,

    /// Cell startup timeout in seconds.
    cell_timeout_secs: u64,

    /// Late-bound handles for forwarding between cells.
    /// Key is the target cell name, value is the late-bound handle.
    /// When a cell is spawned, its handle gets bound to this.
    late_bound_handles: DashMap<String, LateBoundHandle>,

    /// Optional DAW dispatcher for handling DAW service calls in-process.
    /// This is used when the DAW implementation (e.g., ReaperTransport) needs
    /// to run in the host process rather than a separate cell.
    daw_dispatcher: OnceLock<BoxedDispatcher>,
}

impl Host {
    /// Get the global Host singleton. Lazily initializes on first call.
    pub fn get() -> &'static Arc<Host> {
        static HOST: OnceLock<Arc<Host>> = OnceLock::new();
        HOST.get_or_init(|| {
            Arc::new(Host {
                exit_notify: Notify::new(),
                cell_handles: DashMap::new(),
                pending_cells: Mutex::new(HashMap::new()),
                quiet_mode: AtomicBool::new(false),
                driver_handle: OnceLock::new(),
                tracing_state: HostTracingState::new(4096),
                cell_timeout_secs: std::env::var("CELL_TIMEOUT_SECS")
                    .ok()
                    .and_then(|s| s.parse().ok())
                    .unwrap_or(10),
                late_bound_handles: DashMap::new(),
                daw_dispatcher: OnceLock::new(),
            })
        })
    }

    /// Get or create a late-bound handle for forwarding to a cell.
    /// Returns a clone of the LateBoundHandle (they're Arc-based internally).
    pub fn get_or_create_late_bound(&self, cell_name: &str) -> LateBoundHandle {
        self.late_bound_handles
            .entry(cell_name.to_string())
            .or_insert_with(LateBoundHandle::new)
            .clone()
    }

    /// Bind a cell's connection handle to its late-bound handle.
    /// This enables other cells that forward to this cell to start working.
    pub fn bind_late_bound(&self, cell_name: &str, handle: ConnectionHandle) {
        if let Some(late_bound) = self.late_bound_handles.get(cell_name) {
            late_bound.set(handle);
            info!(cell = cell_name, "Late-bound handle bound");
        }
    }

    /// Signal that exit was requested.
    pub fn signal_exit(&self) {
        self.exit_notify.notify_waiters();
    }

    /// Wait for exit to be signaled.
    #[allow(dead_code)]
    pub async fn wait_for_exit(&self) {
        self.exit_notify.notified().await;
    }

    // =========================================================================
    // Cell Handle Management
    // =========================================================================

    /// Register a cell's connection handle.
    pub fn register_cell_handle(&self, cell_name: String, handle: ConnectionHandle) {
        self.cell_handles.insert(cell_name, handle);
    }

    /// Get a cell's connection handle by logical name.
    pub fn get_cell_handle(&self, cell_name: &str) -> Option<ConnectionHandle> {
        self.cell_handles.get(cell_name).map(|r| r.clone())
    }

    // =========================================================================
    // Quiet Mode
    // =========================================================================

    /// Enable quiet mode for spawned cells.
    #[allow(dead_code)]
    pub fn set_quiet_mode(&self, quiet: bool) {
        self.quiet_mode.store(quiet, Ordering::SeqCst);
    }

    /// Check if quiet mode is enabled.
    pub fn is_quiet_mode(&self) -> bool {
        self.quiet_mode.load(Ordering::SeqCst)
    }

    // =========================================================================
    // Driver Handle
    // =========================================================================

    /// Set the driver handle for dynamic peer creation (lazy spawning).
    pub fn set_driver_handle(&self, handle: MultiPeerHostDriverHandle) {
        let _ = self.driver_handle.set(handle);
    }

    /// Get the driver handle for creating peers dynamically.
    pub fn driver_handle(&self) -> Option<&MultiPeerHostDriverHandle> {
        self.driver_handle.get()
    }

    // =========================================================================
    // Cell Tracing
    // =========================================================================

    /// Get the tracing state for creating per-cell tracing services.
    pub fn tracing_state(&self) -> &Arc<HostTracingState> {
        &self.tracing_state
    }

    /// Take the tracing record receiver.
    pub fn take_tracing_receiver(&self) -> Option<tokio::sync::mpsc::Receiver<TaggedRecord>> {
        self.tracing_state.take_receiver()
    }

    // =========================================================================
    // DAW Dispatcher (In-Process)
    // =========================================================================

    /// Set the DAW dispatcher for handling DAW service calls in-process.
    ///
    /// This is used when the DAW implementation (e.g., ReaperTransport, ReaperProject)
    /// needs to run in the host process rather than a separate cell. The dispatcher
    /// is included in the fallback chain for all spawned cells.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let transport = ReaperTransport::new();
    /// let project = ReaperProject::new();
    /// let dispatcher = RoutedDispatcher::new(
    ///     TransportServiceDispatcher::new(transport),
    ///     ProjectServiceDispatcher::new(project),
    /// );
    /// Host::get().set_daw_dispatcher(Arc::new(dispatcher));
    /// ```
    pub fn set_daw_dispatcher(&self, dispatcher: BoxedDispatcher) {
        if self.daw_dispatcher.set(dispatcher).is_err() {
            error!("DAW dispatcher already set");
        } else {
            info!("DAW dispatcher registered for in-process handling");
        }
    }

    /// Get the DAW dispatcher if set.
    pub fn daw_dispatcher(&self) -> Option<&BoxedDispatcher> {
        self.daw_dispatcher.get()
    }

    // =========================================================================
    // Lazy Spawning
    // =========================================================================

    /// Register a pending cell (not yet spawned).
    pub fn register_pending_cell(&self, cell_name: String, pending: PendingCell) {
        // Pre-create late-bound handles for all forward targets
        // This ensures they exist before the target cells spawn
        for target in &pending.forward_targets {
            self.get_or_create_late_bound(&target.cell_name);
            debug!(
                cell = %cell_name,
                target = %target.cell_name,
                method_count = target.method_ids.len(),
                "Pre-created late-bound handle for forward target"
            );
        }

        if let Ok(mut cells) = self.pending_cells.lock() {
            debug!(cell = %cell_name, binary = %pending.binary_path.display(), "Registered pending cell");
            cells.insert(cell_name, pending);
        }
    }

    /// Take a pending cell (removes it from pending, for spawning).
    fn take_pending_cell(&self, cell_name: &str) -> Option<PendingCell> {
        if let Ok(mut cells) = self.pending_cells.lock() {
            return cells.remove(cell_name);
        }
        None
    }

    /// Spawn a pending cell and wait for it to be ready.
    pub async fn spawn_pending_cell(&self, cell_name: &str) -> Option<ConnectionHandle> {
        debug!(cell = cell_name, "spawn_pending_cell: taking pending cell");

        // Take the pending cell atomically (prevents race conditions)
        let pending = match self.take_pending_cell(cell_name) {
            Some(p) => p,
            None => {
                debug!(
                    cell = cell_name,
                    "spawn_pending_cell: already spawned by another caller"
                );
                wait_for_cell_ready(cell_name, self.cell_timeout_secs).await;
                return self.get_cell_handle(cell_name);
            }
        };

        // Spawn the cell process
        spawn_cell_process(cell_name, pending, self.is_quiet_mode()).await;

        // Wait for the cell to be ready
        wait_for_cell_ready(cell_name, self.cell_timeout_secs).await;

        self.get_cell_handle(cell_name)
    }
}

// ============================================================================
// CellClient Trait
// ============================================================================

/// Trait for type-safe cell client access.
pub trait CellClient: Sized {
    /// The cell's logical name.
    const CELL_NAME: &'static str;

    /// Create a client from a connection handle.
    fn from_handle(handle: ConnectionHandle) -> Self;
}

/// Macro to implement CellClient for roam-generated clients.
macro_rules! impl_cell_client {
    ($client:ty, $name:literal) => {
        impl CellClient for $client {
            const CELL_NAME: &'static str = $name;

            fn from_handle(handle: ConnectionHandle) -> Self {
                Self::new(handle)
            }
        }
    };
}

// Implement for our cell clients
impl_cell_client!(daw_proto::TransportServiceClient, "daw-standalone");
impl_cell_client!(daw_proto::ProjectServiceClient, "daw-standalone");
impl_cell_client!(session_proto::SessionServiceClient, "session");
impl_cell_client!(gateway_proto::GatewayControlClient, "gateway-ws");

// ============================================================================
// Client Access
// ============================================================================

impl Host {
    /// Get a connection handle for a cell by name, spawning if needed.
    async fn get_or_spawn_cell_handle(&self, cell_name: &'static str) -> Option<ConnectionHandle> {
        // Fast path: cell is already ready
        if cell_ready_registry().is_ready(cell_name) {
            if let Some(handle) = self.get_cell_handle(cell_name) {
                debug!(
                    cell = cell_name,
                    "get_or_spawn_cell_handle: already ready (fast path)"
                );
                return Some(handle);
            }
        }

        debug!(
            cell = cell_name,
            "get_or_spawn_cell_handle: not ready, spawning"
        );

        // Slow path: spawn the cell
        self.spawn_pending_cell(cell_name).await?;

        self.get_cell_handle(cell_name)
    }

    /// Get a typed cell client, spawning if needed (async).
    #[inline(always)]
    pub async fn client_async<C: CellClient>(&self) -> Option<C> {
        let handle = self.get_or_spawn_cell_handle(C::CELL_NAME).await?;
        Some(C::from_handle(handle))
    }
}

// ============================================================================
// Lazy Spawning Helpers
// ============================================================================

/// Spawn a cell process from a PendingCell using dynamic peer creation.
async fn spawn_cell_process(cell_name: &str, pending: PendingCell, quiet_mode: bool) {
    let PendingCell {
        binary_path,
        inherit_stdio,
        forward_targets,
    } = pending;

    // Get driver handle for dynamic peer creation
    let driver_handle = match Host::get().driver_handle() {
        Some(h) => h,
        None => {
            error!(
                cell = cell_name,
                "No driver handle available for lazy spawning"
            );
            return;
        }
    };

    // Create peer dynamically
    let cell_name_for_death = cell_name.to_string();
    let ticket = match driver_handle
        .create_peer(AddPeerOptions {
            peer_name: Some(cell_name.to_string()),
            on_death: Some(Arc::new(move |peer_id| {
                error!(cell = %cell_name_for_death, ?peer_id, "Cell died unexpectedly");
            })),
            diagnostic_state: None,
        })
        .await
    {
        Ok(t) => t,
        Err(e) => {
            error!(cell = cell_name, error = ?e, "Failed to create peer dynamically");
            return;
        }
    };

    let peer_id = ticket.peer_id;
    let args = ticket.to_args();

    debug!(
        cell = cell_name,
        ?peer_id,
        binary = %binary_path.display(),
        "spawn_cell_process: building command"
    );

    // Build the command
    let mut cmd = Command::new(&binary_path);
    for arg in &args {
        cmd.arg(arg);
    }

    // Configure stdio - inherit to see cell output for debugging
    if inherit_stdio || !quiet_mode {
        cmd.stdin(Stdio::inherit())
            .stdout(Stdio::inherit())
            .stderr(Stdio::inherit());
        // Enable passthrough tracing so cells log directly to stderr
        cmd.env("TRACING_PASSTHROUGH", "1");
    } else {
        cmd.stdin(Stdio::null())
            .stdout(Stdio::null())
            .stderr(Stdio::null());
    }

    // Create the host service dispatcher
    let host_service = HostServiceImpl::new(cell_ready_registry().clone());
    let host_service_dispatcher = cell_host_proto::HostServiceDispatcher::new(host_service);

    // Create the tracing service dispatcher
    let tracing_service = Host::get()
        .tracing_state()
        .service_for_peer(peer_id.get() as u64, Some(cell_name.to_string()));
    let tracing_dispatcher = HostTracingDispatcher::new(tracing_service);

    // Create forwarder for target cells (if any)
    // Use MethodRoutedForwarder for explicit method ID routing
    let forwarder = if !forward_targets.is_empty() {
        let mut routed_forwarder = crate::forwarder::MethodRoutedForwarder::new();
        for target in &forward_targets {
            let handle = Host::get().get_or_create_late_bound(&target.cell_name);
            info!(
                cell = cell_name,
                target = %target.cell_name,
                method_count = target.method_ids.len(),
                method_ids = ?target.method_ids,
                "Adding forwarding target with method IDs"
            );
            routed_forwarder.add_target(handle, target.method_ids.clone());
        }
        routed_forwarder
    } else {
        crate::forwarder::MethodRoutedForwarder::new()
    };

    // Compose dispatchers: HostService -> Tracing -> [DAW] -> Forwarder (fallback chain)
    let base_dispatcher = RoutedDispatcher::new(host_service_dispatcher, tracing_dispatcher);

    // Include DAW dispatcher in the chain if set (for in-process REAPER API handling)
    // This allows guest cells to make DAW service calls that are handled locally
    let with_forwarder = RoutedDispatcher::new(base_dispatcher, forwarder);

    // Register peer with driver - use separate match arms to handle type differences
    let add_peer_result = if let Some(daw_dispatcher) = Host::get().daw_dispatcher() {
        debug!(cell = cell_name, "Including DAW dispatcher in chain");
        let dispatcher = RoutedDispatcher::new(
            with_forwarder,
            crate::forwarder::ArcDispatcher::new(daw_dispatcher.clone()),
        );
        driver_handle.add_peer(peer_id, dispatcher).await
    } else {
        driver_handle.add_peer(peer_id, with_forwarder).await
    };

    match add_peer_result {
        Ok((handle, _incoming)) => {
            debug!(
                cell = cell_name,
                ?peer_id,
                "spawn_cell_process: peer registered"
            );
            // Store the handle
            Host::get().register_cell_handle(cell_name.to_string(), handle.clone());
            // Also bind to late-bound handle so other cells can forward to this one
            Host::get().bind_late_bound(cell_name, handle);
        }
        Err(e) => {
            error!(cell = cell_name, ?peer_id, error = ?e, "Failed to register peer");
            return;
        }
    }

    // Spawn the process using ur_taking_me_with_you so it dies when the host dies
    let child = match ur_taking_me_with_you::spawn_dying_with_parent_async(cmd) {
        Ok(c) => {
            debug!(cell = cell_name, pid = ?c.id(), "Cell process spawned (will die with parent)");
            c
        }
        Err(e) => {
            error!(cell = cell_name, error = ?e, "Failed to spawn cell process");
            return;
        }
    };

    // Drop ticket to close doorbell
    drop(ticket);

    // Spawn child monitor task
    let cell_label = cell_name.to_string();
    tokio::spawn(async move {
        let mut child = child;
        match child.wait().await {
            Ok(status) => {
                if !status.success() {
                    error!(cell = %cell_label, %status, "Cell exited with error");
                } else {
                    info!(cell = %cell_label, "Cell exited normally");
                }
            }
            Err(e) => {
                error!(cell = %cell_label, error = ?e, "Cell wait error");
            }
        }
    });
}

// ============================================================================
// In-Process Cell Registration
// ============================================================================

impl Host {
    /// Register an in-process cell (runs within the host process, no spawning).
    ///
    /// This is used for cells that need direct access to host resources (like REAPER APIs)
    /// and cannot run as separate processes. The dispatcher handles incoming RPC calls
    /// to this cell.
    ///
    /// # Arguments
    ///
    /// * `cell_name` - The logical name of the cell (e.g., "daw-reaper")
    /// * `dispatcher` - The service dispatcher for handling incoming requests
    ///
    /// # Returns
    ///
    /// A `ConnectionHandle` for making RPC calls to this cell, or `None` if registration fails.
    ///
    /// # Example
    ///
    /// ```ignore
    /// let transport = ReaperTransport::new();
    /// let project = ReaperProject::new();
    /// let dispatcher = RoutedDispatcher::new(
    ///     TransportServiceDispatcher::new(transport),
    ///     ProjectServiceDispatcher::new(project),
    /// );
    /// let handle = Host::get().register_in_process_cell("daw-reaper", dispatcher).await?;
    /// ```
    pub async fn register_in_process_cell<D>(
        &self,
        cell_name: &str,
        dispatcher: D,
    ) -> Option<ConnectionHandle>
    where
        D: roam::session::ServiceDispatcher + 'static,
    {
        let driver_handle = self.driver_handle()?;

        // Create a peer slot for this in-process cell
        let cell_name_for_death = cell_name.to_string();
        let ticket = match driver_handle
            .create_peer(roam_shm::spawn::AddPeerOptions {
                peer_name: Some(cell_name.to_string()),
                on_death: Some(std::sync::Arc::new(move |peer_id| {
                    error!(cell = %cell_name_for_death, ?peer_id, "In-process cell died unexpectedly");
                })),
                diagnostic_state: None,
            })
            .await
        {
            Ok(t) => t,
            Err(e) => {
                error!(cell = cell_name, error = ?e, "Failed to create peer for in-process cell");
                return None;
            }
        };

        let peer_id = ticket.peer_id;

        // For in-process cells, we don't spawn a process - just register the dispatcher directly
        // Drop the ticket to close the doorbell (no external process will connect)
        drop(ticket);

        // Register the dispatcher with the driver
        match driver_handle.add_peer(peer_id, dispatcher).await {
            Ok((handle, _incoming)) => {
                info!(cell = cell_name, ?peer_id, "In-process cell registered");

                // Store the handle
                self.register_cell_handle(cell_name.to_string(), handle.clone());

                // Bind to late-bound handle so other cells can forward to this one
                self.bind_late_bound(cell_name, handle.clone());

                // Mark the cell as ready
                cell_ready_registry().mark_ready(cell_host_proto::ReadyMsg {
                    cell_name: cell_name.to_string(),
                    peer_id: peer_id.get() as u16,
                    pid: Some(std::process::id()),
                });

                Some(handle)
            }
            Err(e) => {
                error!(cell = cell_name, ?peer_id, error = ?e, "Failed to register in-process cell dispatcher");
                None
            }
        }
    }
}

// ============================================================================
// Cell Startup Helpers
// ============================================================================

/// Wait for a cell to be ready.
async fn wait_for_cell_ready(cell_name: &str, timeout_secs: u64) {
    let timeout = Duration::from_secs(timeout_secs);
    let start = std::time::Instant::now();

    loop {
        if cell_ready_registry().is_ready(cell_name) {
            debug!(
                cell = cell_name,
                elapsed_ms = start.elapsed().as_millis(),
                "Cell is ready"
            );
            return;
        }

        if start.elapsed() >= timeout {
            error!(
                cell = cell_name,
                timeout_secs, "Cell failed to start within timeout"
            );
            return;
        }

        tokio::time::sleep(Duration::from_millis(10)).await;
    }
}
