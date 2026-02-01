//! Runtime helpers for cells.
//!
//! Provides the `run_cell!` macro that handles all the boilerplate for connecting
//! to the host and signaling readiness.
//!
//! Enable the `cell-debug` feature for verbose startup logging.

pub use cell_host_proto::{HostServiceClient, ReadyAck, ReadyMsg, WaitPolicy};
pub use roam;
pub use roam::Context;
pub use roam::session::diagnostic::{
    DiagnosticState, dump_all_diagnostics, register_diagnostic_state,
};
pub use roam::session::{ConnectionHandle, RoutedDispatcher, ServiceDispatcher};
pub use roam_shm;
pub use roam_shm::driver::{establish_guest, establish_guest_with_diagnostics};
pub use roam_shm::guest::ShmGuest;
pub use roam_shm::spawn::SpawnArgs;
pub use roam_shm::transport::ShmGuestTransport;
pub use roam_tracing;
pub use roam_tracing::{
    CellTracingDispatcher, CellTracingGuard, CellTracingLayer, CellTracingService,
    init_cell_tracing,
};
pub use tokio;
pub use tracing;
pub use tracing_subscriber;
pub use ur_taking_me_with_you;

// ============================================================================
// Debug utilities (inline from dodeca-debug)
// ============================================================================

#[cfg(unix)]
use std::sync::atomic::{AtomicBool, Ordering};

#[cfg(unix)]
static HANDLER_INSTALLED: AtomicBool = AtomicBool::new(false);

#[cfg(unix)]
static DIAGNOSTIC_CALLBACKS: std::sync::RwLock<Vec<Box<dyn Fn() + Send + Sync>>> =
    std::sync::RwLock::new(Vec::new());

/// Register a diagnostic callback to be called on SIGUSR1.
#[cfg(unix)]
pub fn register_diagnostic<F>(callback: F)
where
    F: Fn() + Send + Sync + 'static,
{
    if let Ok(mut callbacks) = DIAGNOSTIC_CALLBACKS.write() {
        callbacks.push(Box::new(callback));
    }
}

#[cfg(not(unix))]
pub fn register_diagnostic<F>(_callback: F)
where
    F: Fn() + Send + Sync + 'static,
{
}

#[cfg(unix)]
static PROCESS_NAME: ProcessName = ProcessName::new();

#[cfg(unix)]
struct ProcessName {
    name: std::sync::atomic::AtomicPtr<&'static str>,
}

#[cfg(unix)]
impl ProcessName {
    const fn new() -> Self {
        Self {
            name: std::sync::atomic::AtomicPtr::new(std::ptr::null_mut()),
        }
    }

    fn store(&self, name: &'static str) {
        let boxed = Box::new(name);
        self.name.store(Box::into_raw(boxed), Ordering::SeqCst);
    }

    fn load(&self) -> &'static str {
        let ptr = self.name.load(Ordering::SeqCst);
        if ptr.is_null() {
            "unknown"
        } else {
            unsafe { *ptr }
        }
    }
}

/// Install a SIGUSR1 handler that dumps diagnostics.
#[cfg(unix)]
pub fn install_sigusr1_handler(process_name: &'static str) {
    if HANDLER_INSTALLED.swap(true, Ordering::SeqCst) {
        return;
    }

    PROCESS_NAME.store(process_name);

    unsafe {
        libc::signal(
            libc::SIGUSR1,
            sigusr1_handler as *const () as libc::sighandler_t,
        );
    }
}

#[cfg(not(unix))]
pub fn install_sigusr1_handler(_process_name: &'static str) {}

#[cfg(unix)]
extern "C" fn sigusr1_handler(_sig: libc::c_int) {
    let process_name = PROCESS_NAME.load();
    let pid = std::process::id();

    eprintln!("[{process_name}] SIGUSR1 (pid={pid})");

    if let Ok(callbacks) = DIAGNOSTIC_CALLBACKS.try_read() {
        for callback in callbacks.iter() {
            callback();
        }
    }
}

// ============================================================================
// Debug print macros
// ============================================================================

/// Debug print macro that only prints when cell-debug feature is enabled
#[macro_export]
#[cfg(feature = "cell-debug")]
macro_rules! cell_debug {
    ($($arg:tt)*) => {
        eprintln!($($arg)*)
    };
}

/// Debug print macro that compiles to nothing when cell-debug feature is disabled
#[macro_export]
#[cfg(not(feature = "cell-debug"))]
macro_rules! cell_debug {
    ($($arg:tt)*) => {};
}

// ============================================================================
// run_cell! macro
// ============================================================================

/// Run a cell with the given name and dispatcher factory.
///
/// The dispatcher factory receives an `Arc<OnceLock<ConnectionHandle>>` that can be used
/// to create clients for calling back to the host. Cells that don't need callbacks
/// can ignore this parameter.
///
/// # Examples
///
/// Cell that doesn't need callbacks:
/// ```ignore
/// use cell_runtime::run_cell;
///
/// fn main() {
///     run_cell!("my-cell", |_handle| {
///         MyServiceDispatcher::new(MyServiceImpl)
///     });
/// }
/// ```
///
/// Cell with callbacks to host:
/// ```ignore
/// use cell_runtime::run_cell;
///
/// fn main() {
///     run_cell!("my-cell", |handle| {
///         let processor = MyProcessorImpl::new(handle);
///         MyProcessorDispatcher::new(processor)
///     });
/// }
/// ```
#[macro_export]
macro_rules! run_cell {
    ($cell_name:expr, |$handle:ident| $make_dispatcher:expr) => {{
        use $crate::tracing_subscriber::prelude::*;
        use $crate::{
            CellTracingDispatcher, ConnectionHandle, DiagnosticState, HostServiceClient, ReadyMsg,
            RoutedDispatcher, ShmGuest, ShmGuestTransport, SpawnArgs, dump_all_diagnostics,
            establish_guest_with_diagnostics, init_cell_tracing, install_sigusr1_handler,
            register_diagnostic, register_diagnostic_state, tokio, tracing, tracing_subscriber,
            ur_taking_me_with_you,
        };

        $crate::cell_debug!(
            "[cell-{}] starting (pid={})",
            $cell_name,
            std::process::id()
        );

        // Install SIGUSR1 handler for diagnostics (must be done early, before async runtime)
        let cell_name_static: &'static str =
            Box::leak(format!("cell-{}", $cell_name).into_boxed_str());
        install_sigusr1_handler(cell_name_static);

        // Register diagnostic callback to dump all connection states
        register_diagnostic(|| {
            let diagnostics = dump_all_diagnostics();
            if !diagnostics.is_empty() {
                eprint!("{}", diagnostics);
            }
        });

        // Ensure this process dies when the parent dies
        ur_taking_me_with_you::die_with_parent();

        $crate::cell_debug!("[cell-{}] die_with_parent completed", $cell_name);

        async fn __run_cell_async() -> Result<(), Box<dyn std::error::Error>> {
            $crate::cell_debug!("[cell] async fn starting");
            let args = SpawnArgs::from_env()?;
            $crate::cell_debug!("[cell] parsed args: peer_id={}", args.peer_id.get());
            let peer_id = args.peer_id;
            let transport = ShmGuestTransport::from_spawn_args(args)?;
            $crate::cell_debug!("[cell] transport created");

            // Initialize cell-side tracing
            // Check TRACING_PASSTHROUGH env var - if set, log to stderr instead of via RPC
            let use_passthrough = std::env::var("TRACING_PASSTHROUGH").is_ok();
            $crate::cell_debug!("[cell] use_passthrough={}", use_passthrough);

            let tracing_guard = if use_passthrough {
                // Passthrough mode: log directly to stderr, respecting RUST_LOG
                use $crate::tracing_subscriber::EnvFilter;
                tracing_subscriber::fmt()
                    .with_writer(std::io::stderr)
                    .with_ansi(false)
                    .with_target(true)
                    .with_env_filter(EnvFilter::from_default_env())
                    .init();

                None
            } else {
                // Normal mode: use roam RPC for tracing
                let (tracing_layer, tracing_guard) = init_cell_tracing(1024);
                tracing_subscriber::registry().with(tracing_layer).init();
                Some(tracing_guard)
            };
            $crate::cell_debug!("[cell] tracing initialized");

            // Let user code create the dispatcher with access to handle
            let handle_cell: std::sync::Arc<std::sync::OnceLock<ConnectionHandle>> =
                std::sync::Arc::new(std::sync::OnceLock::new());

            let $handle = handle_cell.clone();
            $crate::cell_debug!("[cell] creating user dispatcher");
            let user_dispatcher = $make_dispatcher;
            $crate::cell_debug!("[cell] user dispatcher created");

            // Combine user's dispatcher with tracing dispatcher using RoutedDispatcher
            let combined_dispatcher = if let Some(ref guard) = tracing_guard {
                let tracing_dispatcher = CellTracingDispatcher::new(guard.service());
                RoutedDispatcher::new(tracing_dispatcher, user_dispatcher)
            } else {
                // Passthrough mode: create a dummy tracing dispatcher
                let (_, dummy_guard) = init_cell_tracing(1);
                let tracing_dispatcher = CellTracingDispatcher::new(dummy_guard.defuse());
                RoutedDispatcher::new(tracing_dispatcher, user_dispatcher)
            };

            // Create diagnostic state for this connection
            let diagnostic_state =
                std::sync::Arc::new(DiagnosticState::new(format!("cell-{}", $cell_name)));
            register_diagnostic_state(&diagnostic_state);
            $crate::cell_debug!("[cell] diagnostic state registered");

            $crate::cell_debug!("[cell] calling establish_guest_with_diagnostics");
            let (handle, _incoming, driver) = establish_guest_with_diagnostics(
                transport,
                combined_dispatcher,
                Some(diagnostic_state),
            );
            $crate::cell_debug!("[cell] establish_guest_with_diagnostics returned");

            // Store the real handle
            let _ = handle_cell.set(handle.clone());
            $crate::cell_debug!("[cell] handle stored");

            // Spawn driver FIRST - it needs to be running for RPC calls to work
            $crate::cell_debug!("[cell] spawning driver task");
            let driver_handle = tokio::spawn(async move {
                $crate::cell_debug!("[cell] driver task starting");
                if let Err(e) = driver.run().await {
                    eprintln!("Driver error: {:?}", e);
                    std::process::exit(1);
                }
                $crate::cell_debug!("[cell] driver task exited cleanly");
            });
            $crate::cell_debug!("[cell] driver task spawned");

            // Now start tracing service - this queries host config via RPC
            if let Some(guard) = tracing_guard {
                guard.start(handle.clone()).await;
                $crate::cell_debug!("[cell] tracing started (queried host config)");
            }

            // Signal readiness to host
            let host = HostServiceClient::new(handle);
            $crate::cell_debug!("[cell] calling host.ready()");
            host.ready(ReadyMsg {
                peer_id: peer_id.get() as u16,
                cell_name: $cell_name.to_string(),
                pid: Some(std::process::id()),
            })
            .await?;
            $crate::cell_debug!("[cell] host.ready() returned successfully");

            // Wait for driver to complete (it runs until connection closes)
            if let Err(e) = driver_handle.await {
                eprintln!("[cell] driver task panicked: {e:?}");
            }
            Ok(())
        }

        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime")
            .block_on(__run_cell_async())
    }};
}
