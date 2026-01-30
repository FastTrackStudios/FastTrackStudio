//! Runtime helpers for FastTrackStudio extensions.
//!
//! Provides the `run_extension!` macro that handles all the boilerplate for connecting
//! to the host and signaling readiness.
//!
//! Enable the `extension-debug` feature for verbose startup logging.
//!
//! Based on dodeca's cell runtime architecture.

pub use host_proto::{HostServiceClient, ReadyMsg};
pub use roam;
pub use roam::Context;
pub use roam::session::{ConnectionHandle, RoutedDispatcher, ServiceDispatcher, NoDispatcher};
pub use roam_shm;
pub use roam_shm::driver::establish_guest;
pub use roam_shm::spawn::SpawnArgs;
pub use roam_shm::transport::ShmGuestTransport;
pub use roam_tracing::{
    CellTracingDispatcher, CellTracingGuard, CellTracingLayer, CellTracingService,
    init_cell_tracing,
};
pub use tokio;
pub use tracing;
pub use tracing_subscriber;

/// Debug print macro that only prints when extension-debug feature is enabled
#[macro_export]
#[cfg(feature = "extension-debug")]
macro_rules! extension_debug {
    ($($arg:tt)*) => {
        eprintln!($($arg)*)
    };
}

/// Debug print macro that compiles to nothing when extension-debug feature is disabled
#[macro_export]
#[cfg(not(feature = "extension-debug"))]
macro_rules! extension_debug {
    ($($arg:tt)*) => {};
}

/// Run an extension with the given name and dispatcher factory.
///
/// The dispatcher factory receives an `Arc<OnceLock<ConnectionHandle>>` that can be used
/// to create clients for calling back to the host. Extensions that don't need callbacks
/// can ignore this parameter.
///
/// # Examples
///
/// Extension that doesn't need callbacks:
/// ```ignore
/// use extension_runtime::run_extension;
/// use my_extension_proto::{MyServiceDispatcher, MyServiceImpl};
///
/// fn main() {
///     run_extension!("my-extension", |_handle| {
///         MyServiceDispatcher::new(MyServiceImpl)
///     });
/// }
/// ```
///
/// Extension with callbacks to host:
/// ```ignore
/// use extension_runtime::run_extension;
///
/// fn main() {
///     run_extension!("my-extension", |handle| {
///         let service = MyServiceImpl::new(handle);
///         MyServiceDispatcher::new(service)
///     });
/// }
/// ```
#[macro_export]
macro_rules! run_extension {
    ($extension_name:expr, |$handle:ident| $make_dispatcher:expr) => {{
        use $crate::tracing_subscriber::prelude::*;
        use $crate::{
            CellTracingDispatcher, ConnectionHandle, HostServiceClient, ReadyMsg,
            RoutedDispatcher, ShmGuestTransport, SpawnArgs, establish_guest, init_cell_tracing,
            tokio, tracing, tracing_subscriber,
        };

        $crate::extension_debug!(
            "[extension-{}] starting (pid={})",
            $extension_name,
            std::process::id()
        );

        // CRITICAL: Set up parent death detection FIRST (Unix only)
        // Ensures this extension process dies if REAPER crashes/is killed
        #[cfg(unix)]
        {
            $crate::extension_debug!("[extension-{}] setting up parent death detection", $extension_name);
            $crate::roam_shm::spawn::die_with_parent();
            $crate::extension_debug!("[extension-{}] die_with_parent completed", $extension_name);
        }

        async fn __run_extension_async() -> Result<(), Box<dyn std::error::Error>> {
            $crate::extension_debug!("[extension] async fn starting");

            // Parse spawn args from environment
            let args = SpawnArgs::from_env()?;
            $crate::extension_debug!("[extension] parsed args: peer_id={}", args.peer_id.get());
            let peer_id = args.peer_id;
            let transport = ShmGuestTransport::from_spawn_args(args)?;
            $crate::extension_debug!("[extension] transport created");

            // Initialize extension-side tracing
            // Check TRACING_PASSTHROUGH env var - if set, log to stderr instead of via RPC
            let use_passthrough = std::env::var("TRACING_PASSTHROUGH").is_ok();
            $crate::extension_debug!("[extension] use_passthrough={}", use_passthrough);

            let tracing_guard = if use_passthrough {
                // Passthrough mode: log directly to stderr, respecting RUST_LOG
                use $crate::tracing_subscriber::EnvFilter;
                tracing_subscriber::fmt()
                    .with_writer(std::io::stderr)
                    .with_ansi(false)
                    .with_target(true)
                    .with_env_filter(EnvFilter::from_default_env())
                    .init();

                // No tracing guard needed in passthrough mode
                None
            } else {
                // Normal mode: use roam RPC for tracing
                let (tracing_layer, tracing_guard) = init_cell_tracing(1024);
                tracing_subscriber::registry().with(tracing_layer).init();
                Some(tracing_guard)
            };
            $crate::extension_debug!("[extension] tracing initialized");

            // Let user code create the dispatcher with access to handle
            // We use an Arc<OnceLock> pattern so handle can be set later
            let handle_cell: std::sync::Arc<std::sync::OnceLock<ConnectionHandle>> =
                std::sync::Arc::new(std::sync::OnceLock::new());

            let $handle = handle_cell.clone();
            $crate::extension_debug!("[extension] creating user dispatcher");
            let user_dispatcher = $make_dispatcher;
            $crate::extension_debug!("[extension] user dispatcher created");

            // Combine user's dispatcher with tracing dispatcher using RoutedDispatcher
            // IMPORTANT: RoutedDispatcher routes primary.method_ids() to primary, rest to fallback.
            // Tracing must be PRIMARY so tracing RPC calls go to tracing dispatcher.
            let combined_dispatcher = if let Some(ref guard) = tracing_guard {
                let tracing_dispatcher = CellTracingDispatcher::new(guard.service());
                RoutedDispatcher::new(
                    tracing_dispatcher, // primary: handles tracing methods
                    user_dispatcher,    // fallback: handles all extension-specific methods
                )
            } else {
                // Passthrough mode: no tracing dispatcher needed, but we need same type
                // Create a dummy service just for the dispatcher (it won't receive calls)
                let (_, dummy_guard) = init_cell_tracing(1);
                let tracing_dispatcher = CellTracingDispatcher::new(dummy_guard.defuse());
                RoutedDispatcher::new(tracing_dispatcher, user_dispatcher)
            };

            $crate::extension_debug!("[extension] calling establish_guest");
            let (handle, _incoming, driver) = establish_guest(transport, combined_dispatcher);
            $crate::extension_debug!("[extension] establish_guest returned");

            // Store the real handle
            let _ = handle_cell.set(handle.clone());
            $crate::extension_debug!("[extension] handle stored");

            // CRITICAL: Spawn driver FIRST - it needs to be running for RPC calls to work
            $crate::extension_debug!("[extension] spawning driver task");
            let driver_handle = tokio::spawn(async move {
                $crate::extension_debug!("[extension] driver task starting");
                if let Err(e) = driver.run().await {
                    eprintln!("Driver error: {:?}", e);
                    std::process::exit(1);
                }
                $crate::extension_debug!("[extension] driver task exited cleanly");
            });
            $crate::extension_debug!("[extension] driver task spawned");

            // Now start tracing service - this may query host config via RPC
            // (driver must be running for this to work)
            if let Some(guard) = tracing_guard {
                guard.start(handle.clone()).await;
                $crate::extension_debug!("[extension] tracing started (queried host config)");
            }

            // Signal readiness to host
            let host = HostServiceClient::new(handle);
            $crate::extension_debug!("[extension] calling host.ready()");
            host.ready(ReadyMsg {
                extension_name: $extension_name.to_string(),
                peer_id: peer_id.get(),
            })
            .await?;
            $crate::extension_debug!("[extension] host.ready() returned successfully");

            tracing::info!("✅ {} Ready!", $extension_name);

            // Wait for driver to complete (it runs until connection closes)
            if let Err(e) = driver_handle.await {
                eprintln!("[extension] driver task panicked: {e:?}");
            }
            Ok(())
        }

        tokio::runtime::Builder::new_current_thread()
            .enable_all()
            .build()
            .expect("Failed to create tokio runtime")
            .block_on(__run_extension_async())
    }};
}
