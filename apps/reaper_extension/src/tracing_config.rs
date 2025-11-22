//! Tracing setup and utilities for FastTrackStudio REAPER Extension
//!
//! Provides centralized structured logging configuration.
//!
//! Logging can be controlled via:
//! 1. RUST_LOG environment variable (e.g., `RUST_LOG=reaper_extension=debug`)
//! 2. Defaults to warn level for everything
//! 3. Local socket IPC logs are shown at info level for visibility

use tracing_subscriber::{
    fmt,
    EnvFilter,
    layer::SubscriberExt,
    util::SubscriberInitExt,
};

static TRACING_INITIALIZED: std::sync::OnceLock<()> = std::sync::OnceLock::new();

/// Initialize tracing with feature-based filtering
///
/// This should be called early in the extension lifecycle, before any modules initialize.
/// The filter respects RUST_LOG environment variable.
///
/// Example RUST_LOG values:
/// - `reaper_extension=debug` - debug level for extension
/// - `reaper_extension::action_registry=trace` - trace for action registry only
pub fn init_tracing() {
    TRACING_INITIALIZED.get_or_init(|| {
        // Start with RUST_LOG if set, otherwise use defaults
        let filter = if std::env::var("RUST_LOG").is_ok() {
            // User specified RUST_LOG - use it directly
            EnvFilter::from_default_env()
        } else {
            // Default to warn level for everything
            // Specifically allow info logs for IPC/RPC for visibility
            EnvFilter::new("warn")
                .add_directive("reaper_extension=warn".parse().unwrap())
                .add_directive("reaper_extension::local_socket_server=info".parse().unwrap())
                .add_directive("reaper_extension::reaper_rpc_server=info".parse().unwrap())
                .add_directive("peer_2_peer::unix_socket=info".parse().unwrap())
                .add_directive("peer_2_peer::iroh_connection=info".parse().unwrap())
                // Suppress harmless IROH warnings about IPv6 relays and discovery pongs
                .add_directive("iroh::magicsock=error".parse().unwrap())
                .add_directive("iroh_quinn_udp=error".parse().unwrap())
                .add_directive("iroh::protocol::router=error".parse().unwrap())
        };
        
        // Initialize the subscriber with formatted output
        tracing_subscriber::registry()
            .with(fmt::layer().with_target(true)) // Show module paths for clarity
            .with(filter)
            .init();
    });
}

