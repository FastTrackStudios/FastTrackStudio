//! Tracing setup and utilities for FastTrackStudio REAPER Extension
//!
//! Provides centralized structured logging configuration.
//!
//! Logging can be controlled via:
//! 1. RUST_LOG environment variable (e.g., `RUST_LOG=reaper_extension=debug`)
//! 2. Defaults to warn level with extension at info level

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
            // Default to warn level, allow debug for extension
            EnvFilter::new("warn")
                .add_directive("reaper_extension=debug".parse().unwrap())
        };
        
        // Initialize the subscriber with formatted output
        tracing_subscriber::registry()
            .with(fmt::layer().with_target(true)) // Show module paths for clarity
            .with(filter)
            .init();
    });
}

