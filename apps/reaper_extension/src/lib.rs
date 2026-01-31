//! FastTrackStudio - REAPER Extension (Minimal)
//!
//! Minimal implementation using daw-reaper's Transport service.

use daw_proto::TransportDispatcher;
use daw_reaper::ReaperTransport;
use extension_runtime::run_extension;
use reaper_low::PluginContext;
use reaper_macros::reaper_extension_plugin;
use tracing::info;

/// Plugin entry point
#[reaper_extension_plugin]
fn plugin_main(_context: PluginContext) -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    tracing_subscriber::fmt().with_env_filter("info").init();

    info!("FastTrackStudio extension initializing...");

    // Run the extension with the transport service
    // The extension_runtime handles control surface integration and main thread dispatch
    run_extension!("reaper-extension", |_handle| {
        TransportDispatcher::new(ReaperTransport::new())
    });

    info!("FastTrackStudio extension initialized");
    Ok(())
}
