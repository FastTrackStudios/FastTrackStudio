//! Tracing configuration for the DAW host.
//!
//! This module provides unified tracing initialization including:
//! - `tracing_subscriber` configuration with env filter
//! - Cell tracing consumer that dispatches records from cells to the host
//!
//! # Example
//!
//! ```ignore
//! use crate::tracing::{init_tracing, spawn_tracing_consumer};
//!
//! init_tracing();
//! spawn_tracing_consumer();
//! ```

use tracing::info;

use crate::host::Host;

/// Initialize the tracing subscriber with a sensible default configuration.
///
/// Uses `RUST_LOG` environment variable for filtering, with INFO as the default level.
/// The `roam_telemetry` crate is set to INFO level for diagnostic output.
pub fn init_tracing() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into())
                .add_directive("roam_telemetry=info".parse().unwrap()),
        )
        .init();
}

/// Spawn the tracing consumer task that receives and dispatches cell tracing records.
///
/// This should be called after `Host::get()` is initialized but before cells are spawned.
/// The consumer runs in the background and dispatches records from cells to the host's
/// tracing infrastructure.
pub fn spawn_tracing_consumer() {
    if let Some(mut tracing_rx) = Host::get().take_tracing_receiver() {
        tokio::spawn(async move {
            info!("Tracing consumer started");
            while let Some(tagged) = tracing_rx.recv().await {
                roam_tracing::dispatch_record(&tagged);
            }
            info!("Tracing consumer stopped");
        });
    }
}
