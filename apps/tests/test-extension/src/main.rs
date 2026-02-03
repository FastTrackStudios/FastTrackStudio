//! Test Extension - Host that loads daw-standalone for integration testing
//!
//! This binary uses the shared host-runtime infrastructure with daw-standalone
//! as the DAW implementation, allowing full integration testing without REAPER.

#![deny(unsafe_code)]

use std::path::PathBuf;

use host_runtime::{
    init_shm_infrastructure, init_tracing, spawn_tracing_consumer, CellConfig, Host,
};
use tracing::{info, warn};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    init_tracing();

    info!("Test Extension starting (using daw-standalone)...");

    // Initialize SHM infrastructure (keep temp_dir alive)
    let _temp_dir = init_shm_infrastructure().await?;

    // Start cell tracing consumer
    spawn_tracing_consumer();

    // Find cell binary directory
    let cell_dir = host_runtime::default_cell_dir();

    // Register cells for lazy spawning - TEST CONFIGURATION
    register_cells(&cell_dir);

    // Start Unix socket server for desktop app connections
    start_unix_server();

    // Spawn all cells
    spawn_cells().await?;

    info!(
        "Test Extension running with DAW, Session, and Gateway-WS cells. Press Ctrl+C to shutdown."
    );

    // Keep running until Ctrl+C
    tokio::signal::ctrl_c().await?;
    info!("Shutting down test extension...");
    Host::get().signal_exit();

    Ok(())
}

/// Register all cells with the Host for lazy spawning.
/// Uses daw-standalone for testing (not daw-reaper).
fn register_cells(cell_dir: &PathBuf) {
    // DAW cell - using standalone implementation for testing
    CellConfig::new("daw-standalone", cell_dir).register();

    // Session cell - forwards to DAW for all DAW services
    // The session needs access to markers, regions, and tempo map to build songs
    CellConfig::new("session", cell_dir)
        .forwards_to_with_methods("daw-standalone", || {
            daw_proto::TransportServiceDispatcher::<()>::method_ids()
                .into_iter()
                .chain(daw_proto::ProjectServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::MarkerServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::RegionServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::TempoMapServiceDispatcher::<()>::method_ids())
                .collect()
        })
        .register();

    // Gateway WebSocket cell - forwards to both DAW and Session
    // Routes method IDs to the correct cell based on which service handles them
    CellConfig::new("gateway-ws", cell_dir)
        .forwards_to_with_methods("daw-standalone", || {
            // DAW services: Transport, Project, Markers, Regions, TempoMap
            daw_proto::TransportServiceDispatcher::<()>::method_ids()
                .into_iter()
                .chain(daw_proto::ProjectServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::MarkerServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::RegionServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::TempoMapServiceDispatcher::<()>::method_ids())
                .collect()
        })
        .forwards_to_with_methods("session", || {
            // Session services: Setlist, Song, Session, DefinesActions
            session_proto::SetlistServiceDispatcher::<()>::method_ids()
                .into_iter()
                .chain(session_proto::SongServiceDispatcher::<()>::method_ids())
                .chain(session_proto::SessionServiceDispatcher::<()>::method_ids())
                .chain(session_proto::DefinesActionsDispatcher::<()>::method_ids())
                .collect()
        })
        .register();

    info!("Cells registered for lazy spawning (test configuration)");
}

/// Start the Unix socket server for desktop app connections.
fn start_unix_server() {
    let socket_path = std::env::var("FTS_SOCKET")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/tmp/fts-control.sock"));

    tokio::spawn(async move {
        if let Err(e) = host_runtime::unix_server::start_server(&socket_path).await {
            warn!("Unix socket server error: {}", e);
        }
    });
}

/// Spawn all registered cells.
async fn spawn_cells() -> Result<(), Box<dyn std::error::Error>> {
    // DAW cell
    info!("Spawning DAW cell...");
    Host::get()
        .client_async::<daw_proto::TransportServiceClient>()
        .await
        .ok_or("Failed to spawn DAW cell")?;
    info!("DAW cell ready");

    // Session cell
    info!("Spawning Session cell...");
    Host::get()
        .client_async::<session_proto::SessionServiceClient>()
        .await
        .ok_or("Failed to spawn Session cell")?;
    info!("Session cell ready");

    // Gateway WebSocket cell
    info!("Spawning Gateway-WS cell...");
    Host::get()
        .client_async::<gateway_proto::GatewayControlClient>()
        .await
        .ok_or("Failed to spawn Gateway-WS cell")?;
    info!("Gateway-WS cell ready");

    Ok(())
}
