//! DAW Host - Orchestrates DAW cells and routes calls between them
//!
//! This host uses the Host singleton pattern (like dodeca):
//! 1. Initializes the Host singleton with SHM configuration
//! 2. Registers cells for lazy spawning
//! 3. Accesses cells via typed clients (triggers spawn on first access)
//! 4. Uses LateBoundForwarder to route session's RPC calls to DAW

#![deny(unsafe_code)]

mod cells;
mod host;
mod multi_forwarder;
mod tracing;
mod unix_server;

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use ::tracing::{info, warn};
use gateway_proto::GatewayControlClient;
use session_proto::SessionServiceClient;
use tokio::time::sleep;

use crate::host::{CellConfig, Host, default_cell_dir, init_shm_infrastructure};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing::init_tracing();

    info!("DAW Host starting...");

    // Initialize SHM infrastructure (keep temp_dir alive)
    let _temp_dir = init_shm_infrastructure().await?;

    // Start cell tracing consumer
    tracing::spawn_tracing_consumer();

    // Find cell binary directory
    let cell_dir = default_cell_dir();

    // Register cells for lazy spawning
    register_cells(&cell_dir);

    // Start Unix socket server for desktop app connections
    start_unix_server();

    // Spawn all cells
    spawn_cells().await?;

    info!("Host running with DAW, Session, and Gateway-WS cells. Press Ctrl+C to shutdown.");

    // Start health check loop
    start_health_check();

    // Keep running until Ctrl+C
    tokio::signal::ctrl_c().await?;
    info!("Shutting down host...");
    Host::get().signal_exit();

    Ok(())
}

/// Register all cells with the Host for lazy spawning.
fn register_cells(cell_dir: &PathBuf) {
    // DAW cell - standalone, no forwarding needed
    CellConfig::new("daw-standalone", cell_dir).register();

    // Session cell - forwards to DAW
    CellConfig::new("session", cell_dir)
        .forwards_to(&["daw-standalone"])
        .register();

    // Gateway WebSocket cell - forwards to DAW
    CellConfig::new("gateway-ws", cell_dir)
        .forwards_to(&["daw-standalone"])
        .register();

    info!("Cells registered for lazy spawning");
}

/// Start the Unix socket server for desktop app connections.
fn start_unix_server() {
    let socket_path = std::env::var("FTS_SOCKET")
        .map(PathBuf::from)
        .unwrap_or_else(|_| PathBuf::from("/tmp/fts-control.sock"));

    tokio::spawn(async move {
        if let Err(e) = unix_server::start_server(&socket_path).await {
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
        .client_async::<SessionServiceClient>()
        .await
        .ok_or("Failed to spawn Session cell")?;
    info!("Session cell ready");

    // Gateway WebSocket cell
    info!("Spawning Gateway-WS cell...");
    Host::get()
        .client_async::<GatewayControlClient>()
        .await
        .ok_or("Failed to spawn Gateway-WS cell")?;
    info!("Gateway-WS cell ready");

    Ok(())
}

/// Start a background health check loop for the session cell.
fn start_health_check() {
    tokio::spawn(async move {
        // Get session client for health checks
        let session_client = match Host::get().client_async::<SessionServiceClient>().await {
            Some(c) => Arc::new(c),
            None => {
                warn!("Could not get session client for health checks");
                return;
            }
        };

        loop {
            match session_client.get_status().await {
                Ok(status) => info!("Session health check: {}", status),
                Err(e) => warn!("Session health check failed: {}", e),
            }
            sleep(Duration::from_secs(5)).await;
        }
    });
}
