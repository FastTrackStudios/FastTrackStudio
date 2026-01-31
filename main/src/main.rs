//! DAW Host - Orchestrates DAW cells and routes calls between them
//!
//! This host:
//! 1. Creates an SHM segment for communication
//! 2. Spawns the DAW standalone cell
//! 3. Routes calls between cells
//! 4. Demonstrates calling the DAW cell's Transport service

#![deny(unsafe_code)]

use roam_shm::{ShmHost, SegmentConfig, spawn::AddPeerOptions};
use std::path::PathBuf;
use std::process::Command;
use std::time::Duration;
use tokio::time::sleep;
use tracing::{info, warn};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    info!("DAW Host starting...");

    // Create temp directory for SHM segment
    let temp_dir = tempfile::tempdir()?;
    let shm_path = temp_dir.path().join("daw-hub.shm");

    // Create SHM host
    let config = SegmentConfig::default();
    let mut host = ShmHost::create(&shm_path, config)?;
    info!("SHM host created at: {}", shm_path.display());

    // Spawn the DAW standalone cell
    let daw_ticket = host.add_peer(AddPeerOptions {
        peer_name: Some("daw-standalone".to_string()),
        on_death: Some(std::sync::Arc::new(|peer_id| {
            warn!("DAW cell {:?} died!", peer_id);
        })),
        ..Default::default()
    })?;

    let daw_peer_id = daw_ticket.peer_id;
    info!("Spawning DAW standalone cell with peer_id: {:?}", daw_peer_id);

    // Build path to daw-standalone binary
    let daw_binary = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .unwrap()
        .join("target/debug/daw-standalone");

    // Spawn the cell process
    let _child = daw_ticket.spawn(Command::new(&daw_binary))?;
    info!("DAW standalone cell spawned");

    // Give the cell time to start up
    sleep(Duration::from_millis(100)).await;

    // TODO: Establish connection and demonstrate calling the Transport service
    // This requires:
    // 1. Creating a dispatcher for the host
    // 2. Establishing guest connection
    // 3. Getting a ConnectionHandle to the DAW cell
    // 4. Creating a TransportClient and calling methods

    info!("Host running. Press Ctrl+C to shutdown.");

    // Keep running
    tokio::signal::ctrl_c().await?;
    info!("Shutting down host...");

    Ok(())
}