//! Standalone Desktop App - Full SHM Extension System
//!
//! This demonstrates the complete extension architecture running
//! outside of REAPER. Perfect for testing and development.
//!
//! # Architecture
//!
//! - Uses extension-host-core with SHM transport
//! - Spawns extensions as separate processes (like REAPER)
//! - Discovers extensions from extensions/ directory
//! - Includes HTTP gateway for web/mobile access

use anyhow::Result;
use extension_host_core::ShmHub;
use std::path::PathBuf;
use tokio::time::{sleep, Duration};
use tracing::{info, Level};
use tracing_subscriber;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(Level::INFO)
        .with_target(true)
        .init();

    info!("🚀 FastTrackStudio Standalone - Starting");
    info!("");

    // Determine extension directory
    let exe_dir = std::env::current_exe()?
        .parent()
        .unwrap()
        .to_path_buf();

    let extension_dir = exe_dir.clone(); // Extensions in same dir as binary
    let shm_path = if cfg!(unix) {
        PathBuf::from("/tmp/fts-standalone.shm")
    } else {
        PathBuf::from("fts-standalone.shm")
    };

    info!("Extension directory: {}", extension_dir.display());
    info!("SHM path: {}", shm_path.display());
    info!("");

    // Create SHM hub
    let shm_hub = ShmHub::create(&shm_path)?;
    info!("✅ SHM hub created");
    info!("");

    // TODO: Auto-discover and spawn extensions from extension_dir
    // For now, manually spawn hello-world and daw extensions

    info!("Looking for extensions in: {}", extension_dir.display());

    // Try to spawn hello-world extension
    let hello_world_path = extension_dir.join("hello-world-extension");
    if hello_world_path.exists() {
        info!("Found hello-world extension, spawning...");
        let (peer_id, _child) = shm_hub.spawn_guest(&hello_world_path).await?;
        shm_hub.register_peer(peer_id, Some("hello-world".to_string())).await?;
        info!("✅ hello-world extension loaded (peer {})", peer_id);
    } else {
        info!("⚠️  hello-world extension not found at: {}", hello_world_path.display());
    }

    // Try to spawn daw-reaper extension (if available)
    let daw_path = extension_dir.join("daw-reaper-extension");
    if daw_path.exists() {
        info!("Found daw extension, spawning...");
        let (peer_id, _child) = shm_hub.spawn_guest(&daw_path).await?;
        shm_hub.register_peer(peer_id, Some("daw".to_string())).await?;
        info!("✅ daw extension loaded (peer {})", peer_id);
    } else {
        info!("⚠️  daw extension not found at: {}", daw_path.display());
    }

    // Try to spawn HTTP gateway extension
    let http_gateway_path = extension_dir.join("http-gateway-extension");
    if http_gateway_path.exists() {
        info!("Found http-gateway extension, spawning...");
        let (peer_id, _child) = shm_hub.spawn_guest(&http_gateway_path).await?;
        shm_hub.register_peer(peer_id, Some("http-gateway".to_string())).await?;
        info!("✅ http-gateway extension loaded (peer {})", peer_id);
        info!("🌐 Services available at http://localhost:3000/api/{{service}}/{{method}}");
    } else {
        info!("⚠️  http-gateway extension not found at: {}", http_gateway_path.display());
    }

    info!("");
    info!("✅ Standalone app ready!");
    info!("");
    info!("Press Ctrl+C to exit");
    info!("");

    // Keep running
    loop {
        sleep(Duration::from_secs(1)).await;
    }
}
