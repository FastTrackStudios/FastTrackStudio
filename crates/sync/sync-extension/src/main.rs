//! Sync domain SHM guest process.
//!
//! Connects to REAPER via daw-bridge SHM and manages sync state:
//! Ableton Link, tempo mapping, time signature changes, ruler lanes,
//! and sync markers.
//!
//! Registers sync-domain actions with REAPER's action system and runs
//! the Link engine tick loop using daw RPC services.
//!
//! Placed in `UserPlugins/fts-extensions/` and hot-reloaded by daw-bridge.

mod link_bridge;

use daw_extension_runtime::GuestOptions;
use eyre::Result;
use sync::actions::sync_actions;
use tracing::info;

fn main() -> Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env().unwrap_or_else(|_| "info".into()),
        )
        .init();

    let rt = tokio::runtime::Builder::new_current_thread()
        .enable_all()
        .build()?;

    rt.block_on(run())
}

async fn run() -> Result<()> {
    let pid = std::process::id();
    info!("[sync:{pid}] Sync extension starting");

    let daw = daw_extension_runtime::connect(GuestOptions {
        role: "sync",
        ..Default::default()
    })
    .await?;

    info!("[sync:{pid}] Connected to REAPER via SHM");

    // Signal that we're alive — tests read this to verify the extension connected
    daw.ext_state()
        .set("FTS_SYNC_EXT", "status", "ready", false)
        .await?;
    daw.ext_state()
        .set("FTS_SYNC_EXT", "pid", &pid.to_string(), false)
        .await?;
    info!("[sync:{pid}] Health beacon written");

    // Register sync-domain actions with REAPER.
    // Action definitions live in sync-proto — single source of truth.
    let registry = daw.action_registry();
    for def in sync_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.description).await?;
        if cmd_id == 0 {
            tracing::warn!("[sync:{pid}] Failed to register action: {cmd_name}");
        } else {
            info!("[sync:{pid}] Registered {cmd_name} (cmd_id={cmd_id})");
        }
    }
    info!("[sync:{pid}] All sync actions registered");

    // Run the Link engine — this loop runs forever at ~30Hz
    link_bridge::run_link_engine(&daw).await
}
