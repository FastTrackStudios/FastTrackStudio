//! Keyflow domain SHM guest process.
//!
//! Connects to REAPER via daw-bridge SHM and manages keyflow state:
//! chart parsing, guide track generation, and MIDI export.
//!
//! Subscribes to action trigger events from REAPER and handles them locally.
//! The host (daw-bridge) is domain-agnostic.
//!
//! Placed in `UserPlugins/fts-extensions/` and hot-reloaded by daw-bridge.

use daw_extension_runtime::GuestOptions;
use eyre::Result;
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
    info!("[keyflow:{pid}] Keyflow extension starting");

    let daw = daw_extension_runtime::connect(GuestOptions {
        role: "keyflow",
        ..Default::default()
    })
    .await?;

    info!("[keyflow:{pid}] Connected to REAPER via SHM");

    // Signal that we're alive — tests read this to verify the extension connected
    daw.ext_state()
        .set("FTS_KEYFLOW_EXT", "status", "ready", false)
        .await?;
    daw.ext_state()
        .set("FTS_KEYFLOW_EXT", "pid", &pid.to_string(), false)
        .await?;
    info!("[keyflow:{pid}] Health beacon written");

    // Subscribe to action trigger events.
    // Unlike signal-extension, keyflow doesn't register its own actions yet —
    // guide_track and keyflow_actions will be wired in later.
    let registry = daw.action_registry();
    let mut rx = registry.subscribe_actions().await?;
    info!("[keyflow:{pid}] Subscribed to action events");

    // TODO: Watch for project/track changes and sync chart state
    // TODO: Initialize GuideGenerator from keyflow::midi::guide

    // Event loop — handle action triggers from REAPER
    while let Ok(Some(event)) = rx.recv().await {
        match &*event {
            daw::service::ActionEvent::Triggered { command_name } => {
                handle_action(command_name);
            }
        }
    }

    info!("[keyflow:{pid}] Action event stream ended");
    Ok(())
}

fn handle_action(command_name: &str) {
    // TODO: Wire to guide track generation (keyflow::midi::guide::GuideGenerator)
    // TODO: Wire to MIDI export pipeline
    // TODO: Handle keyflow-specific actions once action definitions are added
    info!("[keyflow] Action triggered: {command_name}");
}
