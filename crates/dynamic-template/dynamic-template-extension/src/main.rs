//! Dynamic Template domain SHM guest process.
//!
//! Connects to REAPER via daw-bridge SHM and manages dynamic template state:
//! auto-color classification, visibility manager, and template sorting.
//!
//! Registers dynamic-template-domain actions with REAPER and handles their
//! execution locally when triggered. The host (daw-bridge) is domain-agnostic.
//!
//! Placed in `UserPlugins/fts-extensions/` and hot-reloaded by daw-bridge.

use daw_extension_runtime::GuestOptions;
use dynamic_template_proto::{
    actions::dynamic_template_actions,
    auto_color::actions::auto_color_actions,
    visibility_manager::actions::visibility_manager_actions,
};
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
    info!("[dynamic-template:{pid}] Dynamic Template extension starting");

    let daw = daw_extension_runtime::connect(GuestOptions {
        role: "dynamic-template",
        ..Default::default()
    })
    .await?;

    info!("[dynamic-template:{pid}] Connected to REAPER via SHM");

    // Signal that we're alive — tests read this to verify the extension connected
    daw.ext_state()
        .set("FTS_DYNAMIC_TEMPLATE_EXT", "status", "ready", false)
        .await?;
    daw.ext_state()
        .set("FTS_DYNAMIC_TEMPLATE_EXT", "pid", &pid.to_string(), false)
        .await?;
    info!("[dynamic-template:{pid}] Health beacon written");

    // Register dynamic-template-domain actions with REAPER.
    // Action definitions live in dynamic-template-proto — single source of truth.
    let registry = daw.action_registry();

    // Core dynamic-template actions (sort selected, sort all, import & sort, etc.)
    for def in dynamic_template_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.description).await?;
        if cmd_id == 0 {
            tracing::warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            info!("[dynamic-template:{pid}] Registered {cmd_name} (cmd_id={cmd_id})");
        }
    }

    // Auto-color actions (color all, color selected, toggle, clear)
    for def in auto_color_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.description).await?;
        if cmd_id == 0 {
            tracing::warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            info!("[dynamic-template:{pid}] Registered {cmd_name} (cmd_id={cmd_id})");
        }
    }

    // Visibility manager actions (per-group toggles, show/hide all, rebuild cache)
    for def in visibility_manager_actions::definitions() {
        let cmd_name = def.id.to_command_id();
        let cmd_id = registry.register(&cmd_name, &def.description).await?;
        if cmd_id == 0 {
            tracing::warn!("[dynamic-template:{pid}] Failed to register action: {cmd_name}");
        } else {
            info!("[dynamic-template:{pid}] Registered {cmd_name} (cmd_id={cmd_id})");
        }
    }
    info!("[dynamic-template:{pid}] All dynamic-template actions registered");

    // Subscribe to action trigger events and handle them locally.
    let mut action_rx = registry.subscribe_actions().await?;
    info!("[dynamic-template:{pid}] Subscribed to action events");

    // Subscribe to track events for auto-color (re-classify when tracks change).
    let project = daw.current_project().await?;
    let mut track_rx = project.tracks().subscribe().await?;
    info!("[dynamic-template:{pid}] Subscribed to track events");

    // Event loop — handle action triggers and track changes from REAPER
    loop {
        tokio::select! {
            result = action_rx.recv() => {
                match result {
                    Ok(Some(event)) => {
                        match &*event {
                            daw::service::ActionEvent::Triggered { command_name } => {
                                handle_action(command_name);
                            }
                        }
                    }
                    Ok(None) | Err(_) => {
                        info!("[dynamic-template:{pid}] Action event stream ended");
                        break;
                    }
                }
            }
            result = track_rx.recv() => {
                match result {
                    Ok(Some(event)) => {
                        handle_track_event(&*event);
                    }
                    Ok(None) | Err(_) => {
                        info!("[dynamic-template:{pid}] Track event stream ended");
                        break;
                    }
                }
            }
        }
    }

    Ok(())
}

fn handle_action(command_name: &str) {
    // TODO: Wire to DynamicTemplate controller once it lives in this process.
    // TODO: Dispatch auto-color actions to auto_color engine
    // TODO: Dispatch visibility manager actions to visibility controller
    // TODO: Dispatch sort/organize actions to dynamic-template organizer
    // For now, log the trigger so we can verify the end-to-end flow.
    info!("[dynamic-template] Action triggered: {command_name}");
}

fn handle_track_event(event: &daw::service::TrackEvent) {
    // TODO: When tracks are added/renamed/removed, re-classify and apply auto-color.
    // This should debounce rapid changes (e.g. import of many tracks) and only
    // re-color once the batch settles.
    info!("[dynamic-template] Track event: {event:?}");
}
