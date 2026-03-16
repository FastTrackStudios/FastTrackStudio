//! Signal system bootstrap for the REAPER extension.
//!
//! Creates a [`SignalController`] with a file-backed SQLite database,
//! wires the [`ReaperPatchApplier`] and [`RigSceneManager`], and stores
//! the controller globally so action handlers and RPC services can access it.

use crate::routed_handler::RoutedHandler;
use signal::{DawPatchApplier, RigSceneApplier, SignalController};
use std::sync::{Arc, OnceLock};
use tracing::{info, warn};

/// Global signal controller instance — shared by action handlers and RPC.
static SIGNAL_CONTROLLER: OnceLock<SignalController> = OnceLock::new();

/// Get the global signal controller, if initialized.
pub fn controller() -> Option<&'static SignalController> {
    SIGNAL_CONTROLLER.get()
}

/// Initialize the signal subsystem.
///
/// Creates a file-backed SQLite database at the standard library path,
/// seeds default data on first run, and stores the controller globally.
/// Appliers (ReaperPatchApplier, RigSceneManager) are attached later
/// when a DAW role is detected.
pub async fn init() {
    let home = std::env::var("HOME").unwrap_or_else(|_| "/tmp".to_string());
    let db_dir = format!("{}/Music/FastTrackStudio/Library", home);

    // Ensure the directory exists
    if let Err(e) = std::fs::create_dir_all(&db_dir) {
        warn!("Failed to create signal library directory {}: {}", db_dir, e);
        return;
    }

    let db_path = format!("{}/signal.db", db_dir);
    info!("Initializing signal controller with DB: {}", db_path);

    match signal::connect_db_seeded(&db_path).await {
        Ok(ctrl) => {
            if SIGNAL_CONTROLLER.set(ctrl).is_err() {
                warn!("Signal controller already initialized");
            } else {
                info!("Signal controller initialized successfully");
            }
        }
        Err(e) => {
            warn!("Failed to initialize signal controller: {}", e);
        }
    }
}

/// Add signal service dispatchers to a RoutedHandler for RPC exposure.
///
/// Registers all signal domain services (Block, Layer, Engine, Rig, Profile,
/// Song, Setlist, Browser, Resolve, SceneTemplate, Rack) so fts-control and
/// other clients can call them over the Unix socket.
pub fn add_signal_services(handler: RoutedHandler, ctrl: &SignalController) -> RoutedHandler {
    use signal::services::{
        block_service_service_descriptor, browser_service_service_descriptor,
        engine_service_service_descriptor, layer_service_service_descriptor,
        profile_service_service_descriptor, rack_service_service_descriptor,
        resolve_service_service_descriptor, rig_service_service_descriptor,
        scene_template_service_service_descriptor, setlist_service_service_descriptor,
        song_service_service_descriptor, BlockServiceDispatcher, BrowserServiceDispatcher,
        EngineServiceDispatcher, LayerServiceDispatcher, ProfileServiceDispatcher,
        RackServiceDispatcher, ResolveServiceDispatcher, RigServiceDispatcher,
        SceneTemplateServiceDispatcher, SetlistServiceDispatcher, SongServiceDispatcher,
    };

    // Clone the inner SignalLive out of the Arc. This is cheap since all
    // fields are Arc-wrapped. Dispatchers need the concrete type, not Arc<T>.
    let svc: signal::SignalLive = (**ctrl.service()).clone();

    handler
        .with(block_service_service_descriptor(), BlockServiceDispatcher::new(svc.clone()))
        .with(layer_service_service_descriptor(), LayerServiceDispatcher::new(svc.clone()))
        .with(engine_service_service_descriptor(), EngineServiceDispatcher::new(svc.clone()))
        .with(rig_service_service_descriptor(), RigServiceDispatcher::new(svc.clone()))
        .with(profile_service_service_descriptor(), ProfileServiceDispatcher::new(svc.clone()))
        .with(song_service_service_descriptor(), SongServiceDispatcher::new(svc.clone()))
        .with(setlist_service_service_descriptor(), SetlistServiceDispatcher::new(svc.clone()))
        .with(browser_service_service_descriptor(), BrowserServiceDispatcher::new(svc.clone()))
        .with(resolve_service_service_descriptor(), ResolveServiceDispatcher::new(svc.clone()))
        .with(scene_template_service_service_descriptor(), SceneTemplateServiceDispatcher::new(svc.clone()))
        .with(rack_service_service_descriptor(), RackServiceDispatcher::new(svc))
}

/// Attach the DAW patch applier to the signal controller.
///
/// Called when a DAW role is detected and the applier is ready.
pub fn attach_daw_applier(applier: Arc<dyn DawPatchApplier>) {
    if let Some(ctrl) = controller() {
        ctrl.set_daw_applier(applier);
        info!("DAW patch applier attached to signal controller");
    }
}

/// Attach the rig scene manager to the signal controller.
///
/// Called when a rig scene manager is ready for preloaded scene switching.
pub fn attach_rig_scene_applier(applier: Arc<dyn RigSceneApplier>) {
    if let Some(ctrl) = controller() {
        ctrl.set_rig_scene_applier(applier);
        info!("Rig scene applier attached to signal controller");
    }
}
