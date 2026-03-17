//! Signal system bootstrap for the REAPER extension.
//!
//! Creates a [`SignalController`] with a file-backed SQLite database,
//! wires the [`ReaperPatchApplier`] and [`RigSceneManager`], and stores
//! the controller globally so action handlers and RPC services can access it.

use crate::routed_handler::RoutedHandler;
use signal::{ActiveContext, DawPatchApplier, RigSceneApplier, SignalController};
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
    let db_dir = utils::paths::library_dir();

    // Ensure the directory exists
    if let Err(e) = std::fs::create_dir_all(&db_dir) {
        warn!("Failed to create signal library directory {}: {}", db_dir.display(), e);
        return;
    }

    let db_path = utils::paths::signal_db().to_string_lossy().to_string();
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

/// Wire up the signal appliers for the current REAPER session.
///
/// Creates `ReaperPatchApplier` and `RigSceneManager`, attaches them to the
/// global `SignalController`, and sets the initial `ActiveContext` based on
/// available profiles/rigs.
///
/// Must be called AFTER both `signal_bridge::init()` and `Daw::init()` complete.
pub async fn wire_appliers() {
    let Some(ctrl) = controller() else {
        warn!("Cannot wire signal appliers: controller not initialized");
        return;
    };

    // Get the rig name from environment or default
    let rig_name = match std::env::var("FTS_RIG_TYPE").as_deref() {
        Ok("guitar") => "Guitar Rig".to_string(),
        Ok("bass") => "Bass Rig".to_string(),
        Ok("keys") => "Keys Rig".to_string(),
        Ok("vocals") => "Vocals Rig".to_string(),
        Ok("drums") => "Drums Rig".to_string(),
        Ok(other) => other.to_string(),
        Err(_) => "Guitar Rig".to_string(),
    };

    // Get a DAW project handle via the in-process loopback
    let daw_handle = daw::Daw::get();
    let project = match daw_handle.projects().await {
        Ok(projects) => {
            if let Some(p) = projects.into_iter().next() {
                p
            } else {
                warn!("No REAPER projects found, cannot wire signal appliers");
                return;
            }
        }
        Err(e) => {
            warn!("Failed to list projects for signal applier: {e}");
            return;
        }
    };

    // Create and attach the ReaperPatchApplier
    let applier = Arc::new(signal::reaper_applier::ReaperPatchApplier::new());
    if let Err(e) = applier.set_target(project.clone(), &rig_name).await {
        warn!("Failed to set patch applier target '{}': {:?}", rig_name, e);
    } else {
        ctrl.set_daw_applier(applier);
        info!("Patch applier wired to '{}'", rig_name);
    }

    // Create and attach the RigSceneManager for preloaded scene switching
    let signal_live = ctrl.service().clone();
    let manager = Arc::new(signal::rig_scene_manager::RigSceneManager::new(signal_live));
    if let Err(e) = manager.set_target(&rig_name, project).await {
        warn!("Failed to set rig scene manager target: {e}");
    } else {
        ctrl.set_rig_scene_applier(manager);
        info!("Rig scene manager wired for '{}'", rig_name);
    }

    // Set default ActiveContext — try to find a profile, fall back to rig
    set_default_active_context(ctrl).await;
}

/// Set the initial ActiveContext by loading the first available profile or rig.
async fn set_default_active_context(ctrl: &SignalController) {
    // Try profiles first (via controller ops)
    if let Ok(profiles) = ctrl.profiles().list().await {
        if let Some(profile) = profiles.first() {
            ctrl.active_context().set(ActiveContext::Profile {
                id: profile.id.clone(),
                active_index: 0,
            });
            info!("Active context set to profile '{}' ({})", profile.name, profile.id);
            return;
        }
    }

    // Fall back to rigs
    if let Ok(rigs) = ctrl.rigs().list().await {
        if let Some(rig) = rigs.first() {
            ctrl.active_context().set(ActiveContext::Rig {
                id: rig.id.clone(),
                active_index: 0,
            });
            info!("Active context set to rig '{}' ({})", rig.name, rig.id);
            return;
        }
    }

    info!("No profiles or rigs found — active context remains empty");
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
