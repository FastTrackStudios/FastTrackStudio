//! REAPER Extension - FastTrackStudio2 Plugin
//!
//! This is a REAPER extension that:
//! 1. Registers in-process DAW dispatchers (ReaperTransport, ReaperProject, etc.)
//! 2. Exposes DAW services via Unix socket (`/tmp/fts-daw-{pid}.sock`) for roam RPC
//! 3. Uses ActionsRegistry to register REAPER actions
//! 4. Builds menus in the Extensions menu
//!
//! The extension provides DAW services that can be called by external applications
//! (like the fts-control desktop app) via roam over a Unix socket.

mod action_registry;
mod auto_color;
mod dock_icon;
mod global;
mod guide_track;
mod keyflow_actions;
mod local_actions;
mod menu;
mod routed_handler;
mod session;
mod toolbar_manager;
mod visibility;

use fragile::Fragile;
use reaper_high::{MainTaskMiddleware, Reaper as HighReaper};
use reaper_low::{PluginContext, Swell};
use reaper_macros::reaper_extension_plugin;
use reaper_medium::ReaperSession;
use std::cell::RefCell;
use std::error::Error;
use std::ffi::CString;
use std::sync::OnceLock;
use tracing::{debug, info, warn};

use global::Global;
use routed_handler::RoutedHandler;
use std::path::PathBuf;
use tokio::net::UnixListener;

// ============================================================================
// Eager Plugin Loading (Helgobox pattern)
// ============================================================================

/// Loaded plugin libraries — kept alive for the process lifetime.
static LOADED_PLUGINS: OnceLock<Vec<libloading::Library>> = OnceLock::new();

/// Eagerly load FTS CLAP plugins and call `ReaperPluginEntry` on them.
///
/// This follows the Helgobox pattern: the extension loads the plugin .dylib
/// and calls its `ReaperPluginEntry`, giving the plugin its own initialized
/// `PluginContext`. Each .dylib has separate Rust statics, so the plugin
/// gets its own `reaper-high::Reaper`, `TaskSupport`, and `daw-reaper`.
fn eagerly_load_fts_plugins(context: PluginContext) {
    let mut loaded = Vec::new();

    // Reconstruct the raw plugin info from our PluginContext
    let ext_context = match context.type_specific() {
        reaper_low::TypeSpecificPluginContext::Extension(ext) => ext,
        _ => {
            warn!("Cannot eagerly load plugins: not an extension context");
            return;
        }
    };
    let mut raw_info = ext_context.to_raw();
    let h_instance = context.h_instance();

    // Search paths for fts-macros.clap bundle
    let resource_path = HighReaper::get().resource_path().to_path_buf();
    let home = std::env::var("HOME").unwrap_or_default();
    let candidates: Vec<PathBuf> = vec![
        // REAPER resource path (portable install)
        resource_path.join("UserPlugins/FX/fts-macros.clap/Contents/MacOS/fts-macros").into(),
        // Alternate: underscore variant (cdylib naming)
        resource_path.join("UserPlugins/FX/fts-macros.clap/Contents/MacOS/fts_macros").into(),
        // Standard system CLAP directory (macOS)
        PathBuf::from(&home)
            .join("Library/Audio/Plug-Ins/CLAP/fts-macros.clap/Contents/MacOS/fts-macros"),
    ];

    for path in &candidates {
        if !path.exists() {
            debug!("FTS plugin not found at: {}", path.display());
            continue;
        }

        info!("Eagerly loading fts-macros from: {}", path.display());
        match unsafe { libloading::Library::new(path) } {
            Ok(lib) => {
                // Look up ReaperPluginEntry symbol
                type EntryFn = unsafe extern "C" fn(
                    reaper_low::raw::HINSTANCE,
                    *mut reaper_low::raw::reaper_plugin_info_t,
                ) -> std::os::raw::c_int;

                match unsafe { lib.get::<EntryFn>(b"ReaperPluginEntry\0") } {
                    Ok(entry_fn) => {
                        let result =
                            unsafe { entry_fn(h_instance, &mut raw_info as *mut _) };
                        if result != 0 {
                            info!("FTS Macros: ReaperPluginEntry succeeded (result={})", result);
                            loaded.push(lib);
                        } else {
                            warn!("FTS Macros: ReaperPluginEntry returned 0 (init failed)");
                        }
                    }
                    Err(e) => {
                        warn!(
                            "FTS Macros: ReaperPluginEntry symbol not found: {}",
                            e
                        );
                    }
                }
                break; // Found the plugin, stop searching
            }
            Err(e) => {
                warn!("Failed to load fts-macros from {}: {}", path.display(), e);
            }
        }
    }

    let _ = LOADED_PLUGINS.set(loaded);
}

// Service dispatchers for method ID routing
use daw_proto::{
    AudioEngineServiceDispatcher, ExtStateServiceDispatcher, FxServiceDispatcher,
    HealthServiceDispatcher, LiveMidiServiceDispatcher, MarkerServiceDispatcher,
    MidiAnalysisServiceDispatcher, MidiServiceDispatcher, ProjectServiceDispatcher,
    RegionServiceDispatcher, RoutingServiceDispatcher, TempoMapServiceDispatcher,
    TrackServiceDispatcher, TransportServiceDispatcher,
};

// ============================================================================
// Application State
// ============================================================================

/// Global application state
struct App {
    session: RefCell<ReaperSession>,
    #[allow(dead_code)]
    tokio_runtime: tokio::runtime::Runtime,
    /// Main task middleware for processing TaskSupport queued tasks
    task_middleware: RefCell<MainTaskMiddleware>,
}

// Manual Debug impl since ReaperSession and Runtime don't implement Debug
impl std::fmt::Debug for App {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("App").finish_non_exhaustive()
    }
}

impl App {
    fn new(session: ReaperSession) -> Result<Self, Box<dyn Error>> {
        // Create a tokio runtime for async operations
        let tokio_runtime = tokio::runtime::Builder::new_multi_thread()
            .worker_threads(2)
            .enable_all()
            .build()?;

        // Initialize Global (creates TaskSupport channels)
        Global::init();

        // Create task middleware for processing queued tasks
        let task_middleware = Global::get().create_task_middleware();

        Ok(Self {
            session: RefCell::new(session),
            tokio_runtime,
            task_middleware: RefCell::new(task_middleware),
        })
    }

    /// Process pending main thread tasks (called from timer callback)
    fn process_tasks(&self) {
        self.task_middleware.borrow_mut().run();
    }

    /// Initialize the extension (register actions, menus)
    fn initialize(&mut self) -> Result<(), Box<dyn Error>> {
        info!("Initializing FastTrackStudio2 extension...");

        // Initialize the in-process actions registry
        action_registry::init_registry();
        self.tokio_runtime.block_on(async {
            register_actions().await;

            // Register the DAW dispatcher for in-process REAPER API handling
            // and initialize the session subsystem with an in-process loopback
            register_daw_dispatcher().await;
        });

        // Register the menu hook
        menu::register_extension_menu()?;

        info!("FastTrackStudio2 extension initialized");
        Ok(())
    }
}

/// Register the DAW dispatcher for in-process REAPER API handling.
///
/// This creates dispatchers for all DAW services that use actual REAPER APIs,
/// and registers them with the Host. When guest cells make DAW service calls,
/// they're handled locally in-process.
///
/// Services registered:
/// - TransportService: Play/pause/stop, position, tempo, looping
/// - ProjectService: Project management
/// - MarkerService: Markers for song boundaries
/// - RegionService: Section regions
/// - TempoMapService: Tempo and time signature changes
///
/// Uses TaskSupport from reaper-high to dispatch REAPER API calls to the main thread.
async fn register_daw_dispatcher() {
    info!("Registering DAW dispatcher for in-process REAPER API handling...");

    // Set TaskSupport for daw-reaper to use
    // This allows all REAPER implementations to dispatch calls to the main thread
    daw_reaper::set_task_support(Global::task_support());

    // Initialize transport broadcaster for low-latency state streaming
    // The timer callback will call poll_and_broadcast() to push state updates
    daw_reaper::init_transport_broadcaster();
    info!("Transport broadcaster initialized for low-latency streaming");

    // Initialize FX event broadcaster for reactive FX chain observation
    // The timer callback will call poll_and_broadcast_fx() to push FX events
    daw_reaper::init_fx_broadcaster();
    info!("FX event broadcaster initialized for reactive observation");

    // Create REAPER implementations (they use TaskSupport for main thread dispatch)
    let transport = daw_reaper::ReaperTransport::new();
    let project = daw_reaper::ReaperProject::new();
    let marker = daw_reaper::ReaperMarker::new();
    let region = daw_reaper::ReaperRegion::new();
    let tempo_map = daw_reaper::ReaperTempoMap::new();
    let audio_engine = daw_reaper::ReaperAudioEngine::new();
    let midi = daw_reaper::ReaperMidi::new();
    let midi_analysis = daw_reaper::ReaperMidiAnalysis::new();
    let fx = daw_reaper::ReaperFx::new();
    let track = daw_reaper::ReaperTrack::new();
    let routing = daw_reaper::ReaperRouting::new();
    let live_midi = daw_reaper::ReaperLiveMidi::new();
    let ext_state = daw_reaper::ReaperExtState::new();
    let health = daw_reaper::ReaperHealth::new();

    // Import service descriptor functions for method_id routing.
    // Each `*_service_descriptor()` is generated by the #[roam::service] macro
    // and re-exported via `daw_proto::*`.
    use daw_proto::{
        audio_engine_service_service_descriptor, ext_state_service_service_descriptor,
        fx_service_service_descriptor, health_service_service_descriptor,
        live_midi_service_service_descriptor, marker_service_service_descriptor,
        midi_analysis_service_service_descriptor, midi_service_service_descriptor,
        project_service_service_descriptor, region_service_service_descriptor,
        routing_service_service_descriptor, tempo_map_service_service_descriptor,
        track_service_service_descriptor, transport_service_service_descriptor,
    };

    // Create dispatchers and compose them via RoutedHandler (method_id routing)
    let daw_handler = RoutedHandler::new()
        .with(transport_service_service_descriptor(), TransportServiceDispatcher::new(transport))
        .with(project_service_service_descriptor(), ProjectServiceDispatcher::new(project))
        .with(marker_service_service_descriptor(), MarkerServiceDispatcher::new(marker))
        .with(region_service_service_descriptor(), RegionServiceDispatcher::new(region))
        .with(tempo_map_service_service_descriptor(), TempoMapServiceDispatcher::new(tempo_map))
        .with(audio_engine_service_service_descriptor(), AudioEngineServiceDispatcher::new(audio_engine))
        .with(midi_service_service_descriptor(), MidiServiceDispatcher::new(midi))
        .with(midi_analysis_service_service_descriptor(), MidiAnalysisServiceDispatcher::new(midi_analysis))
        .with(fx_service_service_descriptor(), FxServiceDispatcher::new(fx))
        .with(track_service_service_descriptor(), TrackServiceDispatcher::new(track))
        .with(routing_service_service_descriptor(), RoutingServiceDispatcher::new(routing))
        .with(live_midi_service_service_descriptor(), LiveMidiServiceDispatcher::new(live_midi))
        .with(ext_state_service_service_descriptor(), ExtStateServiceDispatcher::new(ext_state))
        .with(health_service_service_descriptor(), HealthServiceDispatcher::new(health));

    // Initialize the session subsystem with an in-process loopback to the DAW handler.
    // This sets up Daw::init() so the session crate can call daw-control methods locally,
    // and creates SetlistServiceImpl + SongServiceImpl.
    match session::init(daw_handler).await {
        Ok(()) => {
            info!("Session subsystem initialized with in-process DAW loopback");
        }
        Err(e) => {
            warn!("Failed to initialize session subsystem: {}", e);
        }
    }

    // Start the Unix socket server with the combined handler.
    // Note: session::init() already stored the handler in the SessionManager.
    // For the socket server, we rebuild the handler including session services.
    if let Some(session_mgr) = session::SessionManager::try_get() {
        let session_handler = session_mgr.create_handler();
        start_unix_socket_server(session_handler);
        info!("Unix socket serves DAW + Session services");
    } else {
        // Session init failed — can't serve without handler (it was moved)
        warn!("Unix socket not started (session init failed)");
    }

    // Write FTS_DAW_ROLE env var to ExtState so fts-control can classify this instance.
    // Uses persist=false — the role only exists while REAPER is running.
    if let Ok(role) = std::env::var("FTS_DAW_ROLE") {
        let section = CString::new("FTS").expect("valid CString");
        let key = CString::new("role").expect("valid CString");
        let value = CString::new(role.clone()).expect("valid CString");
        let low = reaper_high::Reaper::get().medium_reaper().low();
        unsafe {
            low.SetExtState(section.as_ptr(), key.as_ptr(), value.as_ptr(), false);
        }
        info!("FTS_DAW_ROLE='{}' written to ExtState FTS/role", role);

        // Set a distinct dock icon and color theme per role.
        dock_icon::set_dock_icon_for_role(&role);
        dock_icon::set_theme_for_role(&role);
    }

    info!(
        "DAW dispatcher registered (Transport, Project, Marker, Region, TempoMap, Midi, MidiAnalysis, AudioEngine, Fx, Track, Routing, LiveMidi, ExtState, Session)"
    );
}

// ============================================================================
// Unix Socket Server
// ============================================================================

/// Directory for DAW sockets (discoverable by fts-control).
const SOCKET_DIR: &str = "/tmp";

/// Prefix for PID-based socket names. fts-control globs `fts-daw-*.sock` to
/// discover all running REAPER instances.
const SOCKET_PREFIX: &str = "fts-daw-";

/// Build the socket path for this REAPER instance.
///
/// Default: `/tmp/fts-daw-{pid}.sock` — each REAPER process gets a unique
/// socket so fts-control can discover and connect to all of them.
///
/// Override with `FTS_SOCKET` env var for testing or custom setups.
fn socket_path() -> PathBuf {
    std::env::var("FTS_SOCKET")
        .map(PathBuf::from)
        .unwrap_or_else(|_| {
            let pid = std::process::id();
            PathBuf::from(format!("{}/{}{}.sock", SOCKET_DIR, SOCKET_PREFIX, pid))
        })
}

/// Start the Unix socket server exposing DAW services to external clients.
///
/// Spawns a background tokio task that accepts connections on `/tmp/fts-daw-{pid}.sock`.
/// Each connection runs a roam session with the full DAW handler, allowing
/// fts-control desktop (or any roam client) to call all DAW + Session services.
fn start_unix_socket_server(handler: RoutedHandler) {
    let path = socket_path();

    // Remove stale socket from a previous run
    let _ = std::fs::remove_file(&path);

    let listener = match UnixListener::bind(&path) {
        Ok(l) => l,
        Err(e) => {
            warn!("Failed to bind Unix socket at {}: {}", path.display(), e);
            return;
        }
    };

    info!("Unix socket server listening on {}", path.display());

    let handler = std::sync::Arc::new(handler);

    moire::task::spawn(async move {
        loop {
            match listener.accept().await {
                Ok((stream, _addr)) => {
                    info!("fts-control client connected via Unix socket");
                    let handler = handler.clone();
                    moire::task::spawn(async move {
                        let link = roam_stream::StreamLink::unix(stream);
                        match roam::acceptor(link)
                            .establish::<roam::DriverCaller>(handler.as_ref().clone())
                            .await
                        {
                            Ok((_caller, _session_handle)) => {
                                // Session message processing runs in background tasks
                                // spawned by establish(). We must keep `_caller` alive —
                                // dropping it closes the root connection in roam v7,
                                // which cancels any pending RPCs from the client.
                                debug!("Unix socket session established");
                                std::future::pending::<()>().await;
                            }
                            Err(e) => {
                                warn!("Unix socket handshake failed: {:?}", e);
                            }
                        }
                    });
                }
                Err(e) => {
                    warn!("Unix socket accept error: {}", e);
                }
            }
        }
    });
}

async fn register_actions() {
    local_actions::register_toggle_states();

    if let Err(error) =
        action_registry::register_actions(vec![action_registry::ActionRegistrationSource::Local(
            local_actions::builtin_local_actions(),
        )])
        .await
    {
        warn!(%error, "Failed to register actions");
        return;
    }

    // Add quick input validation buttons to the default FTS floating toolbar.
    let add_toggle = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new("FTS_INPUT_TOGGLE_INPUT_RUNTIME", "Toggle Input")
            .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32)),
        "__input_runtime__",
    );
    if let Err(error) = add_toggle {
        warn!(%error, "Failed to queue Toggle Input toolbar button");
    }

    let add_log = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new("FTS_INPUT_LOG_INPUT_RUNTIME_STATE", "Log Input")
            .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32)),
        "__input_runtime__",
    );
    if let Err(error) = add_log {
        warn!(%error, "Failed to queue Log Input toolbar button");
    }

    let add_intercept = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new("FTS_INPUT_TOGGLE_INPUT_INTERCEPT", "Intercept")
            .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32)),
        "__input_runtime__",
    );
    if let Err(error) = add_intercept {
        warn!(%error, "Failed to queue Intercept toolbar button");
    }

    let add_menu = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new("FTS_INPUT_INPUT_MENU", "Input Menu")
            .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32))
            .double_wide(),
        "__input_runtime__",
    );
    if let Err(error) = add_menu {
        warn!(%error, "Failed to queue Input Menu toolbar button");
    }

    let add_reset_mouse = toolbar_manager::add_button(
        &toolbar_manager::ToolbarButton::new(
            "FTS_INPUT_RESET_MOUSE_MODIFIERS",
            "Reset Mouse Modifiers",
        )
        .on_toolbar(toolbar_manager::ToolbarTarget::Floating(32)),
        "__input_runtime__",
    );
    if let Err(error) = add_reset_mouse {
        warn!(%error, "Failed to queue Reset Mouse Modifiers toolbar button");
    }
}

/// Global App instance (wrapped in Fragile to ensure main-thread-only access)
static APP_INSTANCE: OnceLock<Fragile<App>> = OnceLock::new();

/// Get the global App instance
fn get_app() -> Option<&'static Fragile<App>> {
    APP_INSTANCE.get()
}

/// Timer callback for periodic updates (runs on main thread)
extern "C" fn timer_callback() {
    // Process any pending async tasks
    if let Some(app_fragile) = get_app() {
        let app = app_fragile.get();

        // Process pending main thread tasks via TaskSupport middleware
        app.process_tasks();

        // Keep input mouse hooks attached to newly opened windows (MIDI editors).
        input_reaper::check_and_hook_windows();

        // Check for which-key sequence timeout (~1s idle = reset + hide overlay)
        input_reaper::check_which_key_timeout();

        // Refresh which-key overlay position (tracks arrange view movement)
        input_reaper::refresh_which_key_overlay();

        // Poll transport state and broadcast to subscribers
        // This runs directly on main thread, avoiding async round-trip latency
        daw_reaper::poll_and_broadcast();

        // Poll FX chain state and broadcast events for monitored chains
        daw_reaper::poll_and_broadcast_fx();

        // Re-apply auto-color when track names change (throttled ~1s)
        crate::auto_color::poll_and_recolor();

        // Apply deferred toolbar operations from workflow/input systems.
        toolbar_manager::process_deferred_ops();
    }
}

/// REAPER will call this extension entry-point function once when it's starting.
#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    // Initialize tracing — write to /tmp/fts-reaper.log
    let log_file =
        std::fs::File::create("/tmp/fts-reaper.log").expect("Failed to create /tmp/fts-reaper.log");
    tracing_subscriber::fmt()
        .with_writer(std::sync::Mutex::new(log_file))
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::DEBUG.into()),
        )
        .init();

    info!("FastTrackStudio REAPER Extension starting...");

    // Make Swell available globally (required for swell-ui menu operations)
    let _ = Swell::make_available_globally(Swell::load(context));

    // Initialize REAPER high-level API
    match HighReaper::load(context).setup() {
        Ok(_) => {
            info!("REAPER high-level API initialized");
        }
        Err(_) => {
            debug!("REAPER high-level API already initialized");
        }
    }

    // Create a medium-level API session
    let session = ReaperSession::load(context);

    // Create the App (initializes Global/TaskSupport)
    let mut app = App::new(session)?;

    // Initialize the extension (mut needed to store SHM temp dir)
    app.initialize()?;

    // Register REAPER input bridge on top of the shared input core.
    if let Err(error) =
        input_reaper::register_with_default_keymap(input_reaper::InputRuntimeConfig::default())
    {
        warn!(%error, "Failed to register input-reaper runtime");
    }

    // Eagerly load FTS CLAP plugins (Helgobox pattern)
    // Must happen after initialize() so TaskSupport is ready
    eagerly_load_fts_plugins(context);

    // Store app globally
    APP_INSTANCE
        .set(Fragile::new(app))
        .expect("App already initialized");

    // Register timer callback for periodic updates
    let app = APP_INSTANCE.get().expect("App should be initialized").get();
    app.session
        .borrow_mut()
        .plugin_register_add_timer(timer_callback)?;

    info!("FastTrackStudio REAPER Extension initialized successfully");
    Ok(())
}
