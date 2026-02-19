//! REAPER Extension - FastTrackStudio2 Plugin
//!
//! This is a REAPER extension that:
//! 1. Registers in-process DAW dispatcher (ReaperTransport, ReaperProject, etc.)
//! 2. Uses ActionsRegistry to register REAPER actions
//! 3. Builds menus in the Extensions menu
//!
//! The extension provides DAW services that can be called by external applications
//! (like the fts-control desktop app) via the host runtime infrastructure.

mod action_registry;
mod auto_color;
mod global;
mod local_actions;
mod menu;
mod toolbar_manager;
mod visibility;

use fragile::Fragile;
use reaper_high::{MainTaskMiddleware, Reaper as HighReaper};
use reaper_low::{PluginContext, Swell};
use reaper_macros::reaper_extension_plugin;
use reaper_medium::ReaperSession;
use std::cell::RefCell;
use std::error::Error;
use std::sync::{Arc, OnceLock};
use tracing::{debug, info, warn};

use global::Global;
use roam::session::RoutedDispatcher;
use roam_telemetry::{ExporterConfig, OtlpExporter, TelemetryMiddleware};
use std::time::Duration;

// Service dispatchers for method ID routing
use daw_proto::{
    AudioEngineServiceDispatcher, FxServiceDispatcher, LiveMidiServiceDispatcher,
    MarkerServiceDispatcher, MidiAnalysisServiceDispatcher, MidiServiceDispatcher,
    ProjectServiceDispatcher, RegionServiceDispatcher, RoutingServiceDispatcher,
    TempoMapServiceDispatcher, TrackServiceDispatcher, TransportServiceDispatcher,
};

// ============================================================================
// Telemetry Configuration
// ============================================================================

/// Create telemetry middleware for daw-reaper dispatchers (OTLP only, no console logging)
fn create_telemetry() -> TelemetryMiddleware<OtlpExporter> {
    let otlp_endpoint = std::env::var("OTEL_EXPORTER_OTLP_ENDPOINT")
        .unwrap_or_else(|_| "http://localhost:4318/v1/traces".to_string());

    let otlp_exporter = OtlpExporter::with_config(ExporterConfig {
        endpoint: otlp_endpoint,
        service_name: "daw-reaper".to_string(),
        resource_attributes: vec![],
        max_batch_size: 10,
        max_batch_delay: Duration::from_secs(2),
        timeout: Duration::from_secs(10),
    });

    TelemetryMiddleware::new(otlp_exporter)
}

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
            register_daw_dispatcher();
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
fn register_daw_dispatcher() {
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

    // Create telemetry middleware for OTLP export
    let telemetry = create_telemetry();

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

    // Create dispatchers with telemetry middleware
    let transport_dispatcher =
        TransportServiceDispatcher::new(transport).with_middleware(telemetry.clone());
    let project_dispatcher =
        ProjectServiceDispatcher::new(project).with_middleware(telemetry.clone());
    let marker_dispatcher = MarkerServiceDispatcher::new(marker).with_middleware(telemetry.clone());
    let region_dispatcher = RegionServiceDispatcher::new(region).with_middleware(telemetry.clone());
    let tempo_map_dispatcher =
        TempoMapServiceDispatcher::new(tempo_map).with_middleware(telemetry.clone());
    let midi_dispatcher = MidiServiceDispatcher::new(midi).with_middleware(telemetry.clone());
    let midi_analysis_dispatcher =
        MidiAnalysisServiceDispatcher::new(midi_analysis).with_middleware(telemetry.clone());
    let audio_engine_dispatcher =
        AudioEngineServiceDispatcher::new(audio_engine).with_middleware(telemetry.clone());
    let fx_dispatcher = FxServiceDispatcher::new(fx).with_middleware(telemetry.clone());
    let track_dispatcher = TrackServiceDispatcher::new(track).with_middleware(telemetry.clone());
    let routing_dispatcher =
        RoutingServiceDispatcher::new(routing).with_middleware(telemetry.clone());
    let live_midi_dispatcher = LiveMidiServiceDispatcher::new(live_midi).with_middleware(telemetry);

    // Compose all dispatchers together using RoutedDispatcher chaining
    // The RoutedDispatcher chains dispatchers: first tries left, falls through to right
    let transport_project = RoutedDispatcher::new(project_dispatcher, transport_dispatcher);
    let with_marker = RoutedDispatcher::new(transport_project, marker_dispatcher);
    let with_region = RoutedDispatcher::new(with_marker, region_dispatcher);
    let with_tempo_map = RoutedDispatcher::new(with_region, tempo_map_dispatcher);
    let with_midi = RoutedDispatcher::new(with_tempo_map, midi_dispatcher);
    let with_midi_analysis = RoutedDispatcher::new(with_midi, midi_analysis_dispatcher);
    let with_audio_engine = RoutedDispatcher::new(with_midi_analysis, audio_engine_dispatcher);
    let with_fx = RoutedDispatcher::new(with_audio_engine, fx_dispatcher);
    let with_track = RoutedDispatcher::new(with_fx, track_dispatcher);
    let with_routing = RoutedDispatcher::new(with_track, routing_dispatcher);
    let daw_dispatcher = RoutedDispatcher::new(with_routing, live_midi_dispatcher);

    // TODO: Register DAW dispatcher with new infrastructure (roam is evolving)
    let _daw_dispatcher = Arc::new(daw_dispatcher);

    info!(
        "DAW dispatcher registered (Transport, Project, Marker, Region, TempoMap, Midi, MidiAnalysis, AudioEngine, Fx, Track, Routing, LiveMidi) with OTLP telemetry"
    );
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
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into())
                .add_directive("roam_telemetry::exporter=error".parse().unwrap()),
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
