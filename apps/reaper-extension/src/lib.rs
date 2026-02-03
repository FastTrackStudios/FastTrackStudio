//! REAPER Extension - FastTrackStudio2 Plugin
//!
//! This is a REAPER extension that:
//! 1. Registers in-process DAW dispatcher (ReaperTransport, ReaperProject)
//! 2. Spawns cells (session, gateway-ws)
//! 3. Uses ActionsRegistry to query cells for actions
//! 4. Registers actions with REAPER
//! 5. Builds menus in the Extensions menu
//!
//! Cell binaries are loaded from Extensions/FTS2/ relative to the REAPER resource path.
//! Path structure:
//! - REAPER resource dir: /Users/.../FastTrackStudio/Reaper/FTS-TRACKS/
//! - Walk up to:          /Users/.../FastTrackStudio/
//! - Cell directory:      /Users/.../FastTrackStudio/Extensions/FTS2/

mod action_registry;
mod global;
mod menu;

use fragile::Fragile;
use reaper_high::{MainTaskMiddleware, Reaper as HighReaper};
use reaper_low::{PluginContext, Swell};
use reaper_macros::reaper_extension_plugin;
use reaper_medium::ReaperSession;
use std::cell::RefCell;
use std::error::Error;
use std::path::PathBuf;
use std::sync::{Arc, OnceLock};
use tracing::{debug, info, warn};

use global::Global;
use host_runtime::{init_shm_infrastructure, spawn_tracing_consumer, CellConfig, Host};
use roam::session::RoutedDispatcher;
use roam_telemetry::{
    ExporterConfig, LoggingExporter, OtlpExporter, SpanExporter, TelemetryMiddleware,
};
use std::time::Duration;

// Service dispatchers for method ID routing
use actions_proto::DefinesActionsDispatcher;
use daw_proto::{
    MarkerServiceDispatcher, ProjectServiceDispatcher, RegionServiceDispatcher,
    TempoMapServiceDispatcher, TransportServiceDispatcher,
};
use session_proto::{SessionServiceDispatcher, SetlistServiceDispatcher, SongServiceDispatcher};

// ============================================================================
// Telemetry Configuration
// ============================================================================

/// Composite exporter that sends to both OTLP and console
#[derive(Clone)]
struct CompositeExporter {
    otlp: OtlpExporter,
    logging: LoggingExporter,
}

impl SpanExporter for CompositeExporter {
    fn send(&self, span: roam_telemetry::Span) {
        self.logging.send(span.clone());
        self.otlp.send(span);
    }

    fn service_name(&self) -> &str {
        "daw-reaper"
    }
}

/// Create telemetry middleware for daw-reaper dispatchers
fn create_telemetry() -> TelemetryMiddleware<CompositeExporter> {
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

    let logging_exporter = LoggingExporter::new("daw-reaper");

    TelemetryMiddleware::new(CompositeExporter {
        otlp: otlp_exporter,
        logging: logging_exporter,
    })
}

// ============================================================================
// Application State
// ============================================================================

/// Global application state
struct App {
    session: RefCell<ReaperSession>,
    #[allow(dead_code)]
    tokio_runtime: tokio::runtime::Runtime,
    /// Keep SHM temp directory alive for extension lifetime
    #[allow(dead_code)]
    shm_temp_dir: Option<tempfile::TempDir>,
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
        // Create a tokio runtime for async cell communication
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
            shm_temp_dir: None,
            task_middleware: RefCell::new(task_middleware),
        })
    }

    /// Set the SHM temp directory (must be kept alive for extension lifetime)
    fn set_shm_temp_dir(&mut self, temp_dir: tempfile::TempDir) {
        self.shm_temp_dir = Some(temp_dir);
    }

    /// Process pending main thread tasks (called from timer callback)
    fn process_tasks(&self) {
        self.task_middleware.borrow_mut().run();
    }

    /// Initialize the extension (register actions, menus, spawn cells)
    fn initialize(&mut self) -> Result<(), Box<dyn Error>> {
        info!("Initializing FastTrackStudio2 extension...");

        // Initialize the in-process actions registry
        action_registry::init_registry();

        // Get cell directory from REAPER's resource path
        let cell_dir = get_cell_directory();
        info!("Cell directory: {}", cell_dir.display());

        // Initialize SHM infrastructure and spawn cells in the tokio runtime
        let shm_temp_dir = self.tokio_runtime.block_on(async {
            // Initialize SHM infrastructure
            let temp_dir = match init_shm_infrastructure().await {
                Ok(temp_dir) => {
                    info!("SHM infrastructure initialized");
                    Some(temp_dir)
                }
                Err(e) => {
                    warn!("Failed to initialize SHM infrastructure: {}", e);
                    return None;
                }
            };

            // Start tracing consumer for cell log aggregation
            spawn_tracing_consumer();

            // Register the DAW dispatcher for in-process REAPER API handling
            // This allows guest cells to make DAW service calls (play, stop, etc.)
            // that are handled locally using REAPER APIs via TaskSupport
            register_daw_dispatcher();

            // Register cells for lazy spawning
            register_cells(&cell_dir);

            // Spawn and register cells that implement DefinesActions
            register_cell_actions().await;

            temp_dir
        });

        // Store the temp directory to keep SHM files alive
        if let Some(temp_dir) = shm_temp_dir {
            self.set_shm_temp_dir(temp_dir);
        }

        // Register the menu hook
        menu::register_extension_menu()?;

        info!("FastTrackStudio2 extension initialized");
        Ok(())
    }
}

/// Get the cell binary directory from REAPER's resource path.
///
/// Path structure:
/// - REAPER resource dir: /Users/.../FastTrackStudio/Reaper/FTS-TRACKS/
/// - Walk up to:          /Users/.../FastTrackStudio/
/// - Cell directory:      /Users/.../FastTrackStudio/Extensions/FTS2/
fn get_cell_directory() -> PathBuf {
    let reaper = HighReaper::get();
    let medium_reaper = reaper.medium_reaper();

    let cell_dir = medium_reaper.get_resource_path(|resource_path| {
        info!("REAPER resource path: {}", resource_path);

        // Walk up TWO directories from resource path
        // resource_path: /Users/.../FastTrackStudio/Reaper/FTS-TRACKS/
        // parent:        /Users/.../FastTrackStudio/Reaper/
        // grandparent:   /Users/.../FastTrackStudio/
        let parent = resource_path.parent().unwrap_or(camino::Utf8Path::new("/"));

        let grandparent = parent.parent().unwrap_or(camino::Utf8Path::new("/"));

        info!("FastTrackStudio root: {}", grandparent);

        // Go to Extensions/FTS2 (using FTS2 to distinguish from original FastTrackStudio)
        let cell_dir = grandparent.join("Extensions").join("FTS2");

        info!("Calculated cell directory: {}", cell_dir);

        cell_dir.into_std_path_buf()
    });

    cell_dir
}

/// Register the DAW dispatcher for in-process REAPER API handling.
///
/// This creates dispatchers for TransportService and ProjectService that use
/// actual REAPER APIs, and registers them with the Host. When guest cells
/// make DAW service calls, they're handled locally in-process.
///
/// Uses TaskSupport from reaper-high to dispatch REAPER API calls to the main thread.
fn register_daw_dispatcher() {
    info!("Registering DAW dispatcher for in-process REAPER API handling...");

    // Set TaskSupport for daw-reaper to use
    // This allows ReaperTransport and ReaperProject to dispatch calls to the main thread
    daw_reaper::set_task_support(Global::task_support());

    // Create telemetry middleware for OTLP export
    let telemetry = create_telemetry();

    // Create REAPER implementations (they use TaskSupport for main thread dispatch)
    let transport = daw_reaper::ReaperTransport::new();
    let project = daw_reaper::ReaperProject::new();

    // Create dispatchers with telemetry middleware
    let transport_dispatcher =
        daw_proto::TransportServiceDispatcher::new(transport).with_middleware(telemetry.clone());
    let project_dispatcher =
        daw_proto::ProjectServiceDispatcher::new(project).with_middleware(telemetry);

    // Combine into a single dispatcher
    let daw_dispatcher = RoutedDispatcher::new(transport_dispatcher, project_dispatcher);

    // Register with the Host
    Host::get().set_daw_dispatcher(Arc::new(daw_dispatcher));

    info!("DAW dispatcher registered (TransportService + ProjectService) with OTLP telemetry");
}

/// Register cells that implement DefinesActions with the actions registry
async fn register_cell_actions() {
    // DAW services are handled in-process by the DAW dispatcher
    // No need to spawn a separate daw-reaper cell

    // Spawn the session cell and register its actions
    info!("Spawning session cell...");
    match Host::get().spawn_pending_cell("session").await {
        Some(handle) => {
            info!("Session cell ready, registering actions...");
            action_registry::register_cell("session", handle).await;
        }
        None => {
            warn!("Failed to spawn session cell");
        }
    }

    // Spawn gateway-ws for external connections (fts-control app)
    info!("Spawning gateway-ws cell...");
    match Host::get().spawn_pending_cell("gateway-ws").await {
        Some(_handle) => {
            info!("gateway-ws cell ready (WebSocket server started)");
        }
        None => {
            warn!("Failed to spawn gateway-ws cell");
        }
    }
}

/// Global App instance (wrapped in Fragile to ensure main-thread-only access)
static APP_INSTANCE: OnceLock<Fragile<App>> = OnceLock::new();

/// Get the global App instance
fn get_app() -> Option<&'static Fragile<App>> {
    APP_INSTANCE.get()
}

/// Register cells with the Host for lazy spawning
fn register_cells(cell_dir: &PathBuf) {
    // Session cell - DAW calls are handled by the host's in-process DAW dispatcher
    // Session needs to forward DAW calls (transport, project, markers, regions, tempo)
    CellConfig::new("session", cell_dir)
        .inherit_stdio(true)
        .forwards_to_with_methods("daw-reaper", || {
            daw_proto::TransportServiceDispatcher::<()>::method_ids()
                .into_iter()
                .chain(daw_proto::ProjectServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::MarkerServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::RegionServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::TempoMapServiceDispatcher::<()>::method_ids())
                .collect()
        })
        .register();

    // Gateway WebSocket cell - forwards to both DAW and Session
    // Routes method IDs to the correct cell based on which service handles them
    CellConfig::new("gateway-ws", cell_dir)
        .inherit_stdio(true)
        .forwards_to_with_methods("daw-reaper", || {
            // DAW services: Transport, Project, Markers, Regions, TempoMap
            daw_proto::TransportServiceDispatcher::<()>::method_ids()
                .into_iter()
                .chain(daw_proto::ProjectServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::MarkerServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::RegionServiceDispatcher::<()>::method_ids())
                .chain(daw_proto::TempoMapServiceDispatcher::<()>::method_ids())
                .collect()
        })
        .forwards_to_with_methods("session", || {
            // Session services: Setlist, Song, Session, DefinesActions
            session_proto::SetlistServiceDispatcher::<()>::method_ids()
                .into_iter()
                .chain(session_proto::SongServiceDispatcher::<()>::method_ids())
                .chain(session_proto::SessionServiceDispatcher::<()>::method_ids())
                .chain(actions_proto::DefinesActionsDispatcher::<()>::method_ids())
                .collect()
        })
        .register();

    info!("Cells registered for lazy spawning (session, gateway-ws)");
}

/// Timer callback for periodic updates (runs on main thread)
extern "C" fn timer_callback() {
    // Process any pending async tasks
    if let Some(app_fragile) = get_app() {
        let app = app_fragile.get();

        // Process pending main thread tasks via TaskSupport middleware
        app.process_tasks();
    }
}

/// REAPER will call this extension entry-point function once when it's starting.
#[reaper_extension_plugin]
fn plugin_main(context: PluginContext) -> Result<(), Box<dyn Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
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
