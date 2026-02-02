//! REAPER Extension - FastTrackStudio Plugin
//!
//! This is a REAPER extension that:
//! 1. Spawns cells (session, daw-reaper, gateway-ws)
//! 2. Uses ActionsRegistry to query cells for actions
//! 3. Registers actions with REAPER
//! 4. Builds menus in the Extensions menu

mod action_registry;
mod menu;

use fragile::Fragile;
use reaper_high::Reaper as HighReaper;
use reaper_low::{PluginContext, Swell};
use reaper_macros::reaper_extension_plugin;
use reaper_medium::ReaperSession;
use std::cell::RefCell;
use std::error::Error;
use std::path::PathBuf;
use std::sync::OnceLock;
use tracing::{debug, info, warn};

use host_runtime::{init_shm_infrastructure, CellConfig, Host};

/// Global application state
struct App {
    session: RefCell<ReaperSession>,
    #[allow(dead_code)]
    tokio_runtime: tokio::runtime::Runtime,
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

        Ok(Self {
            session: RefCell::new(session),
            tokio_runtime,
        })
    }

    /// Initialize the extension (register actions, menus, spawn cells)
    fn initialize(&self) -> Result<(), Box<dyn Error>> {
        info!("Initializing FastTrackStudio extension...");

        // Initialize the in-process actions registry
        action_registry::init_registry();

        // Initialize SHM infrastructure and spawn cells in the tokio runtime
        self.tokio_runtime.block_on(async {
            // Initialize SHM infrastructure
            match init_shm_infrastructure().await {
                Ok(_temp_dir) => {
                    info!("SHM infrastructure initialized");
                }
                Err(e) => {
                    warn!("Failed to initialize SHM infrastructure: {}", e);
                    return;
                }
            }

            // Register cells for lazy spawning
            let cell_dir = host_runtime::default_cell_dir();
            register_cells(&cell_dir);

            // Spawn and register cells that implement DefinesActions
            register_cell_actions().await;
        });

        // Register the menu hook
        menu::register_extension_menu()?;

        info!("FastTrackStudio extension initialized");
        Ok(())
    }
}

/// Register cells that implement DefinesActions with the actions registry
async fn register_cell_actions() {
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

    // Future: Register other cells that implement DefinesActions
    // e.g., daw-reaper, gateway-ws, etc.
}

/// Global App instance (wrapped in Fragile to ensure main-thread-only access)
static APP_INSTANCE: OnceLock<Fragile<App>> = OnceLock::new();

/// Get the global App instance
fn get_app() -> Option<&'static Fragile<App>> {
    APP_INSTANCE.get()
}

/// Register cells with the Host for lazy spawning
fn register_cells(cell_dir: &PathBuf) {
    // DAW cell - using standalone for now (will switch to daw-reaper later)
    CellConfig::new("daw-standalone", cell_dir).register();

    // Session cell - forwards to DAW
    CellConfig::new("session", cell_dir)
        .forwards_to(&["daw-standalone"])
        .register();

    // Gateway WebSocket cell - forwards to DAW
    CellConfig::new("gateway-ws", cell_dir)
        .forwards_to(&["daw-standalone"])
        .register();

    info!("Cells registered for lazy spawning");
}

/// Timer callback for periodic updates
extern "C" fn timer_callback() {
    // Process any pending async tasks
    if let Some(app_fragile) = get_app() {
        let _app = app_fragile.get();
        // Future: poll async tasks, update state, etc.
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

    // Create the App
    let app = App::new(session)?;

    // Initialize the extension
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
