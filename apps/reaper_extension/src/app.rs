//! Application Container
//!
//! Manages initialization and provides access to all services.

use reaper_medium::ReaperSession;
use std::error::Error;
use std::sync::Arc;

use crate::infrastructure::action_registry::ActionRegistry;
#[cfg(feature = "core")]
use crate::infrastructure::change_detection::ChangeDetection;
#[cfg(feature = "core")]
use crate::infrastructure::reactive_app_state::ReactiveAppStateService;
#[cfg(feature = "core")]
use crate::infrastructure::reactive_polling::ReactivePollingService;
#[cfg(feature = "core")]
use crate::services::{
    CommandService, ReaperTransport, SeekService, SetlistService, SmoothSeekService, StreamService,
};
use crate::extension_host::ExtensionHost;
use tracing::{debug, error, info, warn};

/// Application container - holds all services and manages initialization
pub struct App {
    // Infrastructure (always needed for menu)
    pub action_registry: Arc<ActionRegistry>,

    // Services (only when core feature is enabled)
    #[cfg(feature = "core")]
    pub setlist_service: Arc<SetlistService>,
    #[cfg(feature = "core")]
    pub command_service: Arc<CommandService>,
    #[cfg(feature = "core")]
    pub seek_service: Arc<SeekService>,
    #[cfg(feature = "core")]
    pub stream_service: Arc<StreamService>,
    #[cfg(feature = "core")]
    pub smooth_seek_service: Arc<SmoothSeekService>,

    // Reactive infrastructure (only when core feature is enabled)
    #[cfg(feature = "core")]
    pub change_detection: Arc<ChangeDetection>,
    #[cfg(feature = "core")]
    pub reactive_polling: Arc<ReactivePollingService>,
    #[cfg(feature = "core")]
    pub reactive_state: Arc<ReactiveAppStateService>,

    // Roam RPC services (for cross-process communication)
    /// Roam-based transport service for RPC communication
    #[cfg(feature = "core")]
    pub roam_transport: Arc<ReaperTransport>,

    // SHM Extension Host (manages guest extension processes)
    // Wrapped in RefCell to allow initialization in initialize() method
    extension_host: std::cell::RefCell<Option<ExtensionHost>>,

    // Tokio runtime for async operations (SHM extension host)
    _tokio_runtime: Option<tokio::runtime::Runtime>,

    // Keep session alive (wrapped in RefCell for interior mutability)
    _session: std::cell::RefCell<ReaperSession>,
}

impl std::fmt::Debug for App {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("App")
            .field("action_registry", &self.action_registry)
            .field("extension_host", &self.extension_host)
            .field("_tokio_runtime", &self._tokio_runtime.is_some())
            .finish()
    }
}

impl App {
    /// Create a new application instance
    pub fn new(session: ReaperSession) -> Result<Self, Box<dyn Error>> {
        // Initialize infrastructure (always needed for menu)
        let action_registry = Arc::new(ActionRegistry::new()?);

        // Create Tokio runtime for async operations (SHM extension host)
        info!("Creating Tokio runtime for extension host...");
        let tokio_runtime = tokio::runtime::Runtime::new()
            .map_err(|e| format!("Failed to create Tokio runtime: {}", e))?;
        info!("✅ Tokio runtime created");

        #[cfg(feature = "core")]
        {
            // Initialize services (only when core feature is enabled)
            let setlist_service = Arc::new(SetlistService::new());
            let command_service = Arc::new(CommandService::new());
            let seek_service = Arc::new(SeekService::new());
            let smooth_seek_service = Arc::new(SmoothSeekService::new());
            let stream_service = Arc::new(StreamService::new(
                setlist_service.clone(),
                command_service.clone(),
                seek_service.clone(),
            ));

            // Initialize reactive infrastructure
            let change_detection = Arc::new(ChangeDetection::new()?);
            let reactive_polling = Arc::new(ReactivePollingService::new());
            let reactive_state = Arc::new(ReactiveAppStateService::new_with_reaper(
                setlist_service.clone(),
            ));

            // Initialize roam RPC services
            let roam_transport = Arc::new(ReaperTransport::new());

            // Note: SHM Extension Host initialization is deferred to initialize()
            // because we need REAPER to be fully available first

            Ok(Self {
                setlist_service,
                command_service,
                seek_service,
                smooth_seek_service,
                stream_service,
                action_registry,
                change_detection,
                reactive_polling,
                reactive_state,
                roam_transport,
                extension_host: std::cell::RefCell::new(None),  // Will be initialized in initialize()
                _tokio_runtime: Some(tokio_runtime),
                _session: std::cell::RefCell::new(session),
            })
        }

        #[cfg(not(feature = "core"))]
        {
            // Note: SHM Extension Host initialization is deferred to initialize()
            // Minimal app with only action registry (for menu)
            Ok(Self {
                action_registry,
                extension_host: std::cell::RefCell::new(None),  // Will be initialized in initialize()
                _tokio_runtime: Some(tokio_runtime),
                _session: std::cell::RefCell::new(session),
            })
        }
    }

    /// Initialize the application (register actions, etc.)
    pub fn initialize(&self) -> Result<(), Box<dyn Error>> {
        // Register actions
        self.action_registry.register_all()?;

        // Register live tracks actions (if live feature is enabled)
        #[cfg(feature = "live")]
        {
            crate::live::tracks::actions::register_all_actions();
        }

        // Register lyrics actions (if lyrics feature is enabled)
        #[cfg(feature = "lyrics")]
        {
            crate::lyrics::register_lyrics_actions();
        }

        // Register lyrics dev actions (if dev feature is enabled)
        #[cfg(feature = "dev")]
        {
            #[cfg(feature = "lyrics")]
            {
                crate::lyrics::register_lyrics_dev_actions();
            }
        }

        // Set track reactive service in change detection (only when core feature is enabled)
        #[cfg(feature = "core")]
        {
            // We know it's always ReaperTrackReactiveService, so we can create a new one with the same streams
            let track_streams = self.reactive_state.track_service.streams().clone();
            let track_service =
                std::sync::Arc::new(fts::daw_reactive::tracks::ReaperTrackReactiveService::new(
                    track_streams,
                    self.setlist_service.clone(),
                ));
            self.change_detection.set_track_service(track_service);

            // Register change detection (needs session)
            self.change_detection.register(&mut self.session_mut())?;
        }

        // Reactive polling logger is deferred until first project is loaded
        // (will be initialized in setlist_service when first update completes)

        // Register extension menu (must be after actions are registered and REAPER is woken up)
        if let Err(e) = crate::infrastructure::menu::register_extension_menu() {
            tracing::warn!("Failed to register extension menu: {:#}", e);
        }

        // Start IROH server for IPC with desktop app (if iroh_server feature is enabled)
        #[cfg(feature = "iroh_server")]
        {
            #[cfg(feature = "core")]
            {
                crate::infrastructure::iroh_server::start_iroh_server(
                    self.stream_service.clone(),
                    self.reactive_state.clone(),
                    self.setlist_service.clone(),
                );
            }
        }

        // Register FTS-Input handler (if input feature is enabled)
        // Note: Handler is registered but disabled by default - user must enable it via action
        #[cfg(feature = "input")]
        {
            // Don't register by default - handler will be registered when user enables FTS-input
            // This ensures the handler is completely transparent when FTS-input is off
            // The handler will be registered when the user toggles FTS-input on

            // Add workflow selector toolbar button to Floating toolbar 32
            // This button shows the current keybind preset and allows switching via popup menu
            crate::infrastructure::workflow_selector::add_toolbar_button();
        }

        // Initialize SHM Extension Host (now that REAPER is available)
        info!("Initializing SHM Extension Host...");
        let session_ref = self._session.borrow();
        let reaper = session_ref.reaper();
        let runtime_handle = self._tokio_runtime.as_ref()
            .expect("Tokio runtime not initialized")
            .handle();
        if let Some(mut host) = Self::initialize_extension_host(&reaper, runtime_handle) {
            // Load plugins
            Self::load_plugins(&mut host);

            // Start watching the plugin directory for changes
            if let Err(e) = host.start_watching() {
                error!("❌ Failed to start plugin directory watcher: {:#}", e);
            }

            // Drop the session borrow before borrowing extension_host mutably
            drop(session_ref);

            // Store the initialized host using RefCell's API
            *self.extension_host.borrow_mut() = Some(host);

            info!("✅ SHM Extension Host initialized and stored");
        } else {
            error!("❌ SHM Extension Host initialization failed");
        }

        Ok(())
    }

    /// Get a mutable reference to the session (for timer registration, etc.)
    pub fn session_mut(&self) -> std::cell::RefMut<'_, ReaperSession> {
        self._session.borrow_mut()
    }

    /// Process pending plugin operations (hot-reload events from watcher thread)
    pub fn process_plugin_operations(&self) {
        if let Some(ref mut host) = *self.extension_host.borrow_mut() {
            host.process_operations();
        }
    }

    /// Initialize the SHM Extension Host
    fn initialize_extension_host(reaper: &reaper_medium::Reaper, runtime_handle: &tokio::runtime::Handle) -> Option<ExtensionHost> {
        info!("Initializing SHM Extension Host");

        // Get plugin directory path
        let plugin_dir = Self::get_plugin_directory(reaper);
        let shm_path = Self::get_shm_path();

        info!("  Plugin directory: {}", plugin_dir.display());
        info!("  SHM path: {}", shm_path.display());

        // Create plugin host
        match ExtensionHost::new(&shm_path, &plugin_dir, runtime_handle) {
            Ok(host) => {
                info!("✅ Extension Host created successfully");
                Some(host)
            }
            Err(e) => {
                error!("❌ Failed to initialize Extension Host: {:#}", e);
                None
            }
        }
    }

    /// Load plugins from the plugin directory
    fn load_plugins(extension_host: &mut ExtensionHost) {
        info!("Loading SHM plugins...");

        // Try to discover plugins from Extensions/FTS directory
        match extension_host.discover_extensions() {
            Ok(()) => {
                let plugin_count = extension_host.list_extensions().len();
                if plugin_count > 0 {
                    info!("✅ Discovered and loaded {} plugin(s)", plugin_count);
                } else {
                    info!("ℹ No plugins found in Extensions/FTS directory");

                    // For development: try loading hello-world from target/debug
                    Self::try_load_dev_plugin(extension_host);
                }
            }
            Err(e) => {
                error!("❌ Failed to discover plugins: {:#}", e);

                // For development: try loading hello-world from target/debug
                Self::try_load_dev_plugin(extension_host);
            }
        }
    }

    /// Try to load development extensions from target/debug (for development)
    fn try_load_dev_plugin(extension_host: &mut ExtensionHost) {
        info!("Looking for development extensions in target/debug...");

        // List of extension binaries to try loading
        let extension_names = &[
            "daw-reaper-extension",
            "daw-test-extension",
            "hello-world-extension",
        ];

        let mut loaded_count = 0;

        for name in extension_names {
            let extension_path = Self::get_dev_extension_path(name);

            if extension_path.exists() {
                info!("  Found {} in target/debug", name);

                match extension_host.load_extension(&extension_path) {
                    Ok(()) => {
                        info!("  ✅ {} loaded successfully", name);
                        loaded_count += 1;
                    }
                    Err(e) => {
                        error!("  ❌ Failed to load {}: {:#}", name, e);
                    }
                }
            } else {
                debug!("  {} not found at: {}", name, extension_path.display());
            }
        }

        if loaded_count == 0 {
            info!("ℹ No development extensions found in target/debug");
            info!("  Build them with: cargo build -p daw-reaper -p daw-test -p hello-world");
            info!("  Or place extensions in: Extensions/FTS/");
        } else {
            info!("✅ Loaded {} development extension(s)", loaded_count);
        }
    }

    /// Get the plugin directory path relative to REAPER's resource directory
    ///
    /// Path structure:
    /// - REAPER resource dir: /Users/.../FastTrackStudio/Reaper/FTS-TRACKS/
    /// - Walk up to:          /Users/.../FastTrackStudio/
    /// - Plugin directory:    /Users/.../FastTrackStudio/Extensions/FTS/
    fn get_plugin_directory(reaper: &reaper_medium::Reaper) -> std::path::PathBuf {
        let plugin_dir = reaper.get_resource_path(|resource_path| {
            info!("REAPER resource path: {}", resource_path);

            // Walk up TWO directories from resource path
            // resource_path: /Users/.../FastTrackStudio/Reaper/FTS-TRACKS/
            // parent:        /Users/.../FastTrackStudio/Reaper/
            // grandparent:   /Users/.../FastTrackStudio/
            let parent = resource_path
                .parent()
                .unwrap_or_else(|| std::path::Path::new("/").try_into().unwrap());

            let grandparent = parent
                .parent()
                .unwrap_or_else(|| std::path::Path::new("/").try_into().unwrap());

            info!("FastTrackStudio root: {}", grandparent);

            // Go to Extensions/FTS
            let plugin_dir = grandparent.join("Extensions").join("FTS");

            info!("Calculated plugin directory: {}", plugin_dir);

            plugin_dir.into_std_path_buf()
        });

        plugin_dir
    }

    /// Get the SHM segment path (unique per REAPER instance)
    fn get_shm_path() -> std::path::PathBuf {
        // Use REAPER's process ID to make SHM path unique per instance
        let pid = std::process::id();
        std::path::PathBuf::from(format!("/tmp/fts-reaper-shm-{}", pid))
    }

    /// Get the path to a development extension binary in target/debug
    fn get_dev_extension_path(name: &str) -> std::path::PathBuf {
        let binary_name = if cfg!(windows) {
            format!("{}.exe", name)
        } else {
            name.to_string()
        };

        std::path::PathBuf::from("target/debug").join(binary_name)
    }
}
