//! Application Container
//!
//! Manages initialization and provides access to all services.

use std::sync::Arc;
use std::error::Error;
use reaper_medium::ReaperSession;

use crate::services::{SetlistService, CommandService, SeekService, StreamService, SmoothSeekService};
use crate::infrastructure::action_registry::ActionRegistry;
use crate::infrastructure::change_detection::ChangeDetection;
use crate::infrastructure::reactive_polling::ReactivePollingService;
use crate::infrastructure::reactive_app_state::ReactiveAppStateService;

/// Application container - holds all services and manages initialization
#[derive(Debug)]
pub struct App {
    // Services
    pub setlist_service: Arc<SetlistService>,
    pub command_service: Arc<CommandService>,
    pub seek_service: Arc<SeekService>,
    pub stream_service: Arc<StreamService>,
    pub smooth_seek_service: Arc<SmoothSeekService>,
    
    // Infrastructure
    pub action_registry: Arc<ActionRegistry>,
    pub change_detection: Arc<ChangeDetection>,
    pub reactive_polling: Arc<ReactivePollingService>,
    pub reactive_state: Arc<ReactiveAppStateService>,
    
    // Keep session alive (wrapped in RefCell for interior mutability)
    _session: std::cell::RefCell<ReaperSession>,
}

impl App {
    /// Create a new application instance
    pub fn new(session: ReaperSession) -> Result<Self, Box<dyn Error>> {
        // Initialize services
        let setlist_service = Arc::new(SetlistService::new());
        let command_service = Arc::new(CommandService::new());
        let seek_service = Arc::new(SeekService::new());
        let smooth_seek_service = Arc::new(SmoothSeekService::new());
        let stream_service = Arc::new(StreamService::new(
            setlist_service.clone(),
            command_service.clone(),
            seek_service.clone(),
        ));
        
        // Initialize infrastructure
        let action_registry = Arc::new(ActionRegistry::new()?);
        let change_detection = Arc::new(ChangeDetection::new()?);
        let reactive_polling = Arc::new(ReactivePollingService::new());
        let reactive_state = Arc::new(ReactiveAppStateService::new_with_reaper(
            setlist_service.clone(),
        ));
        
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
            _session: std::cell::RefCell::new(session),
        })
    }
    
    /// Initialize the application (register actions, etc.)
    pub fn initialize(&self) -> Result<(), Box<dyn Error>> {
        // Register actions
        self.action_registry.register_all()?;
        
        // Register live tracks actions
        crate::live::tracks::actions::register_all_actions();
        
        // Register lyrics actions
        crate::lyrics::register_lyrics_actions();
        crate::lyrics::register_lyrics_dev_actions();
        
        // Set track reactive service in change detection
        // We know it's always ReaperTrackReactiveService, so we can create a new one with the same streams
        let track_streams = self.reactive_state.track_service.streams().clone();
        let track_service = std::sync::Arc::new(
            crate::infrastructure::reaper_track_reactive::ReaperTrackReactiveService::new(
                track_streams,
                self.setlist_service.clone(),
            )
        );
        self.change_detection.set_track_service(track_service);
        
        // Register change detection (needs session)
        self.change_detection.register(&mut self.session_mut())?;
        
        // Reactive polling logger is deferred until first project is loaded
        // (will be initialized in setlist_service when first update completes)
        
        // Register extension menu (must be after actions are registered and REAPER is woken up)
        if let Err(e) = crate::infrastructure::menu::register_extension_menu() {
            tracing::warn!("Failed to register extension menu: {:#}", e);
        }
        
        // Start IROH server for IPC with desktop app
        crate::infrastructure::iroh_server::start_iroh_server(
            self.stream_service.clone(),
            self.reactive_state.clone(),
            self.setlist_service.clone(),
        );
        Ok(())
    }
    
    /// Get a mutable reference to the session (for timer registration, etc.)
    pub fn session_mut(&self) -> std::cell::RefMut<'_, ReaperSession> {
        self._session.borrow_mut()
    }
}

