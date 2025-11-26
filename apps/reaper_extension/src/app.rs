//! Application Container
//!
//! Manages initialization and provides access to all services.

use std::sync::Arc;
use std::error::Error;
use reaper_medium::ReaperSession;
use tracing::info;

use crate::services::{SetlistService, CommandService, SeekService, StreamService, SmoothSeekService};
use crate::infrastructure::action_registry::ActionRegistry;
use crate::infrastructure::change_detection::ChangeDetection;

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
    
    // Keep session alive (wrapped in RefCell for interior mutability)
    _session: std::cell::RefCell<ReaperSession>,
}

impl App {
    /// Create a new application instance
    pub fn new(session: ReaperSession) -> Result<Self, Box<dyn Error>> {
        info!("Creating application container...");
        
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
        
        Ok(Self {
            setlist_service,
            command_service,
            seek_service,
            smooth_seek_service,
            stream_service,
            action_registry,
            change_detection,
            _session: std::cell::RefCell::new(session),
        })
    }
    
    /// Initialize the application (register actions, etc.)
    pub fn initialize(&self) -> Result<(), Box<dyn Error>> {
        info!("Initializing application...");
        
        // Register actions
        self.action_registry.register_all()?;
        
        // Register live tracks actions
        crate::live::tracks::actions::register_all_actions();
        
        // Register lyrics actions
        crate::lyrics::register_lyrics_actions();
        crate::lyrics::register_lyrics_dev_actions();
        
        // Register change detection (needs session)
        self.change_detection.register(&mut self.session_mut())?;
        
        // Register extension menu (must be after actions are registered and REAPER is woken up)
        info!("🔧 About to register extension menu...");
        if let Err(e) = crate::infrastructure::menu::register_extension_menu() {
            tracing::warn!("Failed to register extension menu: {:#}", e);
        } else {
            info!("FastTrackStudio menu registered successfully");
        }
        
        // Start IROH server for IPC with desktop app
        crate::infrastructure::iroh_server::start_iroh_server(self.stream_service.clone());
        
        info!("Application initialized successfully");
        Ok(())
    }
    
    /// Get a mutable reference to the session (for timer registration, etc.)
    pub fn session_mut(&self) -> std::cell::RefMut<ReaperSession> {
        self._session.borrow_mut()
    }
}

