//! FastTrackStudio App (Minimal)
//!
//! Core application state with minimal dependencies.

use reaper_high::PluginContext;
use std::sync::Arc;

use crate::command_executor::CommandExecutor;
use crate::services::{SetlistService, StreamService, TransportService};

/// Main application state
pub struct App {
    /// Command executor for REAPER API calls
    pub command_executor: CommandExecutor,

    /// Transport service
    pub transport_service: TransportService,

    /// Setlist service (optional for now)
    pub setlist_service: Option<SetlistService>,

    /// Stream service (optional for now)
    pub stream_service: Option<StreamService>,
}

impl App {
    /// Create a new app instance
    pub fn new(_context: PluginContext) -> Arc<Self> {
        // Create command executor
        let command_executor = CommandExecutor::default();

        // Create transport service
        let transport_service = TransportService::new(command_executor.clone());

        // Create optional services (can be None for now)
        let setlist_service = None;
        let stream_service = None;

        Arc::new(Self {
            command_executor,
            transport_service,
            setlist_service,
            stream_service,
        })
    }
}

impl std::fmt::Debug for App {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("App")
            .field("transport_service", &self.transport_service)
            .finish()
    }
}
