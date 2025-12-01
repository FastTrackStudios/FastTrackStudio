//! Stream Service
//!
//! Creates and manages the setlist stream API.

use std::sync::Arc;
use fts::setlist::SetlistStreamApi;
use crate::core::{ReaperSetlistStateProvider, ReaperSetlistCommandHandler};
use crate::services::{SetlistService, CommandService, SeekService};
use tracing::info;

/// Service for creating and managing the stream API
#[derive(Debug)]
pub struct StreamService {
    setlist_service: Arc<SetlistService>,
    command_service: Arc<CommandService>,
    seek_service: Arc<SeekService>,
}

impl StreamService {
    /// Create a new stream service
    pub fn new(
        setlist_service: Arc<SetlistService>,
        command_service: Arc<CommandService>,
        seek_service: Arc<SeekService>,
    ) -> Self {
        Self {
            setlist_service,
            command_service,
            seek_service,
        }
    }

    /// Create the setlist stream API
    pub fn create_stream_api(&self) -> Result<SetlistStreamApi, Box<dyn std::error::Error>> {
        let state_provider = Arc::new(ReaperSetlistStateProvider::new(
            self.setlist_service.state(),
        ));
        let command_handler = Arc::new(ReaperSetlistCommandHandler::new(
            self.command_service.clone(),
            self.seek_service.clone(),
        ));
        
        let api = SetlistStreamApi::spawn_with_handler(state_provider, Some(command_handler));
        info!("[REAPER Setlist] Setlist stream service created with command handler.");
        Ok(api)
    }
}

