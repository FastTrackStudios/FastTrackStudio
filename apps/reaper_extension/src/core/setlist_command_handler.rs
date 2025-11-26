//! REAPER implementation of SetlistCommandHandler
//!
//! Handles commands from the stream API by delegating to services.

use async_trait::async_trait;
use std::sync::Arc;
use setlist::{SetlistCommandHandler, TransportCommand, NavigationCommand};
use setlist::infra::stream::LyricsState;
use crate::services::{CommandService, SeekService};

/// REAPER implementation of SetlistCommandHandler
pub struct ReaperSetlistCommandHandler {
    /// Command service for executing transport/navigation commands
    command_service: Arc<CommandService>,
    /// Seek service for executing seek operations
    seek_service: Arc<SeekService>,
}

impl ReaperSetlistCommandHandler {
    /// Create a new command handler with references to services
    pub fn new(
        command_service: Arc<CommandService>,
        seek_service: Arc<SeekService>,
    ) -> Self {
        Self {
            command_service,
            seek_service,
        }
    }
}

#[async_trait::async_trait]
impl SetlistCommandHandler for ReaperSetlistCommandHandler {
    async fn execute_transport_command(&self, command: TransportCommand) -> Result<(), String> {
        self.command_service.execute_transport_command(command)
    }
    
    async fn execute_navigation_command(&self, command: NavigationCommand) -> Result<(), String> {
        self.command_service.execute_navigation_command(command)
    }
    
    async fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String> {
        self.seek_service.seek_to_section(song_index, section_index)
    }
    
    async fn seek_to_song(&self, song_index: usize) -> Result<(), String> {
        self.command_service.seek_to_song(song_index)
    }
    
    async fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String> {
        self.command_service.seek_to_time(song_index, time_seconds)
    }
    
    async fn toggle_loop(&self) -> Result<(), String> {
        self.command_service.toggle_loop()
    }
    
    async fn advance_syllable(&self) -> Result<LyricsState, String> {
        // Use the lyrics stream handler implementation
        use crate::lyrics::stream::ReaperLyricsCommandHandler;
        let handler = ReaperLyricsCommandHandler::new();
        handler.advance_syllable().await
    }
    
    async fn get_lyrics_state(&self) -> Result<LyricsState, String> {
        // Use the lyrics stream handler implementation
        use crate::lyrics::stream::ReaperLyricsCommandHandler;
        let handler = ReaperLyricsCommandHandler::new();
        handler.get_lyrics_state().await
    }
    
    async fn assign_syllable_to_note(&self, syllable_text: String) -> Result<(), String> {
        // Use the lyrics stream handler implementation
        use crate::lyrics::stream::ReaperLyricsCommandHandler;
        let handler = ReaperLyricsCommandHandler::new();
        handler.assign_syllable_to_note(syllable_text).await
    }
    
    async fn update_lyrics(&self, song_index: usize, lyrics: lyrics::core::Lyrics) -> Result<(), String> {
        // Update lyrics in REAPER project
        use crate::lyrics::write::update_lyrics_in_reaper;
        update_lyrics_in_reaper(song_index, lyrics)
    }
}

