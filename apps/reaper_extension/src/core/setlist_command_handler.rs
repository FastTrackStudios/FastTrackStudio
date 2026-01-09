//! REAPER implementation of SetlistCommandHandler
//!
//! Handles commands from the stream API by delegating to services.

use crate::services::{CommandService, SeekService};
use fts::setlist::infra::stream::LyricsState;
use fts::setlist::{NavigationCommand, SetlistCommandHandler, TransportCommand};
use std::sync::Arc;

/// REAPER implementation of SetlistCommandHandler
pub struct ReaperSetlistCommandHandler {
    /// Command service for executing transport/navigation commands
    command_service: Arc<CommandService>,
    /// Seek service for executing seek operations
    seek_service: Arc<SeekService>,
}

impl ReaperSetlistCommandHandler {
    /// Create a new command handler with references to services
    pub fn new(command_service: Arc<CommandService>, seek_service: Arc<SeekService>) -> Self {
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

    async fn seek_to_musical_position(
        &self,
        song_index: usize,
        musical_position: daw::primitives::MusicalPosition,
    ) -> Result<(), String> {
        self.command_service
            .seek_to_musical_position(song_index, musical_position)
    }

    async fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String> {
        self.command_service.seek_to_time(song_index, time_seconds)
    }

    async fn toggle_loop(&self) -> Result<(), String> {
        self.command_service.toggle_loop()
    }

    async fn advance_syllable(&self) -> Result<LyricsState, String> {
        #[cfg(feature = "lyrics")]
        {
            // Use the lyrics stream handler implementation
            use fts::lyrics::infra::reaper::stream::ReaperLyricsCommandHandler;
            let handler = ReaperLyricsCommandHandler::new();
            handler.advance_syllable().await
        }
        #[cfg(not(feature = "lyrics"))]
        {
            Err("Lyrics feature not enabled".to_string())
        }
    }

    async fn get_lyrics_state(&self) -> Result<LyricsState, String> {
        #[cfg(feature = "lyrics")]
        {
            // Use the lyrics stream handler implementation
            use fts::lyrics::infra::reaper::stream::ReaperLyricsCommandHandler;
            let handler = ReaperLyricsCommandHandler::new();
            handler.get_lyrics_state().await
        }
        #[cfg(not(feature = "lyrics"))]
        {
            Err("Lyrics feature not enabled".to_string())
        }
    }

    async fn assign_syllable_to_note(&self, syllable_text: String) -> Result<(), String> {
        #[cfg(feature = "lyrics")]
        {
            // Use the lyrics stream handler implementation
            use fts::lyrics::infra::reaper::stream::ReaperLyricsCommandHandler;
            let handler = ReaperLyricsCommandHandler::new();
            handler.assign_syllable_to_note(syllable_text).await
        }
        #[cfg(not(feature = "lyrics"))]
        {
            Err("Lyrics feature not enabled".to_string())
        }
    }

    async fn update_lyrics(
        &self,
        song_index: usize,
        lyrics: fts::lyrics::core::Lyrics,
    ) -> Result<(), String> {
        #[cfg(feature = "lyrics")]
        {
            // Update lyrics using the trait method
            use fts::lyrics::infra::traits::LyricsWriter;
            use fts::setlist::infra::traits::SetlistBuilder;
            use reaper_high::Reaper;

            // Get the project for this song by building setlist and finding the project
            let reaper = Reaper::get();
            let setlist = reaper
                .build_setlist_from_open_projects(None)
                .map_err(|e| format!("Failed to build setlist: {}", e))?;

            if song_index >= setlist.songs.len() {
                return Err(format!("Song index {} out of range", song_index));
            }

            // Find the project for this song
            let song = &setlist.songs[song_index];
            let project_name = song.project_name_from_metadata();
            let medium_reaper = reaper.medium_reaper();

            let mut target_project = None;
            for i in 0..128u32 {
                if let Some(result) =
                    medium_reaper.enum_projects(reaper_medium::ProjectRef::Tab(i), 512)
                {
                    let tab_name = result
                        .file_path
                        .as_ref()
                        .and_then(|p| p.as_std_path().file_stem())
                        .and_then(|s| s.to_str())
                        .map(|s| s.to_string())
                        .unwrap_or_else(|| format!("Tab {}", i));

                    let normalized_tab = tab_name.to_uppercase().replace('_', "-");
                    let normalized_target = project_name.to_uppercase().replace('_', "-");

                    if normalized_tab == normalized_target {
                        target_project = Some(reaper_high::Project::new(result.project));
                        break;
                    }
                }
            }

            let project = target_project
                .ok_or_else(|| format!("Could not find project for song: {}", project_name))?;

            // Use the trait method
            project
                .update_lyrics_in_project(song_index, lyrics)
                .map_err(|e| format!("Failed to update lyrics: {}", e))
        }
        #[cfg(not(feature = "lyrics"))]
        {
            Err("Lyrics feature not enabled".to_string())
        }
    }
}
