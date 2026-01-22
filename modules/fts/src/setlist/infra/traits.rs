//! Trait definitions for setlist infrastructure
//!
//! These traits define abstract interfaces for DAW-agnostic setlist operations.
//! Implementations are provided in the `reaper` module behind a feature flag.

use super::commands::{LyricsState, NavigationCommand, TransportCommand};
use crate::setlist::core::{Setlist, SetlistApi, SetlistError, Song};

#[cfg(not(target_arch = "wasm32"))]
use async_trait::async_trait;

/// Trait for building setlists from DAW projects
/// This is implemented on the DAW instance (e.g., Reaper) since it operates across all projects
pub trait SetlistBuilder {
    /// Build a setlist from all open projects
    /// This iterates through all open project tabs and builds a setlist from them
    fn build_setlist_from_open_projects(
        &self,
        existing_setlist: Option<&Setlist>,
    ) -> Result<Setlist, SetlistError>;

    /// Build a song from the current active project
    fn build_song_from_current_project(&self) -> Result<Song, SetlistError>;
}

/// Trait for seeking operations in a DAW
pub trait SeekAdapter {
    /// Seek to a section within a song
    fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String>;

    /// Seek to a song
    fn seek_to_song(&self, song_index: usize) -> Result<(), String>;

    /// Seek to a time position within a song
    fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String>;

    /// Seek to a musical position within a song
    fn seek_to_musical_position(
        &self,
        song_index: usize,
        musical_position: daw::primitives::MusicalPosition,
    ) -> Result<(), String>;
}

/// Trait for command execution in a DAW (sync version)
pub trait CommandAdapter {
    /// Execute a transport command
    fn execute_transport_command(&self, command: TransportCommand) -> Result<(), String>;

    /// Execute a navigation command
    fn execute_navigation_command(&self, command: NavigationCommand) -> Result<(), String>;

    /// Toggle loop
    fn toggle_loop(&self) -> Result<(), String>;
}

/// Trait for backends that can provide setlist state (async version)
///
/// This allows the setlist stream actor to get setlist state
/// from different sources (REAPER, other DAWs, etc.)
#[cfg(not(target_arch = "wasm32"))]
#[async_trait]
pub trait SetlistStateProvider: Send + Sync {
    /// Get the current setlist API state (includes computed fields like active song)
    async fn get_setlist_api(&self) -> Result<SetlistApi, String>;
}

/// Trait for backends that can handle setlist commands (async version)
///
/// This allows the setlist stream actor to execute commands
/// on different backends (REAPER, other DAWs, etc.)
#[cfg(not(target_arch = "wasm32"))]
#[async_trait]
pub trait SetlistCommandHandler: Send + Sync {
    /// Execute a transport command
    async fn execute_transport_command(&self, command: TransportCommand) -> Result<(), String>;

    /// Execute a navigation command
    async fn execute_navigation_command(&self, command: NavigationCommand) -> Result<(), String>;

    /// Seek to a specific section
    async fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String>;

    /// Seek to a specific song (switches to that song's tab and moves cursor to beginning)
    async fn seek_to_song(&self, song_index: usize) -> Result<(), String>;

    /// Seek to a specific time position within a song
    async fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String>;

    /// Seek to a specific musical position within a song
    async fn seek_to_musical_position(
        &self,
        song_index: usize,
        musical_position: daw::primitives::MusicalPosition,
    ) -> Result<(), String>;

    /// Toggle loop for current song
    async fn toggle_loop(&self) -> Result<(), String>;

    /// Advance to the next syllable and assign it to the next MIDI note at edit cursor
    async fn advance_syllable(&self) -> Result<LyricsState, String>;

    /// Get current lyrics state
    async fn get_lyrics_state(&self) -> Result<LyricsState, String>;

    /// Assign a specific syllable to the MIDI note at edit cursor position
    async fn assign_syllable_to_note(&self, syllable_text: String) -> Result<(), String>;

    /// Update lyrics for a song
    async fn update_lyrics(
        &self,
        song_index: usize,
        lyrics: crate::lyrics::core::Lyrics,
    ) -> Result<(), String>;
}
