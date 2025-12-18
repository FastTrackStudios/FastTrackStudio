//! Trait definitions for setlist infrastructure
//!
//! These traits define abstract interfaces for DAW-agnostic setlist operations.
//! Implementations are provided in the `reaper` module behind a feature flag.

use crate::setlist::core::{Setlist, SetlistError, Song};

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

/// Trait for command execution in a DAW
pub trait CommandAdapter {
    /// Execute a transport command
    fn execute_transport_command(&self, command: crate::setlist::TransportCommand) -> Result<(), String>;
    
    /// Execute a navigation command
    fn execute_navigation_command(&self, command: crate::setlist::NavigationCommand) -> Result<(), String>;
    
    /// Toggle loop
    fn toggle_loop(&self) -> Result<(), String>;
}
