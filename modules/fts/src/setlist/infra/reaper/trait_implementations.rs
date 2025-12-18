//! Direct trait implementations for ReaperProject
//!
//! Since we own the traits (SetlistBuilder, SeekAdapter, CommandAdapter),
//! we CAN implement them directly for ReaperProject from reaper-rs!
//!
//! This allows you to call methods directly on ReaperProject instances:
//! ```rust
//! let project = Reaper::get().current_project();
//! let setlist = project.build_setlist_from_open_projects(None)?;
//! project.seek_to_song(0)?;
//! ```

#[cfg(feature = "reaper")]
use reaper_high::Project as ReaperProject;
use crate::setlist::core::{Setlist, SetlistError, Song};
use crate::setlist::infra::traits::{SetlistBuilder, SeekAdapter, CommandAdapter};

// Note: This file is a template. The actual implementations would need to:
// 1. Import the existing helper functions from your reaper_extension app
// 2. Adapt them to work with &self (ReaperProject) instead of free functions
// 3. Handle the setlist building logic that currently lives in song_builder.rs

#[cfg(feature = "reaper")]
impl SetlistBuilder for ReaperProject {
    fn build_setlist_from_open_projects(
        &self,
        existing_setlist: Option<&Setlist>,
    ) -> Result<Setlist, SetlistError> {
        // You can use self (which is &ReaperProject) directly here!
        // 
        // This would delegate to your existing build_setlist_from_open_projects function,
        // but you'd need to refactor it to work with a specific project or enumerate
        // all projects and find the one matching self.
        //
        // For now, this is a placeholder showing the structure:
        todo!("Implement setlist building using self (ReaperProject)")
    }
    
    fn build_song_from_current_project(&self) -> Result<Song, SetlistError> {
        // Build song from self (ReaperProject)
        // This would use your existing build_song_from_region logic
        todo!("Implement song building using self (ReaperProject)")
    }
}

#[cfg(feature = "reaper")]
impl SeekAdapter for ReaperProject {
    fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String> {
        // Use self to seek - self is &ReaperProject
        // This would use your existing seek logic from seek_service.rs
        todo!("Implement seek to section")
    }
    
    fn seek_to_song(&self, song_index: usize) -> Result<(), String> {
        todo!("Implement seek to song")
    }
    
    fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String> {
        // Example: self.set_edit_cursor_position(...)
        todo!("Implement seek to time")
    }
    
    fn seek_to_musical_position(
        &self,
        song_index: usize,
        musical_position: daw::primitives::MusicalPosition,
    ) -> Result<(), String> {
        todo!("Implement seek to musical position")
    }
}

#[cfg(feature = "reaper")]
impl CommandAdapter for ReaperProject {
    fn execute_transport_command(&self, command: crate::setlist::TransportCommand) -> Result<(), String> {
        // Use self to execute commands - self is &ReaperProject
        // This would use your existing command logic from command_service.rs
        todo!("Implement transport command")
    }
    
    fn execute_navigation_command(&self, command: crate::setlist::NavigationCommand) -> Result<(), String> {
        todo!("Implement navigation command")
    }
    
    fn toggle_loop(&self) -> Result<(), String> {
        // Example: self.set_loop_state(...)
        todo!("Implement toggle loop")
    }
}
