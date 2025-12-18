//! Direct trait implementations for ReaperProject
//!
//! Since we own the traits (SetlistBuilder, SeekAdapter, CommandAdapter),
//! we CAN implement them directly for ReaperProject from reaper-rs!
//!
//! This is allowed by Rust's orphan rule: you can implement a trait for a type
//! if you own EITHER the trait OR the type. Since we own the traits, we're good!

use reaper_high::Project as ReaperProject;
use crate::setlist::core::{Setlist, SetlistError, Song};
use crate::setlist::infra::traits::{SetlistBuilder, SeekAdapter, CommandAdapter};

// ============================================================================
// Direct Implementation - This WORKS because we own the traits!
// ============================================================================

impl SetlistBuilder for ReaperProject {
    fn build_setlist_from_open_projects(
        &self,
        existing_setlist: Option<&Setlist>,
    ) -> Result<Setlist, SetlistError> {
        // You can call methods on self (which is &ReaperProject) directly!
        // Use the existing song_builder functions or implement here
        todo!("Implement setlist building using self (ReaperProject)")
    }
    
    fn build_song_from_current_project(&self) -> Result<Song, SetlistError> {
        // Build song from self (ReaperProject)
        todo!("Implement song building using self (ReaperProject)")
    }
}

impl SeekAdapter for ReaperProject {
    fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String> {
        // Use self to seek - self is &ReaperProject
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

impl CommandAdapter for ReaperProject {
    fn execute_transport_command(&self, command: crate::setlist::TransportCommand) -> Result<(), String> {
        // Use self to execute commands - self is &ReaperProject
        // Example: self.play(), self.pause(), etc.
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

// ============================================================================
// Usage Example
// ============================================================================
//
// Now you can use ReaperProject directly with your traits!
//
// ```rust
// use reaper_high::Reaper;
// use crate::setlist::infra::traits::{SetlistBuilder, SeekAdapter, CommandAdapter};
//
// let reaper = Reaper::get();
// let project = reaper.current_project();
//
// // Call trait methods directly on ReaperProject!
// let setlist = project.build_setlist_from_open_projects(None)?;
// project.seek_to_song(0)?;
// project.execute_transport_command(TransportCommand::Play)?;
// ```
//
// This gives you the "native API" feel you wanted - you can call your
// trait methods directly on ReaperProject instances!
