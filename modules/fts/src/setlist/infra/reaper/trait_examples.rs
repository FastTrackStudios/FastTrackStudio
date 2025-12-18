//! Examples of implementing traits for ReaperProject
//!
//! This file demonstrates two approaches to implementing traits on external types.
//! It's for reference only - not compiled into the build.

use reaper_high::Project as ReaperProject;
use crate::setlist::core::{Setlist, SetlistError, Song};
use crate::setlist::infra::traits::{SetlistBuilder, SeekAdapter, CommandAdapter};

// ============================================================================
// APPROACH 1: Newtype Pattern (Recommended)
// ============================================================================
// Wrap ReaperProject in your own type, then implement traits on the wrapper.
// This gives you a clean API where you can call methods directly.

/// Wrapper around ReaperProject that implements setlist traits
pub struct ReaperProjectWrapper {
    inner: ReaperProject,
}

impl ReaperProjectWrapper {
    /// Create a new wrapper from a ReaperProject
    pub fn new(project: ReaperProject) -> Self {
        Self { inner: project }
    }
    
    /// Get access to the inner ReaperProject if needed
    pub fn inner(&self) -> &ReaperProject {
        &self.inner
    }
    
    /// Consume the wrapper and return the inner ReaperProject
    pub fn into_inner(self) -> ReaperProject {
        self.inner
    }
}

// Now implement your traits on YOUR type (this is allowed!)
impl SetlistBuilder for ReaperProjectWrapper {
    fn build_setlist_from_open_projects(
        &self,
        existing_setlist: Option<&Setlist>,
    ) -> Result<Setlist, SetlistError> {
        // Access ReaperProject via self.inner
        // Build your setlist logic here
        todo!("Implement setlist building")
    }
    
    fn build_song_from_current_project(&self) -> Result<Song, SetlistError> {
        // Build song from self.inner
        todo!("Implement song building")
    }
}

impl SeekAdapter for ReaperProjectWrapper {
    fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String> {
        // Use self.inner to seek
        todo!("Implement seek to section")
    }
    
    fn seek_to_song(&self, song_index: usize) -> Result<(), String> {
        todo!("Implement seek to song")
    }
    
    fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String> {
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

impl CommandAdapter for ReaperProjectWrapper {
    fn execute_transport_command(&self, command: crate::setlist::TransportCommand) -> Result<(), String> {
        todo!("Implement transport command")
    }
    
    fn execute_navigation_command(&self, command: crate::setlist::NavigationCommand) -> Result<(), String> {
        todo!("Implement navigation command")
    }
    
    fn toggle_loop(&self) -> Result<(), String> {
        todo!("Implement toggle loop")
    }
}

// Usage example:
// ```
// use reaper_high::Reaper;
// 
// let reaper = Reaper::get();
// let reaper_project = reaper.current_project();
// let project = ReaperProjectWrapper::new(reaper_project);
// 
// // Now you can call trait methods directly!
// let setlist = project.build_setlist_from_open_projects(None)?;
// project.seek_to_song(0)?;
// project.execute_transport_command(TransportCommand::Play)?;
// ```

// ============================================================================
// APPROACH 2: Extension Trait Pattern
// ============================================================================
// Create traits with methods that take &ReaperProject as self.
// You can implement these for the external type directly!

/// Extension trait for ReaperProject that provides setlist operations
pub trait ReaperProjectSetlist {
    fn build_setlist(&self, existing_setlist: Option<&Setlist>) -> Result<Setlist, SetlistError>;
    fn build_song(&self) -> Result<Song, SetlistError>;
}

/// Extension trait for ReaperProject that provides seeking operations
pub trait ReaperProjectSeek {
    fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String>;
    fn seek_to_song(&self, song_index: usize) -> Result<(), String>;
    fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String>;
    fn seek_to_musical_position(
        &self,
        song_index: usize,
        musical_position: daw::primitives::MusicalPosition,
    ) -> Result<(), String>;
}

/// Extension trait for ReaperProject that provides command operations
pub trait ReaperProjectCommand {
    fn execute_transport_command(&self, command: crate::setlist::TransportCommand) -> Result<(), String>;
    fn execute_navigation_command(&self, command: crate::setlist::NavigationCommand) -> Result<(), String>;
    fn toggle_loop(&self) -> Result<(), String>;
}

// Implement for ReaperProject (this IS allowed - you own the trait!)
impl ReaperProjectSetlist for ReaperProject {
    fn build_setlist(&self, existing_setlist: Option<&Setlist>) -> Result<Setlist, SetlistError> {
        // Build setlist using self (which is &ReaperProject)
        todo!("Implement setlist building")
    }
    
    fn build_song(&self) -> Result<Song, SetlistError> {
        todo!("Implement song building")
    }
}

impl ReaperProjectSeek for ReaperProject {
    fn seek_to_section(&self, song_index: usize, section_index: usize) -> Result<(), String> {
        todo!("Implement seek to section")
    }
    
    fn seek_to_song(&self, song_index: usize) -> Result<(), String> {
        todo!("Implement seek to song")
    }
    
    fn seek_to_time(&self, song_index: usize, time_seconds: f64) -> Result<(), String> {
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

impl ReaperProjectCommand for ReaperProject {
    fn execute_transport_command(&self, command: crate::setlist::TransportCommand) -> Result<(), String> {
        todo!("Implement transport command")
    }
    
    fn execute_navigation_command(&self, command: crate::setlist::NavigationCommand) -> Result<(), String> {
        todo!("Implement navigation command")
    }
    
    fn toggle_loop(&self) -> Result<(), String> {
        todo!("Implement toggle loop")
    }
}

// Usage example:
// ```
// use reaper_high::Reaper;
// use crate::setlist::infra::reaper::trait_examples::{ReaperProjectSetlist, ReaperProjectSeek, ReaperProjectCommand};
// 
// let reaper = Reaper::get();
// let project = reaper.current_project();
// 
// // Import the traits to use the methods
// // Now you can call methods directly on ReaperProject!
// let setlist = project.build_setlist(None)?;
// project.seek_to_song(0)?;
// project.execute_transport_command(TransportCommand::Play)?;
// ```

// ============================================================================
// COMPARISON
// ============================================================================
//
// Newtype Pattern (Approach 1):
//   ✅ Clean API - methods feel native
//   ✅ Type-safe wrapper
//   ✅ Can add additional state/methods
//   ❌ Must wrap ReaperProject
//   ❌ Need to unwrap to access original methods
//
// Extension Trait Pattern (Approach 2):
//   ✅ Use ReaperProject directly
//   ✅ No wrapping needed
//   ❌ Must import traits to use methods
//   ❌ Method names might conflict
//   ❌ Less "native API" feeling
//
// Recommendation: Use Newtype if you want the "native API" feel.
//                  Use Extension Trait if you want to use ReaperProject directly.
