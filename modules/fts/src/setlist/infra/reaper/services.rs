//! REAPER implementations of service traits
//!
//! This module provides REAPER-specific implementations of the service traits
//! defined in the traits module. These implementations will be used by the
//! services in reaper_extension.
//!
//! NOTE: This is a legacy implementation. The preferred approach is to use
//! the direct trait implementations on `Reaper` and `Project` types in `trait_impls.rs`.

use crate::setlist::infra::traits::SetlistBuilder;
use crate::setlist::core::{Setlist, SetlistError, Song};
use super::song_builder::{build_setlist_from_open_projects, build_song_from_current_project};

/// REAPER implementation of SetlistBuilder
/// This is a legacy wrapper - prefer using the trait directly on Reaper instances
pub struct ReaperSetlistBuilder;

impl ReaperSetlistBuilder {
    pub fn new() -> Self {
        Self
    }
}

impl SetlistBuilder for ReaperSetlistBuilder {
    fn build_setlist_from_open_projects(
        &self,
        existing_setlist: Option<&Setlist>,
    ) -> Result<Setlist, SetlistError> {
        build_setlist_from_open_projects(existing_setlist)
    }
    
    fn build_song_from_current_project(&self) -> Result<Song, SetlistError> {
        build_song_from_current_project()
    }
}

// Note: SeekAdapter and CommandAdapter implementations will be provided
// by the services in reaper_extension that use these building functions.
// The actual seek and command operations require access to REAPER's main thread
// and are handled by the services layer.
