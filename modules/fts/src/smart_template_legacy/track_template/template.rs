//! Template management
//!
//! A Template represents a complete DAW template structure that can be
//! created from scratch or matched against existing tracks.

use std::fmt;
use super::track_list::TrackList;
use super::track::Track;
use super::matcher::TrackMatcher;

/// A template that can be created or matched against
#[derive(Debug, Clone)]
pub struct Template {
    /// The track list for this template
    track_list: TrackList,
    
    /// Template name
    name: String,
}

impl Template {
    /// Create a new empty template
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            track_list: TrackList::new(),
            name: name.into(),
        }
    }
    
    /// Create a template from an existing track list
    pub fn from_track_list(name: impl Into<String>, track_list: TrackList) -> Self {
        Self {
            track_list,
            name: name.into(),
        }
    }
    
    /// Get the template name
    pub fn name(&self) -> &str {
        &self.name
    }
    
    /// Get the track list
    pub fn track_list(&self) -> &TrackList {
        &self.track_list
    }
    
    /// Get a mutable reference to the track list
    pub fn track_list_mut(&mut self) -> &mut TrackList {
        &mut self.track_list
    }
    
    /// Add a track to the template
    pub fn add_track(&mut self, track: Track) {
        self.track_list.add_track(track);
    }
    
    /// Get a track by name
    pub fn get_track(&self, name: &str) -> Option<&Track> {
        self.track_list.get_track(name)
    }
    
    /// Create a matcher for this template
    pub fn create_matcher(&self) -> TrackMatcher {
        TrackMatcher::with_track_list(self.track_list.clone())
    }
}

impl fmt::Display for Template {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Template: {}", self.name)?;
        write!(f, "{}", self.track_list)?;
        Ok(())
    }
}
