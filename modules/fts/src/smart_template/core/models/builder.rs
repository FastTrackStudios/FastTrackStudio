use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::GroupMode;
use crate::smart_template::utils::track_helpers::{create_track, TrackExt};
use daw::tracks::Track;

/// A builder for creating Template structures declaratively
pub struct TemplateBuilder {
    name: String,
    tracks: Vec<Track>,
    parent_stack: Vec<String>,
}

impl TemplateBuilder {
    /// Create a new TemplateBuilder with the given template name
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            tracks: Vec::new(),
            parent_stack: Vec::new(),
        }
    }

    /// Add a BUS track and set it as the current parent for subsequent tracks
    pub fn bus(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        let parent = self.parent_stack.last().cloned();
        self.tracks.push(create_track(&name_str, Some("BUS"), parent.as_deref(), &[]));
        self.parent_stack.push(name_str);
        self
    }

    /// Add a SUM track and set it as the current parent for subsequent tracks
    pub fn sum(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        let parent = self.parent_stack.last().cloned();
        self.tracks.push(create_track(&name_str, Some("SUM"), parent.as_deref(), &[]));
        self.parent_stack.push(name_str);
        self
    }

    /// Add a regular track under the current parent
    pub fn track(mut self, name: impl Into<String>) -> Self {
        let name_str = name.into();
        let parent = self.parent_stack.last().cloned();
        self.tracks.push(create_track(&name_str, None, parent.as_deref(), &[]));
        self
    }

    /// Set visibility modes for the last added track
    pub fn modes(mut self, modes: &[GroupMode]) -> Self {
        if let Some(track) = self.tracks.last_mut() {
            track.set_modes(modes);
        }
        self
    }
    
    /// Set the track type for the last added track
    pub fn with_type(mut self, track_type: &str) -> Self {
        if let Some(track) = self.tracks.last_mut() {
            track.set_track_type(track_type);
        }
        self
    }

    /// Go up one level in the hierarchy
    pub fn end(mut self) -> Self {
        self.parent_stack.pop();
        self
    }

    /// Build the Template
    pub fn build(self) -> Template {
        Template {
            name: self.name.into(),
            tracks: self.tracks,
        }
    }
}
