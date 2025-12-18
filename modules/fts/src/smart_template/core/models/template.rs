//! Template core type
//!
//! The Template struct represents a track structure with hierarchical organization.

use crate::smart_template::core::models::GroupMode;
use crate::smart_template::utils::track_helpers::TrackExt;
use daw::tracks::TrackName;

/// A template represents a track structure
#[derive(Debug, Clone)]
pub struct Template {
    /// Template name
    pub name: TrackName,
    
    /// Tracks in this template (hierarchical structure)
    /// Uses daw::Track directly instead of TrackInfo
    pub tracks: Vec<daw::tracks::Track>,
}

impl Template {
    /// Create a new TemplateBuilder for this template
    pub fn builder(name: impl Into<String>) -> super::builder::TemplateBuilder {
        super::builder::TemplateBuilder::new(name)
    }

    /// Filter tracks by group mode
    ///
    /// Returns a new template containing only tracks that belong to the specified mode.
    /// Tracks with no modes specified are included in all modes.
    /// Parent tracks are automatically included if any of their children are included.
    pub fn filter_by_mode(&self, mode: GroupMode) -> Template {
        // First pass: determine which tracks should be included based on mode
        let mut included = std::collections::HashSet::new();
        
        for track in &self.tracks {
            // If track has no modes specified, include it in all modes
            let modes = track.get_modes();
            if modes.is_empty() {
                included.insert(track.name.clone());
            } else if modes.contains(&mode) {
                // Otherwise, only include if it matches the requested mode
                included.insert(track.name.clone());
            }
        }
        
        // Second pass: include parent tracks if any of their children are included
        let mut changed = true;
        while changed {
            changed = false;
            for track in &self.tracks {
                // If this track is already included, skip
                if included.contains(&track.name) {
                    continue;
                }
                
                // Check if any child of this track is included
                let has_included_child = self.tracks.iter().any(|child| {
                    child.parent_name().map(|p| track.name.eq_ignore_case(p)).unwrap_or(false)
                        && included.contains(&child.name)
                });
                
                if has_included_child {
                    included.insert(track.name.clone());
                    changed = true;
                }
            }
        }
        
        // Build the filtered template
        let filtered_tracks: Vec<daw::tracks::Track> = self.tracks
            .iter()
            .filter(|track| included.contains(&track.name))
            .cloned()
            .collect();
        
        Template {
            name: TrackName::from(format!("{} ({})", self.name, mode.as_str())),
            tracks: filtered_tracks,
        }
    }
    
    /// Get tracks for a specific mode
    ///
    /// Returns a reference to tracks that belong to the specified mode.
    pub fn tracks_for_mode(&self, mode: GroupMode) -> Vec<&daw::tracks::Track> {
        self.tracks
            .iter()
            .filter(|track| {
                let modes = track.get_modes();
                if modes.is_empty() {
                    true
                } else {
                    modes.contains(&mode)
                }
            })
            .collect()
    }
    
    /// Filter tracks by group mode with dynamic inclusion
    ///
    /// Similar to `filter_by_mode`, but also includes tracks that have content (items/audio)
    /// even if they're not normally in the specified mode. This allows tracks like Trig, Sub, Verb
    /// to be included in Recording mode if they have audio content.
    ///
    /// The `has_content` callback should return `true` if the track has items/audio on it.
    ///
    /// Parent tracks are automatically included if any of their children are included.
    pub fn filter_by_mode_with_content<F>(
        &self,
        mode: GroupMode,
        has_content: F,
    ) -> Template
    where
        F: Fn(&daw::tracks::Track) -> bool,
    {
        // First pass: determine which tracks should be included
        let mut included = std::collections::HashSet::new();
        
        for track in &self.tracks {
            // Always include tracks that are in the mode
            let modes = track.get_modes();
            let in_mode = if modes.is_empty() {
                true
            } else {
                modes.contains(&mode)
            };
            
            if in_mode {
                included.insert(track.name.clone());
            } else {
                // If not in mode, check if it has content - if so, include it dynamically
                if has_content(track) {
                    included.insert(track.name.clone());
                }
            }
        }
        
        // Second pass: include parent tracks if any of their children are included
        let mut changed = true;
        while changed {
            changed = false;
            for track in &self.tracks {
                // If this track is already included, skip
                if included.contains(&track.name) {
                    continue;
                }
                
                // Check if any child of this track is included
                let has_included_child = self.tracks.iter().any(|child| {
                    child.parent_name().map(|p| track.name.eq_ignore_case(p)).unwrap_or(false)
                        && included.contains(&child.name)
                });
                
                if has_included_child {
                    included.insert(track.name.clone());
                    changed = true;
                }
            }
        }
        
        // Build the filtered template
        let filtered_tracks: Vec<daw::tracks::Track> = self.tracks
            .iter()
            .filter(|track| included.contains(&track.name))
            .cloned()
            .collect();
        
        Template {
            name: TrackName::from(format!("{} ({})", self.name, mode.as_str())),
            tracks: filtered_tracks,
        }
    }
}

// TrackInfo is deprecated - use daw::Track directly
// Group information is stored in track metadata with key "fts.group"
// Track type is stored in metadata with key "fts.track_type"
// Parent is stored in metadata with key "fts.parent"
// Modes are stored in metadata with key "fts.modes" (comma-separated)
#[deprecated(note = "Use daw::Track directly. Group info is in metadata.")]
pub type TrackInfo = daw::tracks::Track;
