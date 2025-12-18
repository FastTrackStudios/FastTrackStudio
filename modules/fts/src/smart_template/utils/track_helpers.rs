//! Helper methods for Vec<Track>
//!
//! Provides utility methods for working with collections of tracks,
//! including counting by group, filtering, and comparisons.

use daw::tracks::{Track, MetadataKey};
use crate::smart_template::core::models::GroupMode;
use std::collections::HashMap;

/// Helper function to create a Track with template metadata
/// 
/// This makes it easy to create tracks with group, track_type, parent, and modes
/// stored in metadata, matching the old TrackInfo API.
pub fn create_track(
    name: impl Into<String>,
    track_type: Option<&str>,
    parent: Option<&str>,
    modes: &[GroupMode],
) -> Track {
    let mut track = Track::new(name.into());
    
    if let Some(tt) = track_type {
        TrackExt::set_track_type(&mut track, tt);
    }
    
    if let Some(p) = parent {
        TrackExt::set_parent_name(&mut track, p);
    }
    
    if !modes.is_empty() {
        TrackExt::set_modes(&mut track, modes);
    }
    
    track
}

/// Helper function to create a Track with just a name and group
/// 
/// This is a convenience function for tests that need to create expected tracks
pub fn create_track_with_group(name: impl Into<String>, group: &str) -> Track {
    let mut track = Track::new(name.into());
    track.set_group(group);
    track
}

/// Extension trait for Vec<Track> to add helper methods
pub trait TrackVecHelpers {
    /// Count tracks by group type
    /// Groups are stored in track metadata with key "fts.group"
    fn count_by_group(&self) -> HashMap<String, usize>;
    
    /// Count tracks for a specific group
    fn count_for_group(&self, group: &str) -> usize;
    
    /// Filter tracks by group
    fn filter_by_group(&self, group: &str) -> Vec<&Track>;
    
    /// Get all unique groups in the track collection
    fn groups(&self) -> Vec<String>;
    
    /// Set the group for a track (stores in metadata)
    fn set_group(track: &mut Track, group: &str);
    
    /// Get the group for a track (from metadata)
    fn get_group(track: &Track) -> Option<String>;
}

impl TrackVecHelpers for Vec<Track> {
    fn count_by_group(&self) -> HashMap<String, usize> {
        let mut counts = HashMap::new();
        for track in self {
            if let Some(group) = <Vec<Track> as TrackVecHelpers>::get_group(track) {
                *counts.entry(group).or_insert(0) += 1;
            }
        }
        counts
    }
    
    fn count_for_group(&self, group: &str) -> usize {
        self.iter()
            .filter(|track| <Vec<Track> as TrackVecHelpers>::get_group(track).as_deref() == Some(group))
            .count()
    }
    
    fn filter_by_group(&self, group: &str) -> Vec<&Track> {
        self.iter()
            .filter(|track| <Vec<Track> as TrackVecHelpers>::get_group(track).as_deref() == Some(group))
            .collect()
    }
    
    fn groups(&self) -> Vec<String> {
        let mut groups: Vec<String> = self.iter()
            .filter_map(|track| <Vec<Track> as TrackVecHelpers>::get_group(track))
            .collect::<std::collections::HashSet<_>>()
            .into_iter()
            .collect();
        groups.sort();
        groups
    }
    
    fn set_group(track: &mut Track, group: &str) {
        track.set_metadata(MetadataKey::GROUP, group);
    }
    
    fn get_group(track: &Track) -> Option<String> {
        track.get_metadata(MetadataKey::GROUP).cloned()
    }
}

/// Compare two Vec<Track> for equality
/// 
/// Two Vec<Track> are considered equal if they have the same tracks
/// in the same order, where tracks are compared by their name and key properties.
pub fn tracks_eq(a: &[Track], b: &[Track]) -> bool {
    if a.len() != b.len() {
        return false;
    }
    
    a.iter().zip(b.iter()).all(|(a, b)| {
        // Compare by name and key properties
        // Groups are derived from names, so we don't compare them
        // Note: track_depth is calculated dynamically from folder_depth_change, so we don't compare it
        a.name == b.name
            && a.get_metadata(MetadataKey::TRACK_TYPE) == b.get_metadata(MetadataKey::TRACK_TYPE)
            && a.get_metadata(MetadataKey::PARENT) == b.get_metadata(MetadataKey::PARENT)
            && a.is_folder == b.is_folder
            && a.folder_depth_change == b.folder_depth_change
    })
}

/// Extension trait for Track to add template-related helper methods
/// 
/// We own this trait, so we can implement it directly for daw::tracks::Track
pub trait TrackExt {
    /// Get the track type (from metadata)
    fn track_type(&self) -> Option<&str>;
    
    /// Set the track type
    fn set_track_type(&mut self, track_type: &str);
    
    /// Get the parent track name (from metadata)
    fn parent_name(&self) -> Option<&str>;
    
    /// Set the parent track name
    fn set_parent_name(&mut self, parent: &str);
    
    /// Get the group for this track
    fn get_group(&self) -> Option<String>;
    
    /// Set the group for this track
    fn set_group(&mut self, group: &str);
    
    /// Get group modes (from metadata)
    fn get_modes(&self) -> Vec<crate::smart_template::core::models::GroupMode>;
    
    /// Set group modes
    fn set_modes(&mut self, modes: &[crate::smart_template::core::models::GroupMode]);
}

// We own TrackExt, so we can implement it directly for daw::tracks::Track
impl TrackExt for Track {
    fn track_type(&self) -> Option<&str> {
        self.get_metadata(MetadataKey::TRACK_TYPE).map(|s| s.as_str())
    }
    
    fn set_track_type(&mut self, track_type: &str) {
        self.set_metadata(MetadataKey::TRACK_TYPE, track_type);
    }
    
    fn parent_name(&self) -> Option<&str> {
        self.get_metadata(MetadataKey::PARENT).map(|s| s.as_str())
    }
    
    fn set_parent_name(&mut self, parent: &str) {
        self.set_metadata(MetadataKey::PARENT, parent);
    }
    
    fn get_group(&self) -> Option<String> {
        <Vec<Track> as TrackVecHelpers>::get_group(self)
    }
    
    fn set_group(&mut self, group: &str) {
        <Vec<Track> as TrackVecHelpers>::set_group(self, group);
    }
    
    fn get_modes(&self) -> Vec<crate::smart_template::core::models::GroupMode> {
        // Parse modes from metadata
        if let Some(modes_str) = self.get_metadata(MetadataKey::MODES) {
            modes_str.split(',')
                .filter_map(|s| {
                    let s = s.trim();
                    if s.is_empty() {
                        None
                    } else {
                        // Parse as GroupMode
                        crate::smart_template::core::models::GroupMode::from_str(s)
                    }
                })
                .collect()
        } else {
            Vec::new()
        }
    }
    
    fn set_modes(&mut self, modes: &[crate::smart_template::core::models::GroupMode]) {
        let modes_str = modes.iter()
            .map(|m| m.as_str())
            .collect::<Vec<_>>()
            .join(",");
        self.set_metadata(MetadataKey::MODES, &modes_str);
    }
}
