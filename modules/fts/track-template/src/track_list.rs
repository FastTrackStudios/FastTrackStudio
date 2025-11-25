//! Track list management
//!
//! A TrackList represents a collection of tracks that can be matched against
//! and organized into a template structure.

use std::collections::HashMap;
use std::fmt;
use crate::track::{Track, SortStatus};

/// A list of tracks that can be matched and organized
#[derive(Debug, Clone, Default)]
pub struct TrackList {
    /// All tracks indexed by name
    tracks: HashMap<String, Track>,
    
    /// Track order (for maintaining insertion order)
    track_order: Vec<String>,
}

impl TrackList {
    /// Create a new empty track list
    pub fn new() -> Self {
        Self {
            tracks: HashMap::new(),
            track_order: Vec::new(),
        }
    }
    
    /// Add a track to the list
    pub fn add_track(&mut self, track: Track) {
        let name = track.name.clone();
        if !self.tracks.contains_key(&name) {
            self.track_order.push(name.clone());
        }
        self.tracks.insert(name, track);
    }
    
    /// Get a track by name
    pub fn get_track(&self, name: &str) -> Option<&Track> {
        self.tracks.get(name)
    }
    
    /// Get a mutable track by name
    pub fn get_track_mut(&mut self, name: &str) -> Option<&mut Track> {
        self.tracks.get_mut(name)
    }
    
    /// Check if a track exists
    pub fn has_track(&self, name: &str) -> bool {
        self.tracks.contains_key(name)
    }
    
    /// Remove a track
    pub fn remove_track(&mut self, name: &str) -> Option<Track> {
        if let Some(pos) = self.track_order.iter().position(|n| n == name) {
            self.track_order.remove(pos);
        }
        self.tracks.remove(name)
    }
    
    /// Get all tracks in order
    pub fn tracks(&self) -> Vec<&Track> {
        self.track_order.iter()
            .filter_map(|name| self.tracks.get(name))
            .collect()
    }
    
    /// Get all track names in order
    pub fn track_names(&self) -> &[String] {
        &self.track_order
    }
    
    /// Get all child tracks of a parent track
    pub fn get_children(&self, parent_name: &str) -> Vec<&Track> {
        self.tracks()
            .into_iter()
            .filter(|track| track.parent.as_ref().map(|p| p == parent_name).unwrap_or(false))
            .collect()
    }
    
    /// Get the number of tracks
    pub fn len(&self) -> usize {
        self.tracks.len()
    }
    
    /// Check if the track list is empty
    pub fn is_empty(&self) -> bool {
        self.tracks.is_empty()
    }
    
    /// Find a track by partial name match
    pub fn find_track_by_partial(&self, partial: &str) -> Vec<&Track> {
        let partial_lower = partial.to_lowercase();
        self.tracks()
            .into_iter()
            .filter(|track| track.name.to_lowercase().contains(&partial_lower))
            .collect()
    }
    
    /// Find tracks by exact name match (case-insensitive)
    pub fn find_track_exact(&self, name: &str) -> Option<&Track> {
        let name_lower = name.to_lowercase();
        self.tracks()
            .into_iter()
            .find(|track| track.name.to_lowercase() == name_lower)
    }
    
    /// Get root tracks (tracks without parents)
    pub fn get_roots(&self) -> Vec<&Track> {
        self.tracks()
            .into_iter()
            .filter(|track| track.parent.is_none())
            .collect()
    }
    
    /// Get tracks filtered by sort status
    pub fn get_tracks_by_sort_status(&self, status: SortStatus) -> Vec<&Track> {
        self.tracks()
            .into_iter()
            .filter(|track| track.sort_status == status)
            .collect()
    }
    
    /// Get tracks that need confirmation
    pub fn get_tracks_needing_confirmation(&self) -> Vec<&Track> {
        self.get_tracks_by_sort_status(SortStatus::NeedsConfirmation)
    }
    
    /// Get tracks that are not sorted
    pub fn get_unsorted_tracks(&self) -> Vec<&Track> {
        self.get_tracks_by_sort_status(SortStatus::NotSorted)
    }
    
    /// Get sorted tracks
    pub fn get_sorted_tracks(&self) -> Vec<&Track> {
        self.get_tracks_by_sort_status(SortStatus::Sorted)
    }
    
    /// Format the track list as a tree, starting from the given track
    fn fmt_tree_recursive(&self, f: &mut fmt::Formatter, track: &Track, prefix: &str, is_last: bool) -> fmt::Result {
        // Get children first to determine if this track has children
        let children = self.get_children(&track.name);
        let has_children = !children.is_empty();
        
        // Draw the tree connector
        let connector = if is_last { "└── " } else { "├── " };
        write!(f, "{}{}", prefix, connector)?;
        
        // Track name and type
        write!(f, "{}", track.name)?;
        if let Some(track_type) = &track.track_type {
            write!(f, " ({})", track_type)?;
        }
        
        writeln!(f)?;
        
        // Calculate prefix for children
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        
        // Takes
        for (i, take) in track.takes.iter().enumerate() {
            let is_last_take = i == track.takes.len() - 1 && track.sends.is_empty() && track.receives.is_empty() && !has_children;
            let take_connector = if is_last_take { "└── " } else { "├── " };
            write!(f, "{}{}Take: {}", child_prefix, take_connector, take.name)?;
            if let Some(index) = take.index {
                write!(f, " [{}]", index)?;
            }
            writeln!(f)?;
        }
        
        // Sends
        for (i, send) in track.sends.iter().enumerate() {
            let is_last_send = i == track.sends.len() - 1 && track.receives.is_empty() && !has_children;
            let send_connector = if is_last_send { "└── " } else { "├── " };
            write!(f, "{}{}Send → {}", child_prefix, send_connector, send.target_track)?;
            if let Some(level) = send.level {
                write!(f, " ({:.2})", level)?;
            }
            writeln!(f)?;
        }
        
        // Receives
        for (i, receive) in track.receives.iter().enumerate() {
            let is_last_receive = i == track.receives.len() - 1 && !has_children;
            let receive_connector = if is_last_receive { "└── " } else { "├── " };
            write!(f, "{}{}Receive ← {}", child_prefix, receive_connector, receive.target_track)?;
            if let Some(level) = receive.level {
                write!(f, " ({:.2})", level)?;
            }
            writeln!(f)?;
        }
        
        // Format children recursively
        for (i, child) in children.iter().enumerate() {
            let is_last_child = i == children.len() - 1;
            self.fmt_tree_recursive(f, child, &child_prefix, is_last_child)?;
        }
        
        Ok(())
    }
}

impl fmt::Display for TrackList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let roots = self.get_roots();
        
        if roots.is_empty() {
            writeln!(f, "(empty track list)")?;
            return Ok(());
        }
        
        for (i, root) in roots.iter().enumerate() {
            let is_last = i == roots.len() - 1;
            self.fmt_tree_recursive(f, root, "", is_last)?;
        }
        
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::track::{Take, SendReceive};
    
    #[test]
    fn test_tree_display() {
        let mut track_list = TrackList::new();
        
        // Create a hierarchical structure
        let drums = Track::with_type("Drums", "BUS");
        track_list.add_track(drums);
        
        let mut kick = Track::with_type("Kick", "BUS");
        kick.set_parent("Drums");
        kick.add_take(Take::new("Kick In"));
        kick.add_take(Take::new("Kick Out"));
        track_list.add_track(kick);
        
        let mut snare = Track::with_type("Snare", "BUS");
        snare.set_parent("Drums");
        snare.add_take(Take::new("Snare Top"));
        snare.add_send(SendReceive::new("Snare Trig"));
        track_list.add_track(snare);
        
        // Convert to string and verify structure
        let display = format!("{}", track_list);
        assert!(display.contains("Drums (BUS)"));
        assert!(display.contains("Kick (BUS)"));
        assert!(display.contains("Snare (BUS)"));
        assert!(display.contains("Take: Kick In"));
        assert!(display.contains("Send → Snare Trig"));
    }
}
