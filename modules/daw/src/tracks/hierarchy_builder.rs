//! Helper for building track hierarchies using folder depth properties
//!
//! Tracks don't store direct parent/child references. Instead, they use:
//! - `is_folder`: whether the track is a folder
//! - `folder_depth_change`: relative change from previous track (1 = starts folder, -1 = closes folder)
//! - `track_depth`: absolute cumulative nesting level
//!
//! This module provides helpers to build hierarchies using these properties.

use super::Track;
use crate::tracks::api::folder::{FolderDepthChange, TrackDepth};

/// Helper to build a track hierarchy using folder depth properties
///
/// Builds tracks in order with correct `is_folder`, `folder_depth_change`, and `track_depth` values.
///
/// # Example
/// ```rust
/// let tracks = build_track_hierarchy(vec![
///     ("Kick", true),  // (name, is_folder)
///     ("Kick In", false),
///     ("Kick Out", false),
/// ]);
/// ```
pub fn build_track_hierarchy(specs: Vec<(&str, bool)>) -> Vec<Track> {
    let mut tracks = Vec::new();
    let mut current_depth = TrackDepth::default();
    let mut open_folders = 0;
    
    for (i, (name, is_folder)) in specs.iter().enumerate() {
        let mut track = Track::new(name.to_string());
        track.is_folder = *is_folder;
        
        // Calculate folder_depth_change
        if *is_folder {
            // This track starts a new folder
            track.folder_depth_change = FolderDepthChange::FolderStart;
            current_depth = current_depth.apply_change(FolderDepthChange::FolderStart);
            open_folders += 1;
        } else {
            // Normal track
            track.folder_depth_change = FolderDepthChange::Normal;
        }
        
        track.track_depth = current_depth;
        tracks.push(track);
    }
    
    // Close any open folders at the end
    if open_folders > 0 && !tracks.is_empty() {
        if let Some(last_track) = tracks.last_mut() {
            last_track.folder_depth_change = FolderDepthChange::ClosesLevels(-(open_folders as i32));
        }
    }
    
    tracks
}

/// Build a hierarchy from a parent track and its children
///
/// This is a convenience function that creates a folder structure:
/// - Parent track becomes a folder (is_folder=true, folder_depth_change=FolderStart)
/// - Children are normal tracks (is_folder=false, folder_depth_change=Normal)
/// - Last child closes the folder (folder_depth_change=ClosesLevels(-1))
///
/// # Example
/// ```rust
/// let parent = Track::new("Drums".to_string());
/// let child1 = Track::new("Kick".to_string());
/// let child2 = Track::new("Snare".to_string());
/// let tracks = build_hierarchy(parent, vec![child1, child2]);
/// ```
pub fn build_hierarchy(mut parent: Track, children: Vec<Track>) -> Vec<Track> {
    let mut tracks = Vec::new();
    let children_len = children.len();
    
    // Parent is a folder
    parent.is_folder = true;
    parent.folder_depth_change = FolderDepthChange::FolderStart;
    parent.track_depth = TrackDepth::default();
    tracks.push(parent);
    
    // Add children, tracking depth changes as we go
    let mut current_depth = TrackDepth::default().apply_change(FolderDepthChange::FolderStart);
    for (i, mut child) in children.into_iter().enumerate() {
        // Preserve folder status if child is already a folder (for nested folders)
        // Don't overwrite is_folder if it's already set to true
        
        // Set track depth to current depth (before applying this track's change)
        child.track_depth = current_depth;
        
        // Handle folder_depth_change based on position and existing state
        if i == children_len - 1 {
            // Last child: must close the parent folder
            match child.folder_depth_change {
                FolderDepthChange::ClosesLevels(n) => {
                    // Already closes n levels (e.g., -1 for SUM), close one more for parent (e.g., -2)
                    child.folder_depth_change = FolderDepthChange::ClosesLevels(n - 1);
                    // Update depth: close n-1 levels (n is negative, so n-1 is more negative)
                    current_depth = current_depth.apply_change(FolderDepthChange::ClosesLevels(n - 1));
                }
                FolderDepthChange::FolderStart => {
                    // If it's a folder start, it can't also close - this shouldn't happen
                    // But preserve it and add closing
                    child.folder_depth_change = FolderDepthChange::ClosesLevels(-1);
                    current_depth = current_depth.apply_change(FolderDepthChange::ClosesLevels(-1));
                }
                _ => {
                    // No existing closing, just close the parent folder
                    child.folder_depth_change = FolderDepthChange::ClosesLevels(-1);
                    current_depth = current_depth.apply_change(FolderDepthChange::ClosesLevels(-1));
                }
            }
        } else {
            // Not the last child: preserve folder starts and closings, but set normal tracks to Normal
            match child.folder_depth_change {
                FolderDepthChange::FolderStart => {
                    // Preserve folder start for nested folders
                    // Update depth: start a new folder (depth increases by 1)
                    current_depth = current_depth.apply_change(FolderDepthChange::FolderStart);
                }
                FolderDepthChange::ClosesLevels(n) => {
                    // Preserve existing closing (e.g., Kick Trig closes SUM)
                    // Update depth: close n levels (n is negative)
                    current_depth = current_depth.apply_change(FolderDepthChange::ClosesLevels(n));
                }
                _ => {
                    // Normal track, set to Normal
                    child.folder_depth_change = FolderDepthChange::Normal;
                    // Depth stays the same for normal tracks
                }
            }
        }
        
        tracks.push(child);
    }
    
    tracks
}

/// Builder for creating track hierarchies with folder properties
///
/// This builder helps you create hierarchies by automatically calculating
/// folder_depth_change and track_depth values.
pub struct TrackHierarchyBuilder {
    tracks: Vec<Track>,
    current_depth: TrackDepth,
    open_folders: Vec<String>, // Track names of open folders
}

impl TrackHierarchyBuilder {
    /// Create a new hierarchy builder
    pub fn new() -> Self {
        Self {
            tracks: Vec::new(),
            current_depth: TrackDepth::default(),
            open_folders: Vec::new(),
        }
    }
    
    /// Add a folder track (starts a new folder)
    ///
    /// Returns self for chaining.
    pub fn add_folder(mut self, name: impl Into<String>) -> Self {
        let name = name.into();
        let mut track = Track::new(name.clone());
        track.is_folder = true;
        track.folder_depth_change = FolderDepthChange::FolderStart;
        self.current_depth = self.current_depth.apply_change(FolderDepthChange::FolderStart);
        track.track_depth = self.current_depth;
        self.open_folders.push(name);
        self.tracks.push(track);
        self
    }
    
    /// Add a child track (normal track, child of the most recent folder)
    ///
    /// Returns self for chaining.
    pub fn add_child(mut self, name: impl Into<String>) -> Self {
        let mut track = Track::new(name.into());
        track.is_folder = false;
        track.folder_depth_change = FolderDepthChange::Normal;
        track.track_depth = self.current_depth;
        self.tracks.push(track);
        self
    }
    
    /// Close the most recent folder
    ///
    /// Returns self for chaining.
    pub fn close_folder(mut self) -> Self {
        if !self.open_folders.is_empty() {
            self.open_folders.pop();
            self.current_depth = TrackDepth::new((self.current_depth.value() - 1).max(0));
        }
        self
    }
    
    /// Build the final vector of tracks
    ///
    /// Automatically closes any open folders.
    pub fn build(mut self) -> Vec<Track> {
        // Close all remaining open folders by adjusting the last track
        if !self.open_folders.is_empty() && !self.tracks.is_empty() {
            let levels_to_close = self.open_folders.len() as i32;
            if let Some(last_track) = self.tracks.last_mut() {
                last_track.folder_depth_change = FolderDepthChange::ClosesLevels(-levels_to_close);
            }
        }
        self.tracks
    }
}

impl Default for TrackHierarchyBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tracks::{AddChild, PrintTrackTree};
    
    #[test]
    fn test_kick_group_hierarchy() {
        // Build Kick Sum folder with its children
        let kick_sum_tracks = Track::new("Kick Sum".to_string())
            .add_child(vec![
                Track::new("Kick In".to_string()),
                Track::new("Kick Out".to_string()),
                Track::new("Kick Trig".to_string()),
            ]);
        
        // Build Kick Bus with SUM folder and direct children
        let kick_sub = Track::new("Kick Sub".to_string());
        let kick_ambient = Track::new("Kick Ambient".to_string());
        
        // Combine all children: SUM folder + Sub + Ambient
        let mut all_kick_children = kick_sum_tracks;
        all_kick_children.push(kick_sub);
        all_kick_children.push(kick_ambient);
        
        let kick_bus_tracks = Track::new("Kick Bus".to_string())
            .add_child(all_kick_children);
        
        // Print the tree for debugging
        let tree = kick_bus_tracks.print_tree();
        println!("Kick hierarchy:\n{}", tree);
        
        // Verify structure
        // Kick Bus should be at depth 0, is_folder=true, FolderStart
        assert_eq!(kick_bus_tracks[0].name, "Kick Bus");
        assert!(kick_bus_tracks[0].is_folder);
        assert_eq!(kick_bus_tracks[0].track_depth.value(), 0);
        assert_eq!(kick_bus_tracks[0].folder_depth_change, FolderDepthChange::FolderStart);
        
        // Kick Sum should be at depth 1, is_folder=true, FolderStart
        assert_eq!(kick_bus_tracks[1].name, "Kick Sum");
        assert!(kick_bus_tracks[1].is_folder);
        assert_eq!(kick_bus_tracks[1].track_depth.value(), 1);
        assert_eq!(kick_bus_tracks[1].folder_depth_change, FolderDepthChange::FolderStart);
        
        // Kick In should be at depth 2
        assert_eq!(kick_bus_tracks[2].name, "Kick In");
        assert!(!kick_bus_tracks[2].is_folder);
        assert_eq!(kick_bus_tracks[2].track_depth.value(), 2);
        assert_eq!(kick_bus_tracks[2].folder_depth_change, FolderDepthChange::Normal);
        
        // Kick Out should be at depth 2
        assert_eq!(kick_bus_tracks[3].name, "Kick Out");
        assert!(!kick_bus_tracks[3].is_folder);
        assert_eq!(kick_bus_tracks[3].track_depth.value(), 2);
        assert_eq!(kick_bus_tracks[3].folder_depth_change, FolderDepthChange::Normal);
        
        // Kick Trig should be at depth 2, closes SUM folder
        assert_eq!(kick_bus_tracks[4].name, "Kick Trig");
        assert!(!kick_bus_tracks[4].is_folder);
        assert_eq!(kick_bus_tracks[4].track_depth.value(), 2);
        assert_eq!(kick_bus_tracks[4].folder_depth_change, FolderDepthChange::ClosesLevels(-1));
        
        // Kick Sub should be at depth 1 (same level as Kick Sum, child of Kick Bus)
        assert_eq!(kick_bus_tracks[5].name, "Kick Sub");
        assert!(!kick_bus_tracks[5].is_folder);
        assert_eq!(kick_bus_tracks[5].track_depth.value(), 1);
        assert_eq!(kick_bus_tracks[5].folder_depth_change, FolderDepthChange::Normal);
        
        // Kick Ambient should be at depth 1, closes Kick Bus folder (1 level)
        assert_eq!(kick_bus_tracks[6].name, "Kick Ambient");
        assert!(!kick_bus_tracks[6].is_folder);
        assert_eq!(kick_bus_tracks[6].track_depth.value(), 1);
        assert_eq!(kick_bus_tracks[6].folder_depth_change, FolderDepthChange::ClosesLevels(-1));
        
        // Verify tree output contains expected structure
        assert!(tree.contains("Kick Bus"));
        assert!(tree.contains("Kick Sum"));
        assert!(tree.contains("Kick In"));
        assert!(tree.contains("Kick Out"));
        assert!(tree.contains("Kick Trig"));
        assert!(tree.contains("Kick Sub"));
        assert!(tree.contains("Kick Ambient"));
    }
}

