//! Helper for building track hierarchies using folder depth properties
//!
//! Tracks don't store direct parent/child references. Instead, they use:
//! - `is_folder`: whether the track is a folder
//! - `folder_depth_change`: relative change from previous track (1 = starts folder, -1 = closes folder)
//!
//! Track depth is calculated dynamically from folder_depth_change values, not stored.
//! This module provides helpers to build hierarchies using these properties.

use super::Track;
use crate::tracks::api::folder::FolderDepthChange;
use crate::tracks::types::TrackName;

/// Helper to build a track hierarchy using folder depth properties
///
/// Builds tracks in order with correct `is_folder` and `folder_depth_change` values.
pub fn build_track_hierarchy(specs: Vec<(&str, bool)>) -> Vec<Track> {
    let mut tracks = Vec::new();
    let mut open_folders = 0;
    
    for (name, is_folder) in specs {
        let mut track = Track::new(name);
        track.is_folder = is_folder;
        
        if is_folder {
            track.folder_depth_change = FolderDepthChange::FolderStart;
            open_folders += 1;
        } else {
            track.folder_depth_change = FolderDepthChange::Normal;
        }
        
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
pub fn build_hierarchy(mut parent: Track, children: Vec<Track>) -> Vec<Track> {
    let mut tracks = Vec::new();
    let children_len = children.len();
    
    parent.is_folder = true;
    parent.folder_depth_change = FolderDepthChange::FolderStart;
    tracks.push(parent);
    
    let mut all_children = Vec::new();
    
    for (i, mut child) in children.into_iter().enumerate() {
        if i == children_len - 1 {
            match child.folder_depth_change {
                FolderDepthChange::ClosesLevels(n) => {
                    child.folder_depth_change = FolderDepthChange::ClosesLevels(n - 1);
                }
                FolderDepthChange::FolderStart => {
                    child.folder_depth_change = FolderDepthChange::ClosesLevels(-1);
                }
                _ => {
                    child.folder_depth_change = FolderDepthChange::ClosesLevels(-1);
                }
            }
        } else {
            match child.folder_depth_change {
                FolderDepthChange::FolderStart => {}
                FolderDepthChange::ClosesLevels(_) => {}
                _ => {
                    child.folder_depth_change = FolderDepthChange::Normal;
                }
            }
        }
        
        all_children.push(child);
    }
    
    tracks.extend(all_children);
    tracks
}

/// A fluent, recursive builder for creating track hierarchies.
/// 
/// This builder automatically calculates REAPER's `folder_depth_change` values
/// by tracking an internal stack of open folders.
pub struct TrackHierarchyBuilder {
    tracks: Vec<Track>,
    folder_stack: Vec<TrackName>,
}

impl TrackHierarchyBuilder {
    /// Create a new TrackHierarchyBuilder
    pub fn new() -> Self {
        Self {
            tracks: Vec::new(),
            folder_stack: Vec::new(),
        }
    }

    /// Add a folder track and start a new nesting level.
    /// 
    /// Subsequent calls to `track()` or `folder()` will be children of this folder
    /// until `end()` is called.
    pub fn folder(mut self, name: impl Into<TrackName>) -> Self {
        let name = name.into();
        let mut track = Track::new(name.clone());
        track.is_folder = true;
        track.folder_depth_change = FolderDepthChange::FolderStart;
        
        // If this is the last track and it was closing levels, we need to adjust it
        // but here we are starting a NEW folder, so the previous track's depth change remains.
        
        self.tracks.push(track);
        self.folder_stack.push(name);
        self
    }

    /// Add a normal track at the current nesting level.
    pub fn track(mut self, name: impl Into<TrackName>) -> Self {
        let mut track = Track::new(name.into());
        track.is_folder = false;
        track.folder_depth_change = FolderDepthChange::Normal;
        self.tracks.push(track);
        self
    }

    /// Finish the current folder level and return to the parent level.
    pub fn end(mut self) -> Self {
        if self.folder_stack.pop().is_some() {
            // When a folder ends, the LAST track added to that folder must close the level.
            if let Some(last_track) = self.tracks.last_mut() {
                let current = last_track.folder_depth_change.to_reaper_value();
                last_track.folder_depth_change = FolderDepthChange::from_reaper_value(current - 1);
            }
        }
        self
    }

    /// Build the final list of tracks.
    /// 
    /// Automatically closes any remaining open folders.
    pub fn build(mut self) -> Vec<Track> {
        while !self.folder_stack.is_empty() {
            self = self.end();
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
    use crate::tracks::PrintTrackTree;

    #[test]
    fn test_fluent_hierarchy_builder() {
        let tracks = TrackHierarchyBuilder::new()
            .folder("Drums")
                .folder("Kick")
                    .track("Kick In")
                    .track("Kick Out")
                .end()
                .folder("Snare")
                    .track("Snare Top")
                    .track("Snare Bottom")
                .end()
            .end()
            .build();

        let tree = tracks.print_tree();
        println!("Generated Tree:\n{}", tree);

        // Drums (FolderStart)
        assert_eq!(tracks[0].name.0, "Drums");
        assert_eq!(tracks[0].folder_depth_change, FolderDepthChange::FolderStart);

        // Kick (FolderStart)
        assert_eq!(tracks[1].name.0, "Kick");
        assert_eq!(tracks[1].folder_depth_change, FolderDepthChange::FolderStart);

        // Kick In (Normal)
        assert_eq!(tracks[2].name.0, "Kick In");
        assert_eq!(tracks[2].folder_depth_change, FolderDepthChange::Normal);

        // Kick Out (ClosesLevels(-1)) - closes Kick
        assert_eq!(tracks[3].name.0, "Kick Out");
        assert_eq!(tracks[3].folder_depth_change, FolderDepthChange::ClosesLevels(-1));

        // Snare (FolderStart)
        assert_eq!(tracks[4].name.0, "Snare");
        assert_eq!(tracks[4].folder_depth_change, FolderDepthChange::FolderStart);

        // Snare Top (Normal)
        assert_eq!(tracks[5].name.0, "Snare Top");
        assert_eq!(tracks[5].folder_depth_change, FolderDepthChange::Normal);

        // Snare Bottom (ClosesLevels(-2)) - closes Snare AND Drums
        assert_eq!(tracks[6].name.0, "Snare Bottom");
        assert_eq!(tracks[6].folder_depth_change, FolderDepthChange::ClosesLevels(-2));
    }
}
