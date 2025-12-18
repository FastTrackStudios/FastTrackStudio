//! Trait for adding children to tracks or track lists
//!
//! This allows `add_child` to work with both `Track` and `Vec<Track>`,
//! making it easy to compose track hierarchies.

use super::Track;
use crate::tracks::hierarchy_builder::build_hierarchy;
use crate::tracks::api::folder::FolderDepthChange;
use std::fmt;

/// Helper trait to convert various types into Vec<Track>
pub trait IntoTrackVec {
    fn into_track_vec(self) -> Vec<Track>;
}

impl IntoTrackVec for Track {
    fn into_track_vec(self) -> Vec<Track> {
        vec![self]
    }
}

impl IntoTrackVec for Vec<Track> {
    fn into_track_vec(self) -> Vec<Track> {
        self
    }
}

impl<'a> IntoTrackVec for &'a [Track] {
    fn into_track_vec(self) -> Vec<Track> {
        self.to_vec()
    }
}

/// Trait for types that can have child tracks added
pub trait AddChild {
    /// Add child tracks using folder properties
    ///
    /// This method converts the parent to a folder and sets up children with proper
    /// folder depth properties. Accepts either a single Track or Vec<Track>.
    /// Returns a Vec<Track> in the correct order.
    fn add_child<T>(self, children: T) -> Vec<Track>
    where
        T: Into<Vec<Track>>;
    
    
    /// Insert children at a specific position
    ///
    /// Finds the first track with the given name and inserts the children after it.
    /// Returns None if the track is not found.
    ///
    /// # Example
    /// ```rust
    /// let mut tracks = Track::new("Kick").add_child(vec![
    ///     Track::new("Kick Sub"),
    ///     Track::new("Kick Ambient"),
    /// ]);
    /// // Insert SUM tracks after "Kick" but before "Kick Sub"
    /// tracks.insert_children_after("Kick", kick_sum_tracks);
    /// ```
    fn insert_children_after<T>(self, after_track_name: &str, children: T) -> Option<Vec<Track>>
    where
        T: Into<Vec<Track>>;
}

impl AddChild for Track {
    fn add_child<T>(self, children: T) -> Vec<Track>
    where
        T: Into<Vec<Track>>,
    {
        let children_vec = children.into();
        build_hierarchy(self, children_vec)
    }
    
    
    fn insert_children_after<T>(self, after_track_name: &str, children: T) -> Option<Vec<Track>>
    where
        T: Into<Vec<Track>>,
    {
        // For a single Track, we can't insert after it since it's the parent
        // This only makes sense for Vec<Track>
        None
    }
}

impl AddChild for Vec<Track> {
    fn add_child<T>(self, children: T) -> Vec<Track>
    where
        T: Into<Vec<Track>>,
    {
        // For Vec<Track>, we add children to the last track in the vec
        // The last track should be a folder that can contain the children
        let mut result = self;
        if let Some(last_track) = result.pop() {
            let children_vec = children.into();
            let new_tracks = build_hierarchy(last_track, children_vec);
            result.extend(new_tracks);
        }
        result
    }
    
    
    fn insert_children_after<T>(mut self, after_track_name: &str, children: T) -> Option<Vec<Track>>
    where
        T: Into<Vec<Track>>,
    {
        let children_vec = children.into();
        
        // Find the position of the track with the given name
        let insert_pos = self.iter()
            .position(|t| t.name == after_track_name)?;
        
        // Insert children after the found track
        // We need to adjust folder_depth_change for the inserted tracks
        // and update the closing of the folder if needed
        
        // Split the vec at the insertion point
        let after_track = self[insert_pos].clone();
        let mut before = self[..=insert_pos].to_vec();
        let mut after = self[insert_pos + 1..].to_vec();
        
        // If the track we're inserting after is a folder, we need to handle its children
        // For now, let's insert the children right after the track
        // and adjust the closing logic
        
        // Calculate the depth for inserted children
        let insert_depth = after_track.track_depth;
        
        // Update depths and folder_depth_change for inserted children
        let mut inserted_children = children_vec;
        for child in &mut inserted_children {
            child.track_depth = insert_depth;
            // Preserve folder starts, but set normal tracks appropriately
            if !child.is_folder && child.folder_depth_change != FolderDepthChange::FolderStart {
                child.folder_depth_change = FolderDepthChange::Normal;
            }
        }
        
        // If there are tracks after, we need to update the closing
        // The last inserted child should not close if there are more children
        if !after.is_empty() && !inserted_children.is_empty() {
            if let Some(last_inserted) = inserted_children.last_mut() {
                // If it was closing, we need to preserve that but not close the parent yet
                match last_inserted.folder_depth_change {
                    FolderDepthChange::ClosesLevels(n) if n < -1 => {
                        // It closes multiple levels, preserve that
                    }
                    FolderDepthChange::ClosesLevels(_) => {
                        // It was closing one level, set to Normal since more children follow
                        last_inserted.folder_depth_change = FolderDepthChange::Normal;
                    }
                    _ => {}
                }
            }
        }
        
        // Combine: before + inserted + after
        before.extend(inserted_children);
        before.extend(after);
        
        // Update the last track to close the folder if needed
        // First calculate open levels without borrowing mutably
        let mut open_levels = 0;
        for track in &before {
            if track.is_folder {
                open_levels += 1;
            }
            if let FolderDepthChange::ClosesLevels(n) = track.folder_depth_change {
                open_levels += n; // n is negative, so this subtracts
            }
        }
        
        // Now update the last track if needed
        if let Some(last_track) = before.last_mut() {
            // If it's not already closing, make it close
            if !last_track.folder_depth_change.closes_levels() && open_levels > 0 {
                last_track.folder_depth_change = FolderDepthChange::ClosesLevels(-(open_levels as i32));
            }
        }
        
        Some(before)
    }
}

// Helper trait to convert Track or Vec<Track> into Vec<Track>
impl From<Track> for Vec<Track> {
    fn from(track: Track) -> Self {
        vec![track]
    }
}

/// Trait for printing track hierarchies as a tree
///
/// Uses folder properties (is_folder, folder_depth_change, track_depth) to display
/// the hierarchy as a tree structure.
pub trait PrintTrackTree {
    /// Print the track list as a tree hierarchy
    fn print_tree(&self) -> String;
}

impl PrintTrackTree for Vec<Track> {
    fn print_tree(&self) -> String {
        self.as_slice().print_tree()
    }
}

impl PrintTrackTree for &[Track] {
    fn print_tree(&self) -> String {
        let tracks = self;
        
        if tracks.is_empty() {
            return "(empty track list)\n".to_string();
        }
        
        let mut output = String::new();
        
        for (i, track) in tracks.iter().enumerate() {
            let depth = track.track_depth.value() as usize;
            
            // Build prefix based on depth
            let prefix = "    ".repeat(depth);
            
            // Determine if this is the last track at this depth
            let is_last_at_depth = {
                let next_track_at_same_or_lower = tracks.iter()
                    .enumerate()
                    .skip(i + 1)
                    .find(|(_, t)| t.track_depth.value() as usize <= depth);
                
                next_track_at_same_or_lower.is_none() || 
                track.folder_depth_change.closes_levels()
            };
            
            let connector = if is_last_at_depth { "└── " } else { "├── " };
            
            // Write the track
            output.push_str(&format!("{}{}{}", prefix, connector, track.name));
            
            // Show folder indicator
            if track.is_folder {
                output.push_str(" [folder]");
            }
            
            output.push('\n');
        }
        
        output
    }
}
