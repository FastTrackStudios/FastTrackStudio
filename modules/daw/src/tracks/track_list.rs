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
            .position(|t| t.name.0 == after_track_name)?;
        
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
        
        // Calculate the depth for inserted children (depth is calculated, not stored)
        // The depth will be calculated correctly when the tree is printed
        
        // Update folder_depth_change for inserted children
        let mut inserted_children = children_vec;
        for child in &mut inserted_children {
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
        use colored::Colorize;
        
        let tracks = self;
        
        if tracks.is_empty() {
            return "(empty track list)\n".to_string();
        }
        
        // Folder icon
        const FOLDER_ICON: &str = "📁";
        
        // Helper function to calculate depth for a track at a given index
        fn calculate_depth_at(tracks: &[Track], index: usize) -> u32 {
            Track::calculate_depth(tracks, index)
        }
        
        // Helper function to determine group color based on track
        // Also takes parent track name for better context
        fn get_group_color(track: &Track, parent_name: Option<&str>) -> Option<colored::ColoredString> {
            use colored::Colorize;
            
            // Try to parse ItemProperties from ext_state
            if let Some(ref ext_state) = track.ext_state {
                if let Ok(props) = serde_json::from_str::<serde_json::Value>(ext_state) {
                    if let Some(group_prefix) = props.get("group_prefix").and_then(|v| v.as_str()) {
                        return match group_prefix {
                            "Kick" | "Snare" | "Tom" | "Cymbals" | "Room" | "K" | "S" | "T" | "C" | "R" => {
                                Some(track.name.0.red().bold())
                            },
                            "Bass" | "B" => Some(track.name.0.yellow().bold()),
                            "Guitar Electric" | "GTR E" | "G E" => Some(track.name.0.blue().bold()),
                            "Guitar Acoustic" | "GTR A" | "G A" => {
                                Some(track.name.0.truecolor(46, 139, 87).bold()) // Sea green
                            },
                            "Keys" => Some(track.name.0.green().bold()),
                            "Synths" => Some(track.name.0.purple().bold()),
                            "Vocals" | "V" => Some(track.name.0.truecolor(255, 192, 203).bold()), // Pink
                            "BGVs" | "V BGVs" => Some(track.name.0.purple().bold()),
                            _ => None,
                        };
                    }
                }
            }
            
            // Fallback: infer from track name and parent context
            let name_lower = track.name.to_lowercase();
            let parent_lower = parent_name.map(|s| s.to_lowercase());
            
            // Drums: Red
            if name_lower == "kick" || name_lower == "snare" || name_lower == "tom" || 
               name_lower == "cymbals" || name_lower == "room" || name_lower == "sum" ||
               name_lower == "in" || name_lower == "out" || name_lower == "trig" ||
               name_lower == "top" || name_lower == "bottom" || name_lower == "verb" ||
               name_lower == "sub" || name_lower == "amb" || name_lower == "rooms" ||
               name_lower == "highhat" || name_lower == "oh" || name_lower == "ride" ||
               name_lower == "1" || name_lower == "2" ||
               parent_lower.as_ref().map_or(false, |p| {
                   p == "kick" || p == "snare" || p == "tom" || p == "cymbals" || p == "room" || p == "sum"
               }) {
                return Some(track.name.0.red().bold());
            }
            
            // Bass: Yellow
            if name_lower == "di" || name_lower == "amp" ||
               parent_lower.as_ref().map_or(false, |p| p.contains("bass")) {
                return Some(track.name.0.yellow().bold());
            }
            
            // Guitar Electric: Blue
            if name_lower == "clean" || name_lower == "crunch" || name_lower == "lead" {
                return Some(track.name.0.blue().bold());
            }
            
            // Guitar Acoustic: Sea Green
            if name_lower == "main" || name_lower == "verse" || name_lower == "chorus" {
                // Check if we're in a guitar context - for now assume acoustic if it's these names
                return Some(track.name.0.truecolor(46, 139, 87).bold()); // Sea green
            }
            
            // Keys: Green
            if name_lower == "piano" || name_lower == "electric" || name_lower == "organ" {
                return Some(track.name.0.green().bold());
            }
            
            // Synths: Purple
            if name_lower == "pad" || name_lower == "bass" {
                return Some(track.name.0.purple().bold());
            }
            
            // Vocals: Pink
            if name_lower == "main" || name_lower == "double" {
                // Could be vocals or acoustic guitar - need better context
                // For now, if it's "main" or "double" without other context, assume vocals
                return Some(track.name.0.truecolor(255, 192, 203).bold()); // Pink
            }
            
            // BGVs: Purple
            if name_lower == "soprano" || name_lower == "alto" || name_lower == "tenor" || name_lower == "unison" {
                return Some(track.name.0.purple().bold());
            }
            
            None
        }
        
        let mut output = String::new();
        
        // Track parent names for context
        let mut parent_stack: Vec<&str> = Vec::new();
        
        for (i, track) in tracks.iter().enumerate() {
            let depth = calculate_depth_at(tracks, i) as usize;
            
            // Update parent stack based on depth
            while parent_stack.len() > depth {
                parent_stack.pop();
            }
            
            // Build prefix based on depth
            let prefix = "    ".repeat(depth);
            
            // Determine if this is the last track at this depth
            let is_last_at_depth = {
                let next_track_at_same_or_lower = tracks.iter()
                    .enumerate()
                    .skip(i + 1)
                    .find(|(j, _)| calculate_depth_at(tracks, *j) as usize <= depth);
                
                next_track_at_same_or_lower.is_none() || 
                track.folder_depth_change.closes_levels()
            };
            
            let connector = if is_last_at_depth { "└── " } else { "├── " };
            
            // Color the connector (tree lines) in bright black (gray)
            let colored_connector = connector.bright_black();
            
            // Get parent name for context
            let parent_name = parent_stack.last().copied();
            
            // Get group color or use default
            let colored_name = get_group_color(track, parent_name)
                .unwrap_or_else(|| track.name.0.normal());
            
            // Format with folder icon after name if it's a folder
            if track.is_folder {
                output.push_str(&format!("{}{}{} {}\n", 
                    prefix, colored_connector, colored_name, FOLDER_ICON));
                // Add to parent stack
                if parent_stack.len() <= depth {
                    parent_stack.push(&track.name.0);
                }
            } else {
                output.push_str(&format!("{}{}{}\n", 
                    prefix, colored_connector, colored_name));
            }
        }
        
        output
    }
}
