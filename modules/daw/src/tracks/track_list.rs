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
        
        // Helper function to determine group color based on track
        // Also takes parent track names for better context
        fn get_group_color(track: &Track, display_name: &str, parent_stack: &[String]) -> Option<colored::ColoredString> {
            use colored::Colorize;
            use crate::tracks::MetadataKey;
            
            // Try to parse ItemProperties from ext_state
            if let Some(ref ext_state) = track.ext_state {
                if let Ok(props) = serde_json::from_str::<serde_json::Value>(ext_state) {
                    if let Some(group_prefix) = props.get("group_prefix").and_then(|v| v.as_str()) {
                        let group_prefix = group_prefix.to_lowercase();
                        if group_prefix.contains("kick") || group_prefix.contains("snare") || 
                           group_prefix.contains("tom") || group_prefix.contains("cymbal") || 
                           group_prefix.contains("room") || group_prefix == "k" || 
                           group_prefix == "s" || group_prefix == "t" || 
                           group_prefix == "c" || group_prefix == "r" {
                            return Some(display_name.red().bold());
                        }
                        if group_prefix.contains("bass") || group_prefix == "b" {
                            return Some(display_name.yellow().bold());
                        }
                        if group_prefix.contains("guitar electric") || group_prefix.contains("gtr e") {
                            return Some(display_name.blue().bold());
                        }
                        if group_prefix.contains("guitar acoustic") || group_prefix.contains("gtr a") {
                            return Some(display_name.truecolor(0, 128, 128).bold());
                        }
                        if group_prefix.contains("keys") {
                            return Some(display_name.green().bold());
                        }
                        if group_prefix.contains("synth") {
                            return Some(display_name.purple().bold());
                        }
                        if group_prefix.contains("vocal") || group_prefix == "v" {
                            if group_prefix.contains("bgv") {
                                return Some(display_name.purple().bold());
                            }
                            return Some(display_name.truecolor(255, 192, 203).bold());
                        }
                    }
                }
            }
            
            // Try to use metadata or parent stack context
            let parent_meta = track.metadata.get(&crate::tracks::MetadataKey::from(crate::tracks::MetadataKey::PARENT)).map(|s| s.to_lowercase());
            
            // Check for drum context
            let is_drum_context = parent_stack.iter().any(|p| {
                let p = p.to_lowercase();
                p.contains("kick") || p.contains("snare") || p.contains("tom") || 
                p.contains("cymbal") || p.contains("room") || p.contains("drum")
            }) || parent_meta.as_ref().map_or(false, |p| {
                p.contains("kick") || p.contains("snare") || p.contains("tom") || 
                p.contains("cymbal") || p.contains("room") || p.contains("drum")
            });

            if is_drum_context {
                return Some(display_name.red().bold());
            }

            // Check for bass context
            let is_bass_context = parent_stack.iter().any(|p| p.to_lowercase().contains("bass")) ||
                                parent_meta.as_ref().map_or(false, |p| p.contains("bass"));
            
            if is_bass_context {
                return Some(display_name.yellow().bold());
            }

            // Check for guitar context
            let is_guitar_context = parent_stack.iter().any(|p| {
                let p = p.to_lowercase();
                p.contains("guitar") || p.contains("gtr")
            }) || parent_meta.as_ref().map_or(false, |p| {
                p.contains("guitar") || p.contains("gtr")
            });

            if is_guitar_context {
                let is_acoustic = parent_stack.iter().any(|p| p.to_lowercase().contains("acoustic")) ||
                                 parent_meta.as_ref().map_or(false, |p| p.contains("acoustic"));
                if is_acoustic {
                    return Some(display_name.truecolor(0, 128, 128).bold());
                }
                return Some(display_name.blue().bold());
            }

            // Check for keys context
            let is_keys_context = parent_stack.iter().any(|p| {
                let p = p.to_lowercase();
                p.contains("keys") || p.contains("piano")
            }) || parent_meta.as_ref().map_or(false, |p| {
                p.contains("keys") || p.contains("piano")
            });

            if is_keys_context {
                return Some(display_name.green().bold());
            }

            // Check for synth context
            let is_synth_context = parent_stack.iter().any(|p| p.to_lowercase().contains("synth")) ||
                                 parent_meta.as_ref().map_or(false, |p| p.contains("synth"));
            
            if is_synth_context {
                return Some(display_name.purple().bold());
            }

            // Check for vocal context
            let is_vocal_context = parent_stack.iter().any(|p| p.to_lowercase().contains("vocal") || p.to_lowercase() == "v" || p.to_lowercase() == "vocals") ||
                                 parent_meta.as_ref().map_or(false, |p| p.contains("vocal") || p == "v" || p == "vocals");
            
            if is_vocal_context {
                let is_bgv = parent_stack.iter().any(|p| p.to_lowercase().contains("bgv")) ||
                            parent_meta.as_ref().map_or(false, |p| p.contains("bgv"));
                if is_bgv {
                    return Some(display_name.purple().bold());
                }
                return Some(display_name.truecolor(255, 192, 203).bold());
            }

            // Fallback: infer from track name itself
            let name_lower = display_name.to_lowercase();
            
            if name_lower.contains("kick") || name_lower.contains("snare") || 
               name_lower.contains("tom") || name_lower.contains("cymbal") || 
               name_lower.contains("room") || name_lower.contains("drum") ||
               name_lower == "in" || name_lower == "out" || name_lower == "trig" ||
               name_lower == "top" || name_lower == "bottom" || name_lower == "verb" ||
               name_lower == "sub" || name_lower == "amb" || name_lower == "rooms" ||
               name_lower == "highhat" || name_lower == "oh" || name_lower == "ride" ||
               name_lower == "1" || name_lower == "2" {
                return Some(display_name.red().bold());
            }
            
            if name_lower.contains("bass") || name_lower == "di" || name_lower == "amp" {
                return Some(display_name.yellow().bold());
            }
            
            if name_lower.contains("guitar") || name_lower.contains("gtr") {
                if name_lower.contains("acoustic") {
                    return Some(display_name.truecolor(0, 128, 128).bold()); // Sea green
                }
                return Some(display_name.blue().bold());
            }
            
            if name_lower.contains("piano") || name_lower.contains("keys") || name_lower.contains("organ") {
                return Some(display_name.green().bold());
            }
            
            if name_lower.contains("synth") || name_lower.contains("pad") {
                return Some(display_name.purple().bold());
            }
            
            if name_lower.contains("vocal") || name_lower == "lead" || name_lower == "double" {
                if name_lower.contains("bgv") {
                    return Some(display_name.purple().bold());
                }
                return Some(display_name.truecolor(255, 192, 203).bold()); // Pink
            }
            
            None
        }
        
        let mut output = String::new();
        let mut parent_stack: Vec<String> = Vec::new();
        
        // Pre-calculate absolute depths for all tracks
        let mut absolute_depths = Vec::with_capacity(tracks.len());
        let mut running_depth = 0;
        for track in tracks.iter() {
            absolute_depths.push(running_depth);
            running_depth += track.folder_depth_change.to_reaper_value();
            if running_depth < 0 { running_depth = 0; }
        }

        for (i, track) in tracks.iter().enumerate() {
            let depth = absolute_depths[i] as usize;
            
            // Update parent stack
            while parent_stack.len() > depth {
                parent_stack.pop();
            }
            
            // Determine if this is the last track at this depth (among its siblings)
            let is_last = {
                let mut found_sibling = false;
                for j in (i + 1)..tracks.len() {
                    let next_depth = absolute_depths[j] as usize;
                    if next_depth < depth {
                        break; // End of this folder
                    }
                    if next_depth == depth {
                        found_sibling = true;
                        break;
                    }
                }
                !found_sibling
            };
            
            // Build prefix
            let mut prefix = String::new();
            for j in 0..depth {
                // Check if any track at depth j later in the list is a sibling of the parent at depth j
                let has_more_siblings_at_depth = {
                    let mut found = false;
                    for k in (i + 1)..tracks.len() {
                        let next_depth = absolute_depths[k] as usize;
                        if next_depth < j {
                            break;
                        }
                        if next_depth == j {
                            found = true;
                            break;
                        }
                    }
                    found
                };
                
                if has_more_siblings_at_depth {
                    prefix.push_str("│   ");
                } else {
                    prefix.push_str("    ");
                }
            }
            
            let connector = if is_last { "└── " } else { "├── " };
            let colored_connector = connector.bright_black();
            
            // Shorten name if it starts with a parent's name
            let mut display_name = track.name.0.clone();
            if !parent_stack.is_empty() {
                for parent in parent_stack.iter().rev() {
                    let parent_clean = parent
                        .replace(" (SUM)", "")
                        .replace(" (BUS)", "")
                        .replace(" Sum", "")
                        .replace(" Bus", "");
                    
                    let parent_lower = parent_clean.to_lowercase();
                    let search_prefixes = vec![
                        parent_lower.clone() + " ",
                        if parent_lower.ends_with('s') {
                            parent_lower[..parent_lower.len()-1].to_string() + " "
                        } else {
                            parent_lower.clone() + "s "
                        }
                    ];

                    let mut found_prefix = false;
                    for prefix in search_prefixes {
                        if display_name.to_lowercase().starts_with(&prefix) {
                            let remaining = &display_name[prefix.len()..];
                            if remaining.chars().next().map_or(false, |c| c.is_ascii_digit()) {
                                // If it's numeric, use first char of parent as prefix (e.g., Tom 1 -> T1)
                                if let Some(first_char) = parent_clean.chars().next() {
                                    display_name = format!("{}{}", first_char.to_uppercase(), remaining);
                                } else {
                                    display_name = remaining.to_string();
                                }
                            } else {
                                display_name = remaining.to_string();
                            }
                            found_prefix = true;
                            break;
                        }
                    }
                    if found_prefix { break; }
                }
            }
            
            // Clean up common labels for display
            display_name = display_name
                .replace(" (SUM)", " Sum")
                .replace("(SUM)", "Sum")
                .replace(" (BUS)", " Bus")
                .replace("(BUS)", "Bus");
            
            display_name = display_name.trim().to_string();

            let colored_name = get_group_color(track, &display_name, &parent_stack)
                .unwrap_or_else(|| display_name.normal());
            
            if track.is_folder {
                output.push_str(&format!("{}{}{} {}\n", prefix, colored_connector, colored_name, FOLDER_ICON));
                parent_stack.push(track.name.0.clone());
            } else {
                output.push_str(&format!("{}{}{}\n", prefix, colored_connector, colored_name));
            }
        }
        
        output
    }
}
