//! Track list management
//!
//! A TrackList represents a collection of tracks that can be matched against
//! and organized into a template structure.

use std::collections::HashMap;
use std::fmt;
use colored::Colorize;
use crate::track::{Track, SortStatus};

/// Display mode for track names
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DisplayMode {
    /// Show full track names with all prefixes
    FullName,
    /// Show only the relevant part, removing prefix and parent hierarchy
    Hierarchy,
}

/// A list of tracks that can be matched and organized
#[derive(Debug, Clone)]
pub struct TrackList {
    /// All tracks indexed by name
    tracks: HashMap<String, Track>,
    
    /// Track order (for maintaining insertion order)
    track_order: Vec<String>,
    
    /// Display mode for formatting
    display_mode: DisplayMode,
}

impl Default for TrackList {
    fn default() -> Self {
        Self::new()
    }
}

impl TrackList {
    /// Create a new empty track list with FullName display mode
    pub fn new() -> Self {
        Self {
            tracks: HashMap::new(),
            track_order: Vec::new(),
            display_mode: DisplayMode::FullName,
        }
    }
    
    /// Set the display mode
    pub fn set_display_mode(&mut self, mode: DisplayMode) {
        self.display_mode = mode;
    }
    
    /// Get the current display mode
    pub fn display_mode(&self) -> DisplayMode {
        self.display_mode
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
    
    /// Extract hierarchy name by removing prefix and parent parts
    fn extract_hierarchy_name(&self, track: &Track, parent: Option<&Track>) -> String {
        let name = track.name.as_str();
        let mut result = name.to_string();
        
        // Remove common prefixes (D, B, G, GTR E, K, V, VF, U, A)
        // Check longer prefixes first (GTR E before G)
        let prefixes = [
            "GTR E ", "D ", "B ", "G ", "K ", "V ", "VF ", "U ", "A ",
            "GTR E\t", "D\t", "B\t", "G\t", "K\t", "V\t", "VF\t", "U\t", "A\t",
        ];
        for prefix in &prefixes {
            if result.starts_with(prefix) {
                result = result[prefix.len()..].to_string();
                break;
            }
        }
        
        // Remove track type from the name if present (e.g., " (BUS)", " (SUM)")
        if let Some(track_type) = &track.track_type {
            let type_suffix = format!(" ({})", track_type);
            if result.ends_with(&type_suffix) {
                result = result[..result.len() - type_suffix.len()].to_string();
            }
        }
        
        // If we have a parent, remove the parent's hierarchy name from the beginning
        if let Some(parent_track) = parent {
            let parent_hierarchy = self.extract_hierarchy_name(parent_track, None);
            
            // Remove track type from parent hierarchy for comparison
            let mut parent_clean = parent_hierarchy.clone();
            if let Some(parent_type) = &parent_track.track_type {
                let parent_type_suffix = format!(" ({})", parent_type);
                if parent_clean.ends_with(&parent_type_suffix) {
                    parent_clean = parent_clean[..parent_clean.len() - parent_type_suffix.len()].to_string();
                }
            }
            
            // Remove parent hierarchy name if it appears at the start
            if !parent_clean.is_empty() && result.starts_with(&parent_clean) {
                result = result[parent_clean.len()..].to_string();
                // Remove leading space
                if result.starts_with(' ') {
                    result = result[1..].to_string();
                }
            } else if !parent_clean.is_empty() {
                // Try removing words from parent hierarchy in order
                // e.g., "Kick In" -> remove "Kick" -> "In"
                // e.g., "GTR E Lead" -> remove "GTR" then "E" -> "Lead"
                let parent_words: Vec<&str> = parent_clean.split_whitespace().collect();
                for parent_word in &parent_words {
                    // Check if result starts with this word (with optional space after)
                    if result.starts_with(parent_word) {
                        let word_len = parent_word.len();
                        result = result[word_len..].to_string();
                        // Remove leading space
                        if result.starts_with(' ') {
                            result = result[1..].to_string();
                        }
                        // Continue to next word (don't break)
                    }
                }
            }
        }
        
        result.trim().to_string()
    }
    
    /// Format the track list in a creative colored list format
    fn fmt_list_recursive(&self, f: &mut fmt::Formatter, track: &Track, prefix: &str, is_last: bool, parent: Option<&Track>) -> fmt::Result {
        let children = self.get_children(&track.name);
        let color_group = self.get_track_color_group(track);
        
        // Draw the tree connector
        let connector = if is_last { "└── " } else { "├── " };
        write!(f, "{}{}", prefix, connector)?;
        
        // Track name and type (colored)
        let track_display = match self.display_mode {
            DisplayMode::FullName => {
                let mut name = track.name.clone();
                // Replace "G " prefix with "GTR E " if the name contains "GTR E" but doesn't already start with it
                if name.starts_with("G ") {
                    let after_prefix = &name[2..]; // After "G "
                    if after_prefix.starts_with("GTR E") {
                        // Already has GTR E, just remove the "G " prefix
                        name = after_prefix.to_string();
                    } else if after_prefix.contains("GTR E") {
                        // Contains GTR E but not at start, replace "G " with "GTR E "
                        name = format!("GTR E {}", after_prefix);
                    }
                }
                if let Some(track_type) = &track.track_type {
                    name.push_str(&format!(" ({})", track_type));
                }
                name
            }
            DisplayMode::Hierarchy => {
                let mut name = self.extract_hierarchy_name(track, parent);
                // Don't show track type if name is empty (it's redundant)
                if !name.is_empty() {
                    if let Some(track_type) = &track.track_type {
                        name.push_str(&format!(" ({})", track_type));
                    }
                } else if let Some(track_type) = &track.track_type {
                    // If name is empty, just show the type
                    name = format!("({})", track_type);
                }
                name
            }
        };
        write!(f, "{}", self.color_track_name(&track_display, color_group))?;
        
        // Show first take inline if exists, with lighter color in brackets
        if let Some(first_take) = track.takes.first() {
            let take_brackets = format!("[{}]", first_take.name);
            write!(f, " : {}", self.color_take_name(&take_brackets, color_group))?;
            if let Some(index) = first_take.index {
                write!(f, " [{}]", index)?;
            }
        }
        
        writeln!(f)?;
        
        // Calculate prefix for children
        let child_prefix = if is_last {
            format!("{}    ", prefix)
        } else {
            format!("{}│   ", prefix)
        };
        
        // Show additional takes as a tree (if more than one)
        if track.takes.len() > 1 {
            for (i, take) in track.takes.iter().skip(1).enumerate() {
                let is_last_take = i == track.takes.len() - 2 && track.sends.is_empty() && track.receives.is_empty() && children.is_empty();
                let take_connector = if is_last_take { "└── " } else { "├── " };
                write!(f, "{}{}Take: {}", child_prefix, take_connector, take.name)?;
                if let Some(index) = take.index {
                    write!(f, " [{}]", index)?;
                }
                writeln!(f)?;
            }
        }
        
        // Sends
        for (i, send) in track.sends.iter().enumerate() {
            let is_last_send = i == track.sends.len() - 1 && track.receives.is_empty() && children.is_empty();
            let send_connector = if is_last_send { "└── " } else { "├── " };
            write!(f, "{}{}Send → {}", child_prefix, send_connector, send.target_track)?;
            if let Some(level) = send.level {
                write!(f, " ({:.2})", level)?;
            }
            writeln!(f)?;
        }
        
        // Receives
        for (i, receive) in track.receives.iter().enumerate() {
            let is_last_receive = i == track.receives.len() - 1 && children.is_empty();
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
            self.fmt_list_recursive(f, child, &child_prefix, is_last_child, Some(track))?;
        }
        
        Ok(())
    }
    
    /// Get the color for a track based on its group prefix
    fn get_track_color_group(&self, track: &Track) -> TrackColorGroup {
        let name = track.name.as_str();
        
        // Check for BGVs first (before general vocals)
        if name.starts_with("V BGV") || name.starts_with("V Background Vocals") || name.contains("BGV") {
            TrackColorGroup::BGV
        }
        // Check for VocalFX
        else if name.starts_with("VF ") || name.contains("VocalFX") || name.contains("Vocal FX") {
            TrackColorGroup::VocalFX
        }
        // Check for drums (D prefix or drum-related keywords)
        else if name.starts_with("D ") || name.contains("Drums") || name.contains("Kick") || name.contains("Snare") || name.contains("Tom") || name.contains("Cymbals") || name.contains("Rooms") || name.contains("Percussion") || name.contains("OH") || name.contains("HiHat") {
            TrackColorGroup::Drums
        }
        // Check for bass (B prefix)
        else if name.starts_with("B ") || name.contains("Bass") {
            TrackColorGroup::Bass
        }
        // Check for guitar (G prefix, GTR, or GTR E)
        else if name.starts_with("G ") || name.starts_with("GTR E ") || name.contains("GTR") || name.contains("Guitar") {
            TrackColorGroup::Guitar
        }
        // Check for acoustic (A prefix)
        else if name.starts_with("A ") || name.contains("Acoustic") {
            TrackColorGroup::Acoustic
        }
        // Check for keys (K prefix)
        else if name.starts_with("K ") || name.contains("Keys") || name.contains("Piano") || name.contains("Synth") {
            TrackColorGroup::Keys
        }
        // Check for vocals (V prefix, but not BGV)
        else if name.starts_with("V ") && !name.contains("BGV") && !name.contains("Background") {
            TrackColorGroup::Vocals
        }
        // Check for unsorted (U prefix)
        else if name.starts_with("U ") || name.contains("Unsorted") {
            TrackColorGroup::Unsorted
        }
        else {
            TrackColorGroup::Default
        }
    }
    
    /// Color a string based on track color group
    fn color_track_name(&self, text: &str, group: TrackColorGroup) -> colored::ColoredString {
        match group {
            TrackColorGroup::Drums => text.red(),
            TrackColorGroup::Bass => text.yellow(),
            TrackColorGroup::Guitar => text.blue(),
            TrackColorGroup::Acoustic => text.cyan(),
            TrackColorGroup::Keys => text.green(),
            TrackColorGroup::Vocals => text.magenta(),
            TrackColorGroup::BGV => text.bright_magenta(),
            TrackColorGroup::VocalFX => text.magenta(),
            TrackColorGroup::Unsorted => text.bright_black(),
            TrackColorGroup::Default => text.normal(),
        }
    }
    
    /// Color a string with lighter variant for take names
    fn color_take_name(&self, text: &str, group: TrackColorGroup) -> colored::ColoredString {
        match group {
            // Use truecolor RGB for lighter shades
            TrackColorGroup::Drums => text.truecolor(255, 150, 150), // Light red
            TrackColorGroup::Bass => text.truecolor(255, 255, 150), // Light yellow
            TrackColorGroup::Guitar => text.truecolor(150, 150, 255), // Light blue
            TrackColorGroup::Acoustic => text.truecolor(150, 255, 255), // Light cyan
            TrackColorGroup::Keys => text.truecolor(150, 255, 150), // Light green
            TrackColorGroup::Vocals => text.truecolor(255, 150, 255), // Light magenta
            TrackColorGroup::BGV => text.truecolor(255, 150, 255), // Light magenta
            TrackColorGroup::VocalFX => text.truecolor(255, 150, 255), // Light magenta
            TrackColorGroup::Unsorted => text.truecolor(200, 200, 200), // Light gray
            TrackColorGroup::Default => text.normal(),
        }
    }
}

/// Color groups for tracks
#[derive(Debug, Clone, Copy)]
enum TrackColorGroup {
    Drums,
    Bass,
    Guitar,
    Acoustic,
    Keys,
    Vocals,
    BGV,
    VocalFX,
    Unsorted,
    Default,
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
            self.fmt_list_recursive(f, root, "", is_last, None)?;
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
