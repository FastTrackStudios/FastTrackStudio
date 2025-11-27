//! Track list management
//!
//! A TrackList represents a collection of tracks that can be matched against
//! and organized into a template structure.
//!
//! Uses the naming-convention crate to parse track names and extract hierarchy information.

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;
use colored::Colorize;
use naming_convention::{SimpleParser, FullGroup, create_default_groups};
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
#[derive(Clone)]
pub struct TrackList {
    /// All tracks indexed by name
    tracks: HashMap<String, Track>,
    
    /// Track order (for maintaining insertion order)
    track_order: Vec<String>,
    
    /// Display mode for formatting
    display_mode: DisplayMode,
    
    /// Parser for extracting naming convention components
    /// Uses Arc to allow sharing and cloning without duplicating the groups
    parser: Arc<SimpleParser>,
}

impl std::fmt::Debug for TrackList {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TrackList")
            .field("tracks", &self.tracks)
            .field("track_order", &self.track_order)
            .field("display_mode", &self.display_mode)
            .field("parser", &"<SimpleParser>")
            .finish()
    }
}

impl Default for TrackList {
    fn default() -> Self {
        Self::new()
    }
}

impl TrackList {
    /// Create a new empty track list with FullName display mode
    /// Uses default groups from naming-convention crate
    pub fn new() -> Self {
        Self::with_groups(create_default_groups())
    }
    
    /// Create a new track list with custom groups
    /// This allows injecting different group configurations
    pub fn with_groups(groups: Vec<FullGroup>) -> Self {
        let parser = Arc::new(SimpleParser::new(groups));
        Self {
            tracks: HashMap::new(),
            track_order: Vec::new(),
            display_mode: DisplayMode::FullName,
            parser,
        }
    }
    
    /// Create a new track list with a custom parser
    /// This allows full control over the parser configuration
    pub fn with_parser(parser: SimpleParser) -> Self {
        Self {
            tracks: HashMap::new(),
            track_order: Vec::new(),
            display_mode: DisplayMode::FullName,
            parser: Arc::new(parser),
        }
    }
    
    /// Get a reference to the parser
    pub fn parser(&self) -> &SimpleParser {
        &self.parser
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
    
    /// Build hierarchy name from parsed TrackName components
    /// This extracts the relevant parts (sub_type, multi_mic, increment, etc.) without the group prefix
    /// Uses the same logic as the formatter but excludes group_prefix
    fn build_hierarchy_name_from_parsed(&self, parsed: &naming_convention::TrackName) -> String {
        let mut parts = Vec::new();
        
        // Track what we've already added to avoid duplicates
        let mut added_sub_type = String::new();
        
        // Add sub_type (e.g., "Kick", "Snare", "Bass Guitar")
        // Use the last sub_type (most specific) for hierarchy display
        // But remove "GTR E" if it's part of the sub_type (it should be prefix, not sub_type)
        // Also remove the group prefix if it appears in the sub_type (e.g., "Bass Guitar" -> "Guitar" when prefix is "Bass")
        if let Some(sub_types) = &parsed.sub_type {
            if let Some(last_sub_type) = sub_types.last() {
                let mut sub_type = last_sub_type.clone();
                
                // Remove group prefix from sub_type if it appears at the start or anywhere
                // This handles cases like "Bass Guitar" when prefix is "Bass" -> should be just "Guitar"
                // Also handle cases where prefix is a single letter (like "B") but sub_type contains the full group name (like "Bass Guitar")
                if let Some(prefix) = &parsed.group_prefix {
                    let prefix_with_space = format!("{} ", prefix);
                    // First, try removing from the start
                    if sub_type.starts_with(&prefix_with_space) {
                        sub_type = sub_type[prefix_with_space.len()..].trim_start().to_string();
                    } else if &sub_type == prefix {
                        // Sub_type is exactly the prefix - don't add it
                        sub_type = String::new();
                    } else {
                        // Check if prefix is a single letter and sub_type starts with a word that starts with that letter
                        // This handles "Bass Guitar" when prefix is "B" -> but we want to keep "Bass" for top-level tracks
                        // Only remove if it's clearly redundant (e.g., "Bass Bass" -> "Bass")
                        // For "Bass Guitar", keep "Bass" as it's the group name, not redundant
                        if prefix.len() == 1 {
                            let words: Vec<&str> = sub_type.split_whitespace().collect();
                            if !words.is_empty() {
                                let first_word = words[0];
                                // Check if first word starts with the prefix letter
                                if first_word.starts_with(prefix) && first_word.len() > prefix.len() {
                                    // Only remove if the sub_type is a single word that matches common group names
                                    // e.g., "Bass" sub_type when prefix is "B" -> could be redundant
                                    // But "Bass Guitar" should keep "Bass" as it's part of a compound name
                                    // For now, keep all compound names (multiple words) and only remove single-word matches
                                    // that are clearly redundant (this is handled elsewhere in the logic)
                                    // So we don't remove here - let the rest of the logic handle it
                                    // Otherwise, keep the word (it's part of a compound name like "Bass Guitar")
                                }
                            }
                        }
                        
                        // Also check if prefix appears anywhere in the sub_type
                        if sub_type.contains(&prefix_with_space) {
                            // Prefix appears somewhere in the middle - remove it
                            sub_type = sub_type.replace(&prefix_with_space, " ").trim().to_string();
                        } else if sub_type.contains(prefix) && &sub_type != prefix {
                            // Prefix appears as a word (not at start) - try to remove it
                            // This handles "Guitar Bass" -> "Guitar" (though this case is less common)
                            let words: Vec<&str> = sub_type.split_whitespace().collect();
                            let filtered_words: Vec<&str> = words.iter()
                                .filter(|w| !w.eq_ignore_ascii_case(prefix))
                                .copied()
                                .collect();
                            if !filtered_words.is_empty() {
                                sub_type = filtered_words.join(" ");
                            } else {
                                sub_type = String::new();
                            }
                        }
                    }
                }
                
                // Remove "GTR E " if it appears at the start of sub_type
                // Special case: "GTR E Clean" -> extract "Clean" as the arrangement, not sub_type
                // Special case: "GTR E Lead" -> extract "Lead" as the arrangement, not sub_type
                if sub_type.starts_with("GTR E ") {
                    // Extract the part after "GTR E " - this is likely the arrangement
                    let after_gtr_e = sub_type[6..].trim_start().to_string();
                    // If it's a known arrangement word (like "Clean", "Lead"), treat it as arrangement
                    // Add it to parts so it appears in the hierarchy name
                    if after_gtr_e == "Clean" || after_gtr_e == "Lead" {
                        // Add as arrangement (will be checked below to avoid duplicates)
                        // But also mark it so we don't add it twice
                        parts.push(after_gtr_e.clone());
                        added_sub_type = after_gtr_e.clone();
                    } else {
                        // It's a real sub_type like "GTR E Guitar" -> "Guitar"
                        parts.push(after_gtr_e.clone());
                        added_sub_type = after_gtr_e;
                    }
                } else if sub_type == "GTR E" {
                    // Just "GTR E" - don't add anything
                } else if !sub_type.is_empty() {
                    // Don't add sub_type if it matches the group prefix (redundant)
                    // e.g., "Bass" sub_type when prefix is "Bass" -> don't add "Bass" again
                    let should_add = if let Some(prefix) = &parsed.group_prefix {
                        !sub_type.eq_ignore_ascii_case(prefix)
                    } else {
                        true
                    };
                    if should_add {
                        parts.push(sub_type.clone());
                        added_sub_type = sub_type;
                    }
                }
            }
        }
        
        // Add arrangement if it doesn't match sub_type (e.g., "Lead", "Clean")
        // Also don't add if it matches the group prefix (e.g., "Bass" arrangement when prefix is "Bass")
        if let Some(arrangement) = &parsed.arrangement {
            let should_include = if let Some(prefix) = &parsed.group_prefix {
                // Don't add arrangement if it matches the group prefix
                if arrangement.eq_ignore_ascii_case(prefix) {
                    false
                } else if !added_sub_type.is_empty() {
                    // Don't add if we already added it as sub_type (after removing "GTR E ")
                    !added_sub_type.eq_ignore_ascii_case(arrangement)
                } else if let Some(ref sub_types) = parsed.sub_type {
                    !sub_types.iter().any(|st| st.eq_ignore_ascii_case(arrangement))
                } else {
                    true
                }
            } else if !added_sub_type.is_empty() {
                // Don't add if we already added it as sub_type (after removing "GTR E ")
                !added_sub_type.eq_ignore_ascii_case(arrangement)
            } else if let Some(ref sub_types) = parsed.sub_type {
                !sub_types.iter().any(|st| st.eq_ignore_ascii_case(arrangement))
            } else {
                true
            };
            if should_include {
                parts.push(arrangement.clone());
            }
        }
        
        // Add multi_mic (e.g., "In", "Out", "Top", "Bottom", "DI")
        // Join multiple mic positions with space (not comma) for hierarchy display
        if let Some(multi_mics) = &parsed.multi_mic {
            for multi_mic in multi_mics {
                parts.push(multi_mic.clone());
            }
        }
        
        // Add increment (e.g., "1", "2" for Tom 1, Tom 2)
        // For BGVs, we want to show "BGV 1" not just "1"
        if let Some(increment) = &parsed.increment {
            // Check if this is a BGV track (sub_type is "BGVs" but we want to show "BGV" not "BGVs")
            let is_bgv = if let Some(sub_types) = &parsed.sub_type {
                sub_types.iter().any(|st| st == "BGVs")
            } else {
                false
            };
            
            // For BGVs, always add "BGV" before the increment if parts is empty
            // This handles cases where sub_type was removed or not added
            if is_bgv && parts.is_empty() {
                parts.push("BGV".to_string());
            }
            
            // Add increment if we have other parts OR if we have a sub_type (for numbered tracks like BGV1, Tom 1)
            // OR if this is a BGV track (we just added "BGV" above)
            if !parts.is_empty() || parsed.sub_type.is_some() || is_bgv {
                parts.push(increment.clone());
            }
        }
        
        // Add channel (e.g., "L", "R")
        if let Some(channel) = &parsed.channel {
            parts.push(channel.clone());
        }
        
        // Add unparsed words if present (these should be included)
        if let Some(unparsed_words) = &parsed.unparsed_words {
            for word in unparsed_words {
                parts.push(word.clone());
            }
        }
        
        let mut result = parts.join(" ");
        
        // Remove duplicate consecutive words (e.g., "Guitar Guitar" -> "Guitar")
        let words: Vec<&str> = result.split_whitespace().collect();
        let mut deduplicated_words = Vec::new();
        for word in words {
            if deduplicated_words.is_empty() || deduplicated_words.last() != Some(&word) {
                deduplicated_words.push(word);
            }
        }
        result = deduplicated_words.join(" ");

        // Remove "GTR E " if it appears at the start (should be prefix, not part of hierarchy)
        // This handles cases where parser includes "GTR E" in sub_type
        if result.starts_with("GTR E ") {
            result = result[6..].trim_start().to_string();
        } else if result == "GTR E" {
            result = String::new();
        }
        
        // Remove group prefix from result if it appears at the start
        // This handles cases where parser includes the prefix in sub_type or arrangement
        // e.g., "Bass Guitar" when prefix is "Bass" -> should be just "Guitar"
        if let Some(prefix) = &parsed.group_prefix {
            let prefix_with_space = format!("{} ", prefix);
            // Try removing from start first
            if result.starts_with(&prefix_with_space) {
                result = result[prefix_with_space.len()..].trim_start().to_string();
            } else if &result == prefix {
                // Result is exactly the prefix - clear it
                result = String::new();
            } else if result.contains(&prefix_with_space) {
                // Prefix appears somewhere in the result - remove it
                result = result.replace(&prefix_with_space, " ").trim().to_string();
            } else if result.contains(prefix) && &result != prefix {
                // Prefix appears as a word (not at start) - try to remove it
                let words: Vec<&str> = result.split_whitespace().collect();
                let filtered_words: Vec<&str> = words.iter()
                    .filter(|w| !w.eq_ignore_ascii_case(prefix))
                    .copied()
                    .collect();
                if !filtered_words.is_empty() {
                    result = filtered_words.join(" ");
                } else {
                    result = String::new();
                }
            }
        }

        result
    }
    
    /// Extract hierarchy name by parsing the track name and removing parent parts
    /// Uses the naming-convention parser to extract components instead of hardcoded logic
    fn extract_hierarchy_name(&self, track: &Track, parent: Option<&Track>) -> String {
        // Parse the track name using the configured parser
        let parsed = self.parser.parse(&track.name);
        
        // Special case: For top-level Bass track with sub_type "Bass Guitar", show "Bass" not "Guitar"
        // This handles the case where "Bass" defaults to "Bass Guitar" but we want to show "Bass" at the top level
        if parent.is_none() && parsed.group_prefix.as_ref().map(|p| p == "B").unwrap_or(false) {
            if let Some(sub_types) = &parsed.sub_type {
                if sub_types.len() == 1 && sub_types[0] == "Bass Guitar" {
                    // Top-level Bass track with "Bass Guitar" sub_type - show "Bass"
                    return "Bass".to_string();
                }
            }
        }
        
        // Special case: For top-level "GTR E" track, show "GTR E" not empty
        if parent.is_none() {
            // Check the track name directly first (most reliable)
            if track.name.contains("GTR E") {
                return "GTR E".to_string();
            }
            
            if let Some(prefix) = &parsed.group_prefix {
                // Check if this is "GTR E" - the prefix might be "G" but sub_type might be "GTR E"
                if prefix == "G" {
                    if let Some(sub_types) = &parsed.sub_type {
                        if let Some(last_sub_type) = sub_types.last() {
                            if last_sub_type == "GTR E" || last_sub_type.starts_with("GTR E ") {
                                return "GTR E".to_string();
                            }
                        }
                    }
                    // Also check original name for "GTR E"
                    if let Some(original) = &parsed.original_name {
                        if original.contains("GTR E") {
                            return "GTR E".to_string();
                        }
                    }
                } else if prefix == "V BGVs" || prefix == "V" {
                    // Check if this is BGVs - if sub_type is empty or just "BGVs", show "BGVs"
                    if let Some(sub_types) = &parsed.sub_type {
                        if sub_types.is_empty() || (sub_types.len() == 1 && sub_types[0] == "BGVs") {
                            return "BGVs".to_string();
                        }
                    } else {
                        // No sub_type but prefix is "V BGVs" - show "BGVs"
                        if prefix == "V BGVs" {
                            return "BGVs".to_string();
                        }
                    }
                }
            }
        }
        
        // First, try to build from parsed components
        let mut result = self.build_hierarchy_name_from_parsed(&parsed);
        
        // Remove track type from result if present (we'll add it back in display logic if needed)
        if let Some(track_type) = &track.track_type {
            let type_suffix = format!(" ({})", track_type);
            if result.ends_with(&type_suffix) {
                result = result[..result.len() - type_suffix.len()].to_string();
            }
        }
        
        // Always ensure we remove the group prefix if it's detected in the original name
        // Handle both single-word prefixes (like "D", "B") and multi-word prefixes (like "GTR E")
        let name_without_type = if let Some(track_type) = &track.track_type {
            track.name.replace(&format!(" ({})", track_type), "")
        } else {
            track.name.clone()
        };
        
        if let Some(prefix) = &parsed.group_prefix {
            // Check if result still contains the prefix (shouldn't happen, but be defensive)
            let prefix_with_space = format!("{} ", prefix);
            let prefix_with_tab = format!("{}\t", prefix);
            
            if result.starts_with(&prefix_with_space) {
                result = result[prefix_with_space.len()..].trim_start().to_string();
            } else if result.starts_with(&prefix_with_tab) {
                result = result[prefix_with_tab.len()..].trim_start().to_string();
            }
            // Don't remove prefix if it's not followed by a space (e.g., "Bass" shouldn't have "B" removed)
            // The prefix_with_space and prefix_with_tab checks above already handle "B " prefix removal
            // If result starts with just "B" (not "B "), it's likely part of a word like "Bass", so don't remove it
            
            // Also handle multi-word prefixes that might be in sub_type (e.g., "GTR E" when prefix is "G")
            // If sub_type contains "GTR E", remove "GTR E " from it (it should be prefix, not part of hierarchy)
            // This handles cases where parser includes "GTR E" in sub_type
            if prefix == "G" {
                if let Some(sub_types) = &parsed.sub_type {
                    for sub_type in sub_types {
                        // If sub_type is "GTR E" or starts with "GTR E ", remove "GTR E " from result
                        if sub_type == "GTR E" {
                            // If result is just "GTR E", clear it
                            if result == "GTR E" {
                                result = String::new();
                            } else if result.starts_with("GTR E ") {
                                result = result[6..].trim_start().to_string();
                            }
                            break;
                        } else if sub_type.starts_with("GTR E ") {
                            // Remove "GTR E " from result if present
                            if result.starts_with("GTR E ") {
                                result = result[6..].trim_start().to_string();
                            }
                            break;
                        }
                    }
                }
            }
            
            // If result is empty or doesn't seem right, use fallback
            // But be more careful - if result already looks good (doesn't contain "GTR E " at start), don't overwrite it
            let should_use_fallback = result.is_empty() || 
               (name_without_type.starts_with(&prefix_with_space) && 
                !name_without_type[prefix_with_space.len()..].trim_start().starts_with(&result) &&
                result.starts_with("GTR E "));  // Only use fallback if result starts with "GTR E "
            
            if should_use_fallback {
                // Remove the prefix from the original name
                if name_without_type.starts_with(&prefix_with_space) {
                    result = name_without_type[prefix_with_space.len()..].trim_start().to_string();
                } else if name_without_type.starts_with(&prefix_with_tab) {
                    result = name_without_type[prefix_with_tab.len()..].trim_start().to_string();
                } else if name_without_type.starts_with(prefix) {
                    result = name_without_type[prefix.len()..].trim_start().to_string();
                } else {
                    result = name_without_type.clone();
                }
                
                // Also handle "GTR E" as a special case - always remove "GTR E " from fallback result
                // This ensures "G GTR E Clean DI L" -> "GTR E Clean DI L" -> "Clean DI L" (parent will remove "Clean" later)
                if result.starts_with("GTR E ") {
                    result = result[6..].trim_start().to_string();
                } else if result == "GTR E" {
                    result = String::new();
                }
            }
        } else if result.is_empty() {
            // No prefix detected, just use the name without track type
            result = name_without_type;
        }
        
        // Final cleanup: remove any remaining single-letter prefixes that might have leaked through
        // This handles cases like "G GTR E" where "G" is incorrectly extracted as prefix
        // Also remove "GTR E " if it appears (it should be part of prefix, not hierarchy name)
        // BUT: Don't remove if it would break a word (e.g., "B Bass" -> "Bass" is OK, but "Bass Guitar" -> "ass Guitar" is NOT)
        let single_letter_prefixes = ["G ", "D ", "B ", "K ", "V ", "VF ", "U ", "A "];
        for single_prefix in &single_letter_prefixes {
            if result.starts_with(single_prefix) {
                let remaining = &result[single_prefix.len()..];
                let prefix_letter = single_prefix.chars().next().unwrap();
                let remaining_trimmed = remaining.trim_start();
                
                // If removing the single-letter prefix leaves "GTR E", remove "GTR E " too
                if remaining_trimmed.starts_with("GTR E ") {
                    result = remaining_trimmed[6..].trim_start().to_string(); // Remove both "G " and "GTR E "
                } else if remaining_trimmed == "GTR E" {
                    result = String::new(); // Remove both "G " and "GTR E"
                } else if !remaining_trimmed.starts_with(prefix_letter) {
                    // Only remove if the remaining text doesn't start with the same letter
                    // This prevents "B Bass" -> "ass" (bad) but allows "B DI" -> "DI" (good)
                    result = remaining_trimmed.to_string();
                } else {
                    // The remaining text starts with the same letter as the prefix
                    // This could be "B Bass" where "Bass" is a complete word, or "Bass Guitar" where "Bass" is part of a word
                    // For "B Bass", we want to keep "B " (it's a prefix before the word "Bass")
                    // For "Bass Guitar", we don't want to remove "B" from "Bass"
                    // So we should NOT remove the prefix in this case - keep it as is
                    // The prefix removal will happen later in the hierarchy extraction
                }
                // Keep the prefix (it's part of a word like "Bass" or it's a valid prefix like "B Bass")
                break;
            }
        }
        
        // Clean result of "GTR E " and single-letter prefixes before parent removal
        // This ensures parent removal works on a clean result
        // Handle both "G GTR E Clean DI L" -> remove "G " -> "GTR E Clean DI L" -> remove "GTR E " -> "Clean DI L"
        // Use a more aggressive approach: remove "GTR E " first, then single-letter prefixes
        // This ensures "GTR E " is always removed even if it appears after other text
        
        // First, remove "GTR E " if it appears anywhere (more aggressive)
        if result.contains(" GTR E ") {
            result = result.replace(" GTR E ", " ");
        }
        if result.starts_with("GTR E ") {
            result = result[6..].trim_start().to_string();
        } else if result == "GTR E" {
            result = String::new();
        }
        
        // Then remove single-letter prefixes, but only if what follows is NOT a word that starts with that letter
        // This prevents removing "B " from "B Bass Guitar" (which would become "ass Guitar")
        // We should only remove prefixes when they're actual prefixes, not part of a word
        let single_letter_prefixes = ["G ", "D ", "B ", "K ", "V ", "VF ", "U ", "A "];
        for prefix in &single_letter_prefixes {
            if result.starts_with(prefix) {
                let remaining = &result[prefix.len()..];
                let prefix_letter = prefix.chars().next().unwrap();
                let remaining_trimmed = remaining.trim_start();
                
                // Only remove if the remaining text doesn't start with the same letter
                // This prevents "B Bass" -> "ass" (bad) but allows "B DI" -> "DI" (good)
                // Also allow removal if it's a known hierarchy word like "DI"
                // BUT: "B Bass" should become "Bass" (remove "B "), not "ass" (remove "B" from "Bass")
                // So we need to check if the remaining text is a complete word that starts with the prefix letter
                // For "B Bass", remaining is "Bass" which starts with "B", so we should NOT remove "B "
                // For "B DI", remaining is "DI" which doesn't start with "B", so we SHOULD remove "B "
                if !remaining_trimmed.starts_with(prefix_letter) {
                    // Safe to remove - remaining doesn't start with the same letter
                    result = remaining_trimmed.to_string();
                    break;
                } else if remaining_trimmed.starts_with("DI ") || 
                          (remaining_trimmed.starts_with("DI") && remaining_trimmed.len() == 2) {
                    // Special case: "B DI" -> "DI" is OK
                    result = remaining_trimmed.to_string();
                    break;
                }
                // Otherwise, keep the prefix (it's part of a word like "Bass" - "B Bass" should stay as "B Bass")
            }
        }
        
        // Final check: remove "GTR E " one more time in case it was added back
        if result.starts_with("GTR E ") {
            result = result[6..].trim_start().to_string();
        } else if result == "GTR E" {
            result = String::new();
        }
        
        // If we have a parent, remove the parent's hierarchy name from the beginning
        if let Some(parent_track) = parent {
            // Parse the parent track name and build its hierarchy name
            let parent_parsed = self.parser.parse(&parent_track.name);
            let mut parent_hierarchy = self.build_hierarchy_name_from_parsed(&parent_parsed);
            
            // Clean parent hierarchy of "GTR E " and single-letter prefixes too
            // Also remove group prefix from parent hierarchy if it appears (e.g., "Bass Bass" -> "Bass")
            for prefix in &single_letter_prefixes {
                if parent_hierarchy.starts_with(prefix) {
                    parent_hierarchy = parent_hierarchy[prefix.len()..].trim_start().to_string();
                    break;
                }
            }
            if parent_hierarchy.starts_with("GTR E ") {
                parent_hierarchy = parent_hierarchy[6..].trim_start().to_string();
            }
            
            // Remove group prefix from parent hierarchy if it appears
            // This handles cases like "Bass Bass" -> "Bass" when prefix is "Bass"
            // First, check for duplicate words (e.g., "Bass Bass" -> "Bass")
            let words: Vec<&str> = parent_hierarchy.split_whitespace().collect();
            if words.len() > 1 && words[0] == words[1] {
                // Duplicate word like "Bass Bass" -> "Bass"
                parent_hierarchy = words[0].to_string();
            } else if let Some(parent_prefix) = &parent_parsed.group_prefix {
                let parent_prefix_with_space = format!("{} ", parent_prefix);
                if parent_hierarchy.starts_with(&parent_prefix_with_space) {
                    parent_hierarchy = parent_hierarchy[parent_prefix_with_space.len()..].trim_start().to_string();
                } else if parent_hierarchy == *parent_prefix {
                    // Parent hierarchy is exactly the prefix - keep it as is (it's the group name)
                } else if parent_hierarchy.contains(&parent_prefix_with_space) {
                    // Prefix appears somewhere in the parent hierarchy - remove it
                    parent_hierarchy = parent_hierarchy.replace(&parent_prefix_with_space, " ").trim().to_string();
                }
            }
            
            // Remove track type from parent hierarchy for comparison
            let mut parent_clean = parent_hierarchy.clone();
            if let Some(parent_type) = &parent_track.track_type {
                let parent_type_suffix = format!(" ({})", parent_type);
                if parent_clean.ends_with(&parent_type_suffix) {
                    parent_clean = parent_clean[..parent_clean.len() - parent_type_suffix.len()].to_string();
                }
            }
            
            // Remove parent hierarchy name if it appears at the start
            // But preserve compound names like "Bass Guitar" when parent is "Bass"
            // Also handle "GTR E" as a special case - remove it from children
            if !parent_clean.is_empty() {
                // Special case: If parent is "GTR E", remove it from result
                if parent_clean == "GTR E" && result.starts_with("GTR E ") {
                    result = result[6..].trim_start().to_string();
                } else if !result.is_empty() && result.starts_with(&parent_clean) {
                    let remaining = result[parent_clean.len()..].trim_start();
                    // Only remove if it's a single word match (exact parent name)
                    // or if the remaining part is a short hierarchy word (possibly with increment)
                    let result_words: Vec<&str> = result.split_whitespace().collect();
                    let remaining_words: Vec<&str> = remaining.split_whitespace().collect();
                    let short_hierarchy_words = ["In", "Out", "Top", "Bottom", "L", "R", "C", "Trig"];
                    let technical_terms = ["DI", "MIDI", "FX", "AUX", "SUM", "BUS"];
                    let channel_indicators = ["L", "R", "C", "Left", "Right", "Center"];
                    
                    // Remove only if:
                    // 1. Result is exactly the parent (single word), OR
                    // 2. Remaining starts with a hierarchy word (possibly followed by increment/number), OR
                    // 3. Remaining starts with a technical term (e.g., "DI"), OR
                    // 4. Remaining starts with a technical term followed by a channel indicator (e.g., "DI L", "DI R")
                    // 5. Remaining is just a channel indicator (e.g., "R" when parent is "Lead")
                    if result_words.len() == 1 && result_words[0] == parent_clean {
                        result = remaining.to_string();
                    } else if !remaining_words.is_empty() {
                        let first_remaining = remaining_words[0];
                        // Check if first word is a hierarchy word
                        let is_hierarchy_word = short_hierarchy_words.contains(&first_remaining) ||
                            (first_remaining.len() <= 3 && !technical_terms.contains(&first_remaining));
                        
                        // Check if it's a technical term by itself (e.g., "DI")
                        let is_tech_term = technical_terms.contains(&first_remaining);
                        
                        // Check if it's a technical term followed by a channel (e.g., "DI L", "DI R")
                        let is_tech_term_with_channel = technical_terms.contains(&first_remaining) &&
                            remaining_words.len() >= 2 &&
                            channel_indicators.contains(&remaining_words[1]);
                        
                        // Check if it's just a channel indicator (e.g., "R" when parent is "Lead")
                        let is_just_channel = remaining_words.len() == 1 && 
                            channel_indicators.contains(&first_remaining);
                        
                        if is_hierarchy_word || is_tech_term_with_channel || is_tech_term || is_just_channel {
                            // This is a hierarchy case like:
                            // - "Snare Trig 2" -> "Trig 2"
                            // - "Kick In" -> "In"
                            // - "Clean DI L" -> "DI L"
                            // - "Bass Guitar DI" -> "DI" (when parent is "Bass Guitar")
                            // - "Lead R" -> "R" (when parent is "Lead")
                            result = remaining.to_string();
                        }
                        // Otherwise, preserve compound names like "Bass Guitar" (when parent is "Bass" and remaining is "Guitar")
                    }
                }
            }
            
            // Additional check: Try removing the parent hierarchy name from the result if it appears anywhere (not at start)
            if !parent_clean.is_empty() && !result.is_empty() && !result.starts_with(&parent_clean) {
                // Handle cases where parent might appear anywhere in the result (not just at start)
                // e.g., "GTR E Clean DI L" where parent is "Clean" -> should become "DI L"
                
                let short_hierarchy_words = ["In", "Out", "Top", "Bottom", "L", "R", "C", "Trig"];
                let technical_terms = ["DI", "MIDI", "FX", "AUX", "SUM", "BUS"];
                let channel_indicators = ["L", "R", "C", "Left", "Right", "Center"];
                
                // First, try removing if result starts with parent_clean
                if result.starts_with(&parent_clean) {
                    let remaining = result[parent_clean.len()..].trim_start();
                    let remaining_words: Vec<&str> = remaining.split_whitespace().collect();
                    
                    if !remaining_words.is_empty() {
                        let first_remaining = remaining_words[0];
                        let is_tech_term_with_channel = technical_terms.contains(&first_remaining) &&
                            remaining_words.len() >= 2 &&
                            channel_indicators.contains(&remaining_words[1]);
                        let is_hierarchy_word = short_hierarchy_words.contains(&first_remaining) ||
                            (first_remaining.len() <= 3 && !technical_terms.contains(&first_remaining));
                        
                        if is_hierarchy_word || is_tech_term_with_channel {
                            result = remaining.to_string();
                        }
                    }
                } else if result.contains(&parent_clean) {
                    // Parent appears somewhere in the result (e.g., "Bass Guitar DI" contains "Bass Guitar")
                    // But first, ensure "GTR E " is removed if it's still present (defensive)
                    if result.starts_with("GTR E ") {
                        result = result[6..].trim_start().to_string();
                    }
                    
                    // Now try to remove the parent
                    let parent_words: Vec<&str> = parent_clean.split_whitespace().collect();
                    let result_words: Vec<&str> = result.split_whitespace().collect();
                    
                    // Find where the parent words start in the result
                    for (i, _) in result_words.iter().enumerate() {
                        if i + parent_words.len() <= result_words.len() {
                            let slice = &result_words[i..i + parent_words.len()];
                            if slice == parent_words.as_slice() {
                                // Found the parent words, remove them
                                let after = result_words[i + parent_words.len()..].join(" ");
                                
                                // Prefer the "after" part (what comes after the parent)
                                // This handles:
                                // - "Clean DI L" -> remove "Clean" -> "DI L"
                                // - "Bass Guitar DI" -> remove "Bass Guitar" -> "DI"
                                if !after.is_empty() {
                                    let after_words: Vec<&str> = after.split_whitespace().collect();
                                    if !after_words.is_empty() {
                                        let first_after = after_words[0];
                                        let after_is_tech_term_with_channel = technical_terms.contains(&first_after) &&
                                            after_words.len() >= 2 &&
                                            channel_indicators.contains(&after_words[1]);
                                        let after_is_hierarchy_word = short_hierarchy_words.contains(&first_after) ||
                                            (first_after.len() <= 3 && !technical_terms.contains(&first_after));
                                        
                                        // Also check if it's a technical term by itself (e.g., "DI")
                                        let is_tech_term = technical_terms.contains(&first_after);
                                        
                                        // Check if it's just a channel indicator (e.g., "R" when parent is "Lead")
                                        let is_just_channel = after_words.len() == 1 && 
                                            channel_indicators.contains(&first_after);
                                        
                                        if after_is_hierarchy_word || after_is_tech_term_with_channel || is_tech_term || after_words.len() == 1 || is_just_channel {
                                            result = after.trim().to_string();
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                } else {
                    // Parent doesn't appear in result, but check if we can remove parent words from the start
                    // This handles "Bass DI" when parent is "Bass Guitar" - remove "Bass" to get "DI"
                    // Also check the parent's original track name for words that might appear in the result
                    let short_hierarchy_words = ["In", "Out", "Top", "Bottom", "L", "R", "C", "Trig"];
                    let technical_terms = ["DI", "MIDI", "FX", "AUX", "SUM", "BUS"];
                    let channel_indicators = ["L", "R", "C", "Left", "Right", "Center"];
                    let parent_words: Vec<&str> = parent_clean.split_whitespace().collect();
                    let result_words: Vec<String> = result.split_whitespace().map(|s| s.to_string()).collect();
                    
                    // Also get words from the parent's original track name (without type)
                    let parent_name_without_type = if let Some(parent_type) = &parent_track.track_type {
                        parent_track.name.replace(&format!(" ({})", parent_type), "")
                    } else {
                        parent_track.name.clone()
                    };
                    let parent_name_words: Vec<&str> = parent_name_without_type.split_whitespace().collect();
                    
                    // Try removing parent words from the start of result
                    if !parent_words.is_empty() && !result_words.is_empty() {
                        // Check if result starts with any parent word
                        for parent_word in parent_words.iter() {
                            if result_words[0] == *parent_word {
                                // Result starts with this parent word, check if we should remove it
                                let remaining = result_words[1..].iter().map(|s| s.as_str()).collect::<Vec<&str>>().join(" ");
                                if !remaining.is_empty() {
                                    let remaining_words: Vec<&str> = remaining.split_whitespace().collect();
                                    if !remaining_words.is_empty() {
                                        let first_remaining = remaining_words[0];
                                        let is_tech_term = technical_terms.contains(&first_remaining);
                                        let is_hierarchy_word = short_hierarchy_words.contains(&first_remaining) ||
                                            (first_remaining.len() <= 3 && !technical_terms.contains(&first_remaining));
                                        
                                        // If what remains is a technical term or hierarchy word, remove the parent word
                                        // This handles "Bass DI" -> remove "Bass" -> "DI" (when parent is "Bass Guitar")
                                        if is_tech_term || is_hierarchy_word || remaining_words.len() == 1 {
                                            result = remaining.trim().to_string();
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                    
                    // Also try removing words from the parent's original track name
                    // This handles "Bass DI" when parent is "Bass Guitar (BUS)" - remove "Bass" to get "DI"
                    if !parent_name_words.is_empty() && !result_words.is_empty() {
                        // Parse the parent track to get its prefix
                        let parent_parsed = self.parser.parse(&parent_track.name);
                        
                        // Remove group prefix from parent name words first
                        let mut parent_name_words_clean = parent_name_words.clone();
                        if let Some(parent_prefix) = &parent_parsed.group_prefix {
                            // Remove prefix from parent name words
                            parent_name_words_clean.retain(|w| !w.eq_ignore_ascii_case(parent_prefix));
                        }
                        
                        // Check if result starts with any parent name word (after removing prefix)
                        for parent_name_word in parent_name_words_clean.iter() {
                            if result_words[0].eq_ignore_ascii_case(parent_name_word) {
                                // Result starts with this parent name word, check if we should remove it
                                let remaining = result_words[1..].iter().map(|s| s.as_str()).collect::<Vec<&str>>().join(" ");
                                if !remaining.is_empty() {
                                    let remaining_words: Vec<&str> = remaining.split_whitespace().collect();
                                    if !remaining_words.is_empty() {
                                        let first_remaining = remaining_words[0];
                                        let is_tech_term = technical_terms.contains(&first_remaining);
                                        let is_hierarchy_word = short_hierarchy_words.contains(&first_remaining) ||
                                            (first_remaining.len() <= 3 && !technical_terms.contains(&first_remaining));
                                        
                                        // Check if it's just a channel indicator (e.g., "R" when parent is "Lead")
                                        let is_just_channel = remaining_words.len() == 1 && 
                                            channel_indicators.contains(&first_remaining);
                                        
                                        // If what remains is a technical term or hierarchy word, remove the parent word
                                        // This handles "Bass DI" -> remove "Bass" -> "DI" (when parent is "Bass Guitar")
                                        // Also handles "Lead R" -> remove "Lead" -> "R"
                                        if is_tech_term || is_hierarchy_word || remaining_words.len() == 1 || is_just_channel {
                                            result = remaining.trim().to_string();
                                            break;
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Final defensive cleanup: ensure "GTR E " is removed if it somehow still appears
        // Also remove group prefix if it still appears (e.g., "Bass Guitar" -> "Guitar" when prefix is "Bass")
        // This handles edge cases where cleanup didn't work earlier
        let mut final_result = result.trim().to_string();
        if final_result.starts_with("GTR E ") {
            final_result = final_result[6..].trim_start().to_string();
        } else if final_result == "GTR E" {
            final_result = String::new();
        }
        
        // Remove group prefix from final result if it still appears
        // This handles cases where parent removal didn't work correctly
        if let Some(prefix) = &parsed.group_prefix {
            let prefix_with_space = format!("{} ", prefix);
            if final_result.starts_with(&prefix_with_space) {
                final_result = final_result[prefix_with_space.len()..].trim_start().to_string();
            } else if &final_result == prefix {
                final_result = String::new();
            } else if final_result.contains(&prefix_with_space) {
                // Prefix appears somewhere in the result - remove it
                final_result = final_result.replace(&prefix_with_space, " ").trim().to_string();
            } else if final_result.contains(prefix) && &final_result != prefix {
                // Prefix appears as a word (not at start) - try to remove it
                let words: Vec<&str> = final_result.split_whitespace().collect();
                let filtered_words: Vec<&str> = words.iter()
                    .filter(|w| !w.eq_ignore_ascii_case(prefix))
                    .copied()
                    .collect();
                if !filtered_words.is_empty() {
                    final_result = filtered_words.join(" ");
                } else {
                    final_result = String::new();
                }
            }
        }

        final_result
    }
    
    /// Format the track list in a creative colored list format
    fn fmt_list_recursive(&self, f: &mut fmt::Formatter, track: &Track, prefix: &str, is_last: bool, parent: Option<&Track>) -> fmt::Result {
        let children = self.get_children(&track.name);
        let color_group = self.get_track_color_group(track);
        
        // Draw the tree connector
        let connector = if is_last { "└── " } else { "├── " };
        write!(f, "{}{}", prefix, connector)?;
        
        // Check if this is a root track using get_roots()
        let roots = self.get_roots();
        let is_root = roots.iter().any(|r| r.name == track.name);
        
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
                // is_root is already computed above
                
                // If name is empty, try to build a meaningful name from parsed components
                if name.is_empty() {
                    let parsed = self.parser.parse(&track.name);
                    
                    // Try to use sub_type if available
                    if let Some(sub_types) = &parsed.sub_type {
                        if let Some(last_sub_type) = sub_types.last() {
                            // Remove group prefix from sub_type if it appears
                            let mut fallback_name = last_sub_type.clone();
                            if let Some(prefix) = &parsed.group_prefix {
                                let prefix_with_space = format!("{} ", prefix);
                                if fallback_name.starts_with(&prefix_with_space) {
                                    fallback_name = fallback_name[prefix_with_space.len()..].to_string();
                                } else if &fallback_name == prefix {
                                    fallback_name = String::new();
                                } else if fallback_name.contains(&prefix_with_space) {
                                    fallback_name = fallback_name.replace(&prefix_with_space, " ").trim().to_string();
                                }
                            }
                            if !fallback_name.is_empty() {
                                name = fallback_name;
                            }
                        }
                    }
                    
                    // If still empty, try using group prefix (for top-level tracks)
                    if name.is_empty() && is_root {
                        if let Some(prefix) = &parsed.group_prefix {
                            // For multi-word prefixes like "GTR E", use the full prefix
                            // For single-letter prefixes, try to get the group name
                            if prefix.len() > 1 || prefix == "G" {
                                // Multi-word prefix or "G" (which should be "GTR E")
                                if prefix == "G" {
                                    // Check if the original name contains "GTR E"
                                    if let Some(original) = &parsed.original_name {
                                        if original.contains("GTR E") {
                                            name = "GTR E".to_string();
                                        } else {
                                            name = prefix.clone();
                                        }
                                    } else {
                                        name = prefix.clone();
                                    }
                                } else {
                                    name = prefix.clone();
                                }
                            }
                        }
                    }
                    
                    // If still empty, check if parent has the same name (SUM tracks often duplicate parent name)
                    if name.is_empty() {
                        if let Some(parent_track) = parent {
                            let parent_parsed = self.parser.parse(&parent_track.name);
                            let parent_hierarchy = self.build_hierarchy_name_from_parsed(&parent_parsed);
                            let current_parsed = self.parser.parse(&track.name);
                            let current_hierarchy = self.build_hierarchy_name_from_parsed(&current_parsed);
                            
                            // If current hierarchy matches parent hierarchy, it's a duplicate (like "Kick" -> "Kick")
                            // For SUM tracks, show "(SUM)" instead
                            if current_hierarchy == parent_hierarchy {
                                if let Some(track_type) = &track.track_type {
                                    if track_type == "SUM" {
                                        name = format!("({})", track_type);
                                    }
                                }
                            }
                        }
                    }
                    
                    // If still empty and it's a SUM track, show the type
                    if name.is_empty() {
                        if let Some(track_type) = &track.track_type {
                            if track_type == "SUM" {
                                name = format!("({})", track_type);
                            } else if track_type == "BUS" && !is_root {
                                // For non-root BUS tracks with empty name, show type
                                name = format!("({})", track_type);
                            }
                        }
                    }
                    
                    // If still empty, try using the original track name (fallback for tracks like Rooms, Percussion)
                    if name.is_empty() {
                        // Remove track type from name if present
                        let name_without_type = if let Some(track_type) = &track.track_type {
                            track.name.replace(&format!(" ({})", track_type), "")
                        } else {
                            track.name.clone()
                        };
                        
                        // Remove group prefix if present
                        let parsed = self.parser.parse(&track.name);
                        if let Some(prefix) = &parsed.group_prefix {
                            let prefix_with_space = format!("{} ", prefix);
                            if name_without_type.starts_with(&prefix_with_space) {
                                name = name_without_type[prefix_with_space.len()..].trim_start().to_string();
                            } else if name_without_type.starts_with(prefix) {
                                name = name_without_type[prefix.len()..].trim_start().to_string();
                            } else {
                                name = name_without_type;
                            }
                        } else {
                            name = name_without_type;
                        }
                    }
                } else {
                    // Name is not empty - add type suffix if needed
                    if let Some(track_type) = &track.track_type {
                        let type_suffix = format!(" ({})", track_type);
                        
                        // For root tracks with BUS type, explicitly remove the type if present
                        if is_root && track_type.as_str() == "BUS" {
                            if name.ends_with(&type_suffix) {
                                name = name[..name.len() - type_suffix.len()].to_string();
                            }
                            // Don't add the type back - we want to hide it for root tracks
                        } else {
                            // For non-root tracks or non-BUS types, add the type if needed
                            if !name.ends_with(&type_suffix) {
                                name.push_str(&type_suffix);
                            }
                        }
                    }
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
