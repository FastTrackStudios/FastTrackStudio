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
        
        // Add sub_type (e.g., "Kick", "Snare", "Bass Guitar")
        // Use the last sub_type (most specific) for hierarchy display
        // But remove "GTR E" if it's part of the sub_type (it should be prefix, not sub_type)
        if let Some(sub_types) = &parsed.sub_type {
            if let Some(last_sub_type) = sub_types.last() {
                let mut sub_type = last_sub_type.clone();
                // Remove "GTR E " if it appears at the start of sub_type
                if sub_type.starts_with("GTR E ") {
                    sub_type = sub_type[6..].trim_start().to_string();
                } else if sub_type == "GTR E" {
                    sub_type = String::new();
                }
                if !sub_type.is_empty() {
                    parts.push(sub_type);
                }
            }
        }
        
        // Add arrangement if it doesn't match sub_type (e.g., "Lead", "Clean")
        if let Some(arrangement) = &parsed.arrangement {
            let should_include = if let Some(ref sub_types) = parsed.sub_type {
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
        if let Some(increment) = &parsed.increment {
            parts.push(increment.clone());
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
        
        // Remove "GTR E " if it appears at the start (should be prefix, not part of hierarchy)
        // This handles cases where parser includes "GTR E" in sub_type
        if result.starts_with("GTR E ") {
            result = result[6..].trim_start().to_string();
        } else if result == "GTR E" {
            result = String::new();
        }
        
        result
    }
    
    /// Extract hierarchy name by parsing the track name and removing parent parts
    /// Uses the naming-convention parser to extract components instead of hardcoded logic
    fn extract_hierarchy_name(&self, track: &Track, parent: Option<&Track>) -> String {
        // Parse the track name using the configured parser
        let parsed = self.parser.parse(&track.name);
        
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
            } else if result.starts_with(prefix) && result.len() > prefix.len() {
                result = result[prefix.len()..].trim_start().to_string();
            }
            
            // Also handle multi-word prefixes that might be in sub_type (e.g., "GTR E" when prefix is "G")
            // If sub_type contains "GTR E", we want to keep it in the hierarchy name (it's the instrument name)
            // But if the prefix is just "G" and sub_type is "GTR E", we should use "GTR E" as the hierarchy name
            if prefix == "G" {
                if let Some(sub_types) = &parsed.sub_type {
                    for sub_type in sub_types {
                        // If we have "G GTR E", the hierarchy name should be "GTR E" (the instrument group)
                        if sub_type == "GTR E" || sub_type.starts_with("GTR E ") {
                            // Use the sub_type as the hierarchy name instead of trying to remove the prefix
                            result = sub_type.clone();
                            break;
                        }
                    }
                }
            }
            
            // If result is empty or doesn't seem right, use fallback
            if result.is_empty() || 
               (name_without_type.starts_with(&prefix_with_space) && 
                !name_without_type[prefix_with_space.len()..].trim_start().starts_with(&result)) {
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
                
                // Also handle "GTR E" as a special case - if we have "G GTR E", remove both "G " and "GTR E "
                // But only if what remains is a clear hierarchy case (like "Clean DI L")
                if result.starts_with("GTR E ") {
                    let after_gtr_e = result[6..].trim_start();
                    let after_words: Vec<&str> = after_gtr_e.split_whitespace().collect();
                    // If what follows "GTR E " is a single word or starts with a hierarchy/tech term, remove "GTR E "
                    // This handles "GTR E Clean DI L" -> "Clean DI L" (we'll remove "Clean" later via parent)
                    if !after_words.is_empty() {
                        result = after_gtr_e.to_string();
                    } else {
                        result = String::new();
                    }
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
        let single_letter_prefixes = ["G ", "D ", "B ", "K ", "V ", "VF ", "U ", "A "];
        for single_prefix in &single_letter_prefixes {
            if result.starts_with(single_prefix) {
                let remaining = &result[single_prefix.len()..];
                // If removing the single-letter prefix leaves "GTR E", remove "GTR E " too
                if remaining.starts_with("GTR E ") {
                    result = remaining[6..].trim_start().to_string(); // Remove both "G " and "GTR E "
                } else if remaining == "GTR E" {
                    result = String::new(); // Remove both "G " and "GTR E"
                } else {
                    // For other cases, remove the single-letter prefix
                    result = remaining.trim_start().to_string();
                }
                break;
            }
        }
        
        // Also remove "GTR E " if it appears at the start (should have been removed by prefix removal, but be defensive)
        if result.starts_with("GTR E ") {
            result = result[6..].trim_start().to_string();
        } else if result == "GTR E" {
            result = String::new();
        }
        
        // Remove "GTR E " if it appears at the start (should be part of prefix, not hierarchy)
        // Do this before parent removal so parent removal works correctly
        // This handles cases where parser includes "GTR E" in sub_type or fallback includes it
        // Check multiple times to handle cases like "G GTR E Clean DI L" -> remove "G " -> "GTR E Clean DI L" -> remove "GTR E " -> "Clean DI L"
        loop {
            let original = result.clone();
            if result.starts_with("GTR E ") {
                result = result[6..].trim_start().to_string();
            } else if result == "GTR E" {
                result = String::new();
                break;
            } else {
                break; // No more "GTR E" to remove
            }
            // If we didn't change anything, break to avoid infinite loop
            if result == original {
                break;
            }
        }
        
        // If we have a parent, remove the parent's hierarchy name from the beginning
        if let Some(parent_track) = parent {
            // Parse the parent track name and build its hierarchy name
            let parent_parsed = self.parser.parse(&parent_track.name);
            let parent_hierarchy = self.build_hierarchy_name_from_parsed(&parent_parsed);
            
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
            if !parent_clean.is_empty() && result.starts_with(&parent_clean) {
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
                // 3. Remaining starts with a technical term followed by a channel indicator (e.g., "DI L", "DI R")
                if result_words.len() == 1 && result_words[0] == parent_clean {
                    result = remaining.to_string();
                } else if !remaining_words.is_empty() {
                    let first_remaining = remaining_words[0];
                    // Check if first word is a hierarchy word
                    let is_hierarchy_word = short_hierarchy_words.contains(&first_remaining) ||
                        (first_remaining.len() <= 3 && !technical_terms.contains(&first_remaining));
                    
                    // Check if it's a technical term followed by a channel (e.g., "DI L", "DI R")
                    let is_tech_term_with_channel = technical_terms.contains(&first_remaining) &&
                        remaining_words.len() >= 2 &&
                        channel_indicators.contains(&remaining_words[1]);
                    
                    if is_hierarchy_word || is_tech_term_with_channel {
                        // This is a hierarchy case like:
                        // - "Snare Trig 2" -> "Trig 2"
                        // - "Kick In" -> "In"
                        // - "Clean DI L" -> "DI L"
                        result = remaining.to_string();
                    }
                    // Otherwise, preserve compound names like "Bass DI" (when DI is not followed by channel)
                }
                // Otherwise, preserve compound names like "Bass Guitar"
            } else if !parent_clean.is_empty() {
                // Try removing the parent hierarchy name from the result
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
                    // Parent appears somewhere in the result (e.g., "GTR E Clean DI L" contains "Clean")
                    // Try to remove it by finding where it appears
                    let parent_words: Vec<&str> = parent_clean.split_whitespace().collect();
                    let result_words: Vec<&str> = result.split_whitespace().collect();
                    
                    // Find where the parent words start in the result
                    for (i, _) in result_words.iter().enumerate() {
                        if i + parent_words.len() <= result_words.len() {
                            let slice = &result_words[i..i + parent_words.len()];
                            if slice == parent_words.as_slice() {
                                // Found the parent words, remove them
                                let before = result_words[..i].join(" ");
                                let after = result_words[i + parent_words.len()..].join(" ");
                                
                                // Prefer the "after" part (what comes after the parent)
                                // This handles "GTR E Clean DI L" -> remove "Clean" -> "DI L"
                                if !after.is_empty() {
                                    let after_words: Vec<&str> = after.split_whitespace().collect();
                                    if !after_words.is_empty() {
                                        let first_after = after_words[0];
                                        let after_is_tech_term_with_channel = technical_terms.contains(&first_after) &&
                                            after_words.len() >= 2 &&
                                            channel_indicators.contains(&after_words[1]);
                                        let after_is_hierarchy_word = short_hierarchy_words.contains(&first_after) ||
                                            (first_after.len() <= 3 && !technical_terms.contains(&first_after));
                                        
                                        if after_is_hierarchy_word || after_is_tech_term_with_channel || after_words.len() == 1 {
                                            result = after.trim().to_string();
                                            break;
                                        }
                                    }
                                }
                                
                                // Fallback to checking what's before, or combined
                                let remaining = if !after.is_empty() {
                                    after.clone()
                                } else if !before.is_empty() {
                                    before
                                } else {
                                    String::new()
                                };
                                
                                let remaining_words: Vec<&str> = remaining.split_whitespace().collect();
                                if !remaining_words.is_empty() {
                                    let first_remaining = remaining_words[0];
                                    let is_tech_term_with_channel = technical_terms.contains(&first_remaining) &&
                                        remaining_words.len() >= 2 &&
                                        channel_indicators.contains(&remaining_words[1]);
                                    let is_hierarchy_word = short_hierarchy_words.contains(&first_remaining) ||
                                        (first_remaining.len() <= 3 && !technical_terms.contains(&first_remaining));
                                    
                                    if is_hierarchy_word || is_tech_term_with_channel || remaining_words.len() == 1 {
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
        
        // Final pass: remove "GTR E " and single-letter prefixes if they still appear
        // This ensures cleanup happens even if fallback logic added them back
        // Do this BEFORE parent removal so parent removal can work on clean result
        let mut cleaned_result = result.trim().to_string();
        
        // Remove single-letter prefixes first, then "GTR E "
        let single_letter_prefixes = ["G ", "D ", "B ", "K ", "V ", "VF ", "U ", "A "];
        for prefix in &single_letter_prefixes {
            if cleaned_result.starts_with(prefix) {
                cleaned_result = cleaned_result[prefix.len()..].trim_start().to_string();
                break;
            }
        }
        
        // Now remove "GTR E " if it appears
        if cleaned_result.starts_with("GTR E ") {
            cleaned_result = cleaned_result[6..].trim_start().to_string();
        } else if cleaned_result == "GTR E" {
            cleaned_result = String::new();
        }
        
        // Use cleaned result for parent removal (parent removal logic is below)
        result = cleaned_result;
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
                
                // For SUM tracks, if name is empty after removing parent, show the type
                // For other tracks, show type only if name is not empty
                if name.is_empty() {
                    if let Some(track_type) = &track.track_type {
                        // Only show type for SUM tracks when name is empty
                        if track_type == "SUM" {
                            name = format!("({})", track_type);
                        }
                    }
                } else {
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
