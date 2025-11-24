//! Simple parser implementation for track names
//!
//! This is a basic parser that matches track names against groups and extracts components.
//! For production use, see the full parser implementation in fts-naming-parser.

use crate::{TrackName, FullGroup, ComponentType};
use std::collections::HashSet;
use regex::Regex;
use splitty::split_unquoted_whitespace;

/// Simple parser for track names
pub struct SimpleParser {
    groups: Vec<FullGroup>,
}

impl SimpleParser {
    /// Create a new parser with the given groups
    pub fn new(groups: Vec<FullGroup>) -> Self {
        Self { groups }
    }
    
    /// Parse a track name string into a TrackName struct
    pub fn parse(&self, name: &str) -> TrackName {
        let mut track_name = TrackName::new();
        
        // Extract file extension if present
        let (clean_name, extension) = Self::extract_file_extension(name);
        track_name.file_extension = extension;
        
        let name_lower = clean_name.to_lowercase();
        
        // Use splitty for smarter word splitting (handles quoted sections)
        let words: Vec<String> = split_unquoted_whitespace(&clean_name)
            .unwrap_quotes(true)
            .map(|w| {
                // Trim non-alphanumeric characters except dots and dashes
                w.trim_matches(|c: char| !c.is_alphanumeric() && c != '.' && c != '-')
                    .to_string()
            })
            .filter(|w| !w.is_empty())
            .collect();
        
        let mut matched_words = HashSet::new();
        
        // Extract track type from parentheses first (e.g., "Kick (BUS)")
        let (base_name, track_type) = Self::extract_track_type(&clean_name);
        if let Some(tt) = &track_type {
            track_name.track_type = Some(tt.clone());
            // Mark track type words as matched
            for word in split_unquoted_whitespace(tt)
                .unwrap_quotes(true)
                .map(|w| w.to_lowercase()) {
                matched_words.insert(word);
            }
        }
        
        // Store original base name for case preservation
        let original_base = base_name.clone();
        let name_to_match = base_name.to_lowercase();
        
        // STEP 1: Find matching group
        let matching_group = self.find_matching_group(&name_to_match, &original_base);
        
        if let Some((group, child_path)) = matching_group {
            // Build the full prefix path from all parent groups
            let mut prefix_path = Vec::new();
            Self::collect_prefix_path(group, &child_path, &mut prefix_path);
            
            // Store the full prefix path (all parent prefixes)
            if !prefix_path.is_empty() {
                track_name.group_prefix = Some(prefix_path.join(" "));
            } else {
                track_name.group_prefix = Some(group.prefix.clone());
            }
            
            // Track group-related words as matched
            let group_name_lower = group.name.to_lowercase();
            let group_prefix_lower = group.prefix.to_lowercase();
            
            // Mark group name and prefix as matched if they appear in the name
            for word in &words {
                let word_lower = word.to_lowercase();
                if word_lower == group_name_lower || word_lower == group_prefix_lower {
                    matched_words.insert(word_lower);
                }
            }
            
            // Set sub-types from child path and mark them as matched
            // Note: child_path contains the nested group names, not prefixes
            if !child_path.is_empty() {
                track_name.sub_type = Some(child_path.clone());
                // Mark sub-type names as matched
                for sub_type in &child_path {
                    let sub_type_lower = sub_type.to_lowercase();
                    matched_words.insert(sub_type_lower.clone());
                    // Also check if any words match the sub-type
                    for word in &words {
                        if word.to_lowercase() == sub_type_lower {
                            matched_words.insert(word.to_lowercase());
                        }
                    }
                }
            }
            
            // STEP 2: Extract components using group context
            Self::extract_arrangement(&mut track_name, &base_name, &name_to_match, group, &mut matched_words);
            Self::extract_section(&mut track_name, &base_name, &name_to_match, group, &mut matched_words);
            Self::extract_performer(&mut track_name, &base_name, &name_to_match, group, &mut matched_words);
            Self::extract_layers(&mut track_name, &base_name, &name_to_match, group, &mut matched_words);
            Self::extract_multi_mic(&mut track_name, &base_name, &name_to_match, group, &mut matched_words);
            Self::extract_effect(&mut track_name, &base_name, &name_to_match, group, &mut matched_words);
            Self::extract_increment(&mut track_name, &base_name, &name_to_match, group, &mut matched_words);
            Self::extract_channel(&mut track_name, &base_name, &name_to_match, &mut matched_words);
            Self::extract_playlist(&mut track_name, &base_name, &name_to_match, &mut matched_words);
            Self::extract_rec_tag(&mut track_name, &base_name, &name_to_match, group, &mut matched_words);
        }
        
        // Mark track type word as matched (if it was extracted from parentheses)
        if let Some(ref tt) = track_name.track_type {
            let tt_lower = tt.to_lowercase();
            matched_words.insert(tt_lower);
        }
        
        // Collect unparsed words (only include if they're not part of the expected structure)
        let unparsed: Vec<String> = words.iter()
            .filter(|w| {
                let w_lower = w.to_lowercase();
                // Don't include if already matched
                !matched_words.contains(&w_lower)
            })
            .cloned()
            .collect();
        
        if !unparsed.is_empty() {
            track_name.unparsed_words = Some(unparsed);
        }
        
        track_name
    }
    
    /// Find the best matching group for a track name
    /// Returns the top-level group and the full path to the deepest matching child
    fn find_matching_group(&self, name_lower: &str, original: &str) -> Option<(&FullGroup, Vec<String>)> {
        let mut best_match: Option<(&FullGroup, Vec<String>, i32)> = None;
        
        for group in &self.groups {
            // First check if this group matches
            if let Some(score) = Self::group_matches(group, name_lower, original) {
                // Try to find child groups recursively (use original for case preservation)
                let child_path = Self::find_child_path(group, name_lower, original);
                
                // Calculate total score (parent + children)
                let total_score = score + (child_path.len() as i32 * 50);
                
                let current_best = best_match.as_ref().map(|(_, _, s)| *s).unwrap_or(0);
                if total_score > current_best {
                    best_match = Some((group, child_path, total_score));
                }
            }
            
            // Also check if any child matches (even if parent doesn't)
            // This handles cases like "Kick In" matching Kick (child of Drums)
            let child_match = Self::find_matching_child_in_group(group, name_lower, original);
            if let Some((child_path, child_score)) = child_match {
                let current_best = best_match.as_ref().map(|(_, _, s)| *s).unwrap_or(0);
                if child_score > current_best {
                    // Build full path including parent
                    best_match = Some((group, child_path, child_score));
                }
            }
        }
        
        best_match.map(|(group, path, _)| (group, path))
    }
    
    /// Find if any child of a group matches, returning the path and score
    fn find_matching_child_in_group(group: &FullGroup, name_lower: &str, original: &str) -> Option<(Vec<String>, i32)> {
        for child in &group.children {
            if let Some(score) = Self::group_matches(child, name_lower, original) {
                // Found a matching child, build the path
                let mut path = vec![child.name.clone()];
                // Recursively check for deeper children
                let deeper_path = Self::find_child_path(child, name_lower, original);
                path.extend(deeper_path);
                let total_score = score + (path.len() as i32 * 50);
                return Some((path, total_score));
            }
            
            // Recursively check grandchildren
            if let Some((grandchild_path, grandchild_score)) = Self::find_matching_child_in_group(child, name_lower, original) {
                let mut full_path = vec![child.name.clone()];
                full_path.extend(grandchild_path);
                return Some((full_path, grandchild_score));
            }
        }
        None
    }
    
    /// Check if a group matches a track name
    fn group_matches(group: &FullGroup, name_lower: &str, original: &str) -> Option<i32> {
        // Check negative patterns first (use word boundaries)
        for neg_pattern in &group.negative_patterns {
            let neg_lower = neg_pattern.to_lowercase();
            // Check if negative pattern appears as a whole word
            if name_lower == neg_lower 
                || name_lower.starts_with(&format!("{} ", neg_lower))
                || name_lower.ends_with(&format!(" {}", neg_lower))
                || name_lower.contains(&format!(" {} ", neg_lower)) {
                return None;
            }
        }
        
        // Check if any pattern matches (using word boundaries)
        let mut has_match = false;
        let mut score = group.priority;
        
        // Check group name (exact word match gets highest score)
        let group_name_lower = group.name.to_lowercase();
        if name_lower == group_name_lower {
            has_match = true;
            score += 1000; // Exact match gets very high score
        } else if name_lower.starts_with(&format!("{} ", group_name_lower))
            || name_lower.ends_with(&format!(" {}", group_name_lower))
            || name_lower.contains(&format!(" {} ", group_name_lower)) {
            has_match = true;
            score += 500; // Word boundary match
        } else if name_lower.contains(&group_name_lower) {
            has_match = true;
            score += 100; // Substring match (lower priority)
        }
        
        // Check prefix (exact word match)
        let prefix_lower = group.prefix.to_lowercase();
        if name_lower == prefix_lower {
            has_match = true;
            score += 500;
        } else if name_lower.starts_with(&format!("{} ", prefix_lower))
            || name_lower.ends_with(&format!(" {}", prefix_lower))
            || name_lower.contains(&format!(" {} ", prefix_lower)) {
            has_match = true;
            score += 200;
        } else if name_lower.contains(&prefix_lower) {
            has_match = true;
            score += 50;
        }
        
        // Check patterns (word boundary matching)
        for pattern in &group.patterns {
            let pattern_lower = pattern.to_lowercase();
            if name_lower == pattern_lower {
                has_match = true;
                score += 300;
                break;
            } else if name_lower.starts_with(&format!("{} ", pattern_lower))
                || name_lower.ends_with(&format!(" {}", pattern_lower))
                || name_lower.contains(&format!(" {} ", pattern_lower)) {
                has_match = true;
                score += 100;
                break;
            } else if name_lower.contains(&pattern_lower) {
                has_match = true;
                score += 10;
                break;
            }
        }
        
        if has_match {
            Some(score)
        } else {
            None
        }
    }
    
    /// Find the path of child groups that match, preserving original case
    fn find_child_path(group: &FullGroup, name_lower: &str, original: &str) -> Vec<String> {
        let mut path = Vec::new();
        Self::find_child_path_recursive(group, name_lower, original, &mut path);
        path
    }
    
    fn find_child_path_recursive(group: &FullGroup, name_lower: &str, original: &str, path: &mut Vec<String>) {
        for child in &group.children {
            // Check negative patterns first
            let child_negative_matches = child.negative_patterns.iter()
                .any(|np| {
                    let np_lower = np.to_lowercase();
                    name_lower == np_lower
                        || name_lower.starts_with(&format!("{} ", np_lower))
                        || name_lower.ends_with(&format!(" {}", np_lower))
                        || name_lower.contains(&format!(" {} ", np_lower))
                });
            
            if child_negative_matches {
                continue;
            }
            
            // Check if child matches (using word boundaries)
            let child_name_lower = child.name.to_lowercase();
            let child_matches = 
                name_lower == child_name_lower
                || name_lower.starts_with(&format!("{} ", child_name_lower))
                || name_lower.ends_with(&format!(" {}", child_name_lower))
                || name_lower.contains(&format!(" {} ", child_name_lower))
                || child.patterns.iter().any(|p| {
                    let p_lower = p.to_lowercase();
                    name_lower == p_lower
                        || name_lower.starts_with(&format!("{} ", p_lower))
                        || name_lower.ends_with(&format!(" {}", p_lower))
                        || name_lower.contains(&format!(" {} ", p_lower))
                })
                || name_lower.contains(&child.prefix.to_lowercase());
            
            if child_matches {
                // Try to preserve original case from the input
                // First try to find the exact word match, then try patterns
                let original_case = Self::find_word_in_original(original, &child_name_lower)
                    .or_else(|| {
                        // Try to find a pattern match
                        child.patterns.iter()
                            .find_map(|p| Self::find_word_in_original(original, &p.to_lowercase()))
                    })
                    .unwrap_or_else(|| child.name.clone());
                path.push(original_case);
                // Recursively check children
                Self::find_child_path_recursive(child, name_lower, original, path);
                break;
            }
        }
    }
    
    /// Extract arrangement component
    fn extract_arrangement(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &FullGroup,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.arrangement.is_some() {
            return;
        }
        
        // Check group-specific patterns first
        let patterns = group.arrangement_patterns();
        for pattern in &patterns {
            let pattern_lower = pattern.to_lowercase();
            if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                // Find original case
                if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                    track_name.arrangement = Some(original_word);
                    matched_words.insert(pattern_lower);
                    return;
                }
            }
        }
        
        // Check child groups
        for child in &group.children {
            let child_patterns = child.arrangement_patterns();
            for pattern in &child_patterns {
                let pattern_lower = pattern.to_lowercase();
                if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                    if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                        track_name.arrangement = Some(original_word);
                        matched_words.insert(pattern_lower);
                        return;
                    }
                }
            }
        }
    }
    
    /// Extract section component
    fn extract_section(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &FullGroup,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.section.is_some() {
            return;
        }
        
        let patterns = group.get_component_patterns(ComponentType::Section);
        for pattern in &patterns {
            let pattern_lower = pattern.to_lowercase();
            if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                    track_name.section = Some(original_word);
                    matched_words.insert(pattern_lower);
                    return;
                }
            }
        }
    }
    
    /// Extract performer component
    fn extract_performer(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &FullGroup,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.performer.is_some() {
            return;
        }
        
        let patterns = group.get_component_patterns(ComponentType::Performer);
        for pattern in &patterns {
            let pattern_lower = pattern.to_lowercase();
            if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                    track_name.performer = Some(original_word);
                    matched_words.insert(pattern_lower);
                    return;
                }
            }
        }
    }
    
    /// Extract layers component
    fn extract_layers(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &FullGroup,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.layers.is_some() {
            return;
        }
        
        let patterns = group.layers_patterns();
        for pattern in &patterns {
            let pattern_lower = pattern.to_lowercase();
            if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                    track_name.layers = Some(original_word);
                    matched_words.insert(pattern_lower);
                    return;
                }
            }
        }
    }
    
    /// Extract multi-mic component
    fn extract_multi_mic(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &FullGroup,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.multi_mic.is_some() {
            return;
        }
        
        let mut matches = Vec::new();
        
        // Check multi-mic descriptors first
        for descriptor in &group.multi_mic_descriptors {
            if descriptor.matches(name_lower) {
                // Try to find the descriptor name or its patterns in the original
                let descriptor_name_lower = descriptor.name.to_lowercase();
                if let Some(original_word) = Self::find_word_in_original(original, &descriptor_name_lower) {
                    matches.push(original_word);
                    matched_words.insert(descriptor_name_lower);
                } else {
                    // Try to find any of the descriptor's patterns
                    for pattern in &descriptor.patterns {
                        let pattern_lower = pattern.to_lowercase();
                        if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                            if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                                matches.push(original_word);
                                matched_words.insert(pattern_lower);
                                break;
                            }
                        }
                    }
                }
            }
        }
        
        // Check component patterns as fallback
        if matches.is_empty() {
            let patterns = group.get_component_patterns(ComponentType::MultiMic);
            for pattern in &patterns {
                let pattern_lower = pattern.to_lowercase();
                if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                    if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                        matches.push(original_word);
                        matched_words.insert(pattern_lower);
                    }
                }
            }
        }
        
        if !matches.is_empty() {
            track_name.multi_mic = Some(matches);
        }
    }
    
    /// Extract effect component
    fn extract_effect(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &FullGroup,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.effect.is_some() {
            return;
        }
        
        let mut matches = Vec::new();
        let patterns = group.effect_patterns();
        
        for pattern in &patterns {
            let pattern_lower = pattern.to_lowercase();
            if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                    matches.push(original_word);
                    matched_words.insert(pattern_lower);
                }
            }
        }
        
        if !matches.is_empty() {
            track_name.effect = Some(matches);
        }
    }
    
    /// Extract increment component
    fn extract_increment(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &FullGroup,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.increment.is_some() {
            return;
        }
        
        // Check if group or sub-type supports increments
        let supports_increment = group.group_type == Some(crate::GroupType::Increment)
            || track_name.sub_type.as_ref().and_then(|st| {
                group.children.iter()
                    .find(|c| st.contains(&c.name))
                    .and_then(|c| Some(c.group_type == Some(crate::GroupType::Increment)))
            }).unwrap_or(false);
        
        if supports_increment {
            // Match numbers at the end (e.g., "Tom 1", "Tom 2")
            if let Ok(re) = Regex::new(r"\b(\d+)\s*$") {
                if let Some(captures) = re.captures(original) {
                    if let Some(num) = captures.get(1) {
                        let num_str = num.as_str();
                        if !matched_words.contains(num_str) {
                            track_name.increment = Some(num_str.to_string());
                            matched_words.insert(num_str.to_string());
                        }
                    }
                }
            }
        }
    }
    
    /// Extract channel component (L, R, C, Left, Right, etc.)
    fn extract_channel(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.channel.is_some() {
            return;
        }
        
        let channel_patterns = vec!["l", "r", "c", "left", "right", "center", "centre"];
        for pattern in &channel_patterns {
            if name_lower.ends_with(&format!(" {}", pattern)) || name_lower == *pattern {
                if let Some(original_word) = Self::find_word_in_original(original, pattern) {
                    track_name.channel = Some(original_word);
                    matched_words.insert(pattern.to_string());
                    return;
                }
            }
        }
    }
    
    /// Extract playlist component
    fn extract_playlist(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.playlist.is_some() {
            return;
        }
        
        // Match playlist patterns like ".1", ".2", ".A", ".B", etc.
        if let Ok(re) = Regex::new(r"\.(\d+|[A-Za-z]+)$") {
            if let Some(captures) = re.captures(original) {
                if let Some(matched) = captures.get(0) {
                    let matched_str = matched.as_str();
                    if !matched_words.contains(&matched_str.to_lowercase()) {
                        track_name.playlist = Some(matched_str.to_string());
                        matched_words.insert(matched_str.to_lowercase());
                    }
                }
            }
        }
    }
    
    /// Extract rec tag component
    fn extract_rec_tag(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &FullGroup,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.rec_tag.is_some() {
            return;
        }
        
        let patterns = group.get_component_patterns(ComponentType::RecTag);
        for pattern in &patterns {
            let pattern_lower = pattern.to_lowercase();
            if name_lower.contains(&pattern_lower) && !matched_words.contains(&pattern_lower) {
                if let Some(original_word) = Self::find_word_in_original(original, &pattern_lower) {
                    track_name.rec_tag = Some(original_word);
                    matched_words.insert(pattern_lower);
                    return;
                }
            }
        }
    }
    
    /// Find a word in the original string preserving case
    fn find_word_in_original(original: &str, pattern_lower: &str) -> Option<String> {
        let words: Vec<String> = split_unquoted_whitespace(original)
            .unwrap_quotes(true)
            .map(|w| w.to_string())
            .collect();
        for word in words {
            if word.to_lowercase() == pattern_lower {
                return Some(word);
            }
        }
        None
    }
    
    /// Extract file extension from name
    /// Collect prefix path from group and child path
    /// This builds up all parent prefixes (e.g., ["D"] for Drums, ["D", "K"] for Drums -> Kick)
    /// Note: We only include prefixes up to (but not including) the final child in the path
    fn collect_prefix_path(group: &FullGroup, child_path: &[String], prefix_path: &mut Vec<String>) {
        // Add current group's prefix
        prefix_path.push(group.prefix.clone());
        
        // Add prefixes for all children EXCEPT the last one (the last one is the actual track name)
        let mut current_group = group;
        for (i, child_name) in child_path.iter().enumerate() {
            // Don't add prefix for the last child - that's the track name itself
            if i < child_path.len() - 1 {
                if let Some(child) = current_group.find_child(child_name) {
                    prefix_path.push(child.prefix.clone());
                    current_group = child;
                } else {
                    break;
                }
            }
        }
    }
    
    fn extract_file_extension(name: &str) -> (String, Option<String>) {
        const EXTENSIONS: &[&str] = &[
            ".wav", ".wave", ".aif", ".aiff", ".aifc", ".flac", ".mp3", ".m4a",
            ".aac", ".ogg", ".wma", ".opus", ".aax", ".au", ".snd", ".raw",
            ".pcm", ".dsd", ".dsf", ".dff", ".caf", ".w64", ".rf64", ".bwf"
        ];
        
        let name_lower = name.to_lowercase();
        for &ext in EXTENSIONS {
            if name_lower.ends_with(ext) {
                let clean = name[..name.len() - ext.len()].to_string();
                return (clean, Some(ext.to_string()));
            }
        }
        
        (name.to_string(), None)
    }
    
    /// Extract track type from parentheses (e.g., "Kick (BUS)" -> ("Kick", Some("BUS")))
    fn extract_track_type(name: &str) -> (String, Option<String>) {
        if let Ok(re) = Regex::new(r"^(.+?)\s*\(([^)]+)\)$") {
            if let Some(captures) = re.captures(name) {
                if let (Some(base), Some(tt)) = (captures.get(1), captures.get(2)) {
                    return (base.as_str().trim().to_string(), Some(tt.as_str().to_string()));
                }
            }
        }
        (name.to_string(), None)
    }
}

