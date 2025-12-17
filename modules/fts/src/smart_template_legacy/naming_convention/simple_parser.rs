//! Simple parser implementation for track names
//!
//! This is a basic parser that matches track names against groups and extracts components.
//! For production use, see the full parser implementation in fts-naming-parser.

use super::{TrackName, Group, ComponentType};
use std::collections::HashSet;
use regex::Regex;
use splitty::split_unquoted_whitespace;

/// Simple parser for track names
#[derive(Clone)]
pub struct SimpleParser {
    groups: Vec<Group>,
}

impl std::fmt::Debug for SimpleParser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("SimpleParser")
            .field("groups_count", &self.groups.len())
            .finish()
    }
}

impl SimpleParser {
    /// Create a new parser with the given groups
    pub fn new(groups: Vec<Group>) -> Self {
        Self { groups }
    }
    
    /// Parse a track name string into a TrackName struct
    pub fn parse(&self, name: &str) -> TrackName {
        let mut track_name = TrackName::new();
        
        // Store the original input name for later context checking
        track_name.original_name = Some(name.to_string());
        
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
        
        // ============================================
        // PASS 1: Match parent groups first (narrow down to top-level groups)
        // ============================================
        // Find matching top-level group first, without checking children
        let matching_parent_group = self.find_matching_parent_group(&name_to_match, &original_base);
        
        if let Some((parent_group, parent_score)) = matching_parent_group {
            // Mark parent group words as matched
            let group_name_lower = parent_group.name.to_lowercase();
            let group_prefix_lower = parent_group.prefix.to_lowercase();
            
            for (i, word) in words.iter().enumerate() {
                let word_lower = word.to_lowercase();
                if word_lower == group_name_lower || word_lower == group_prefix_lower {
                    matched_words.insert(word_lower.clone());
                    
                    // If we matched "GTR" and the next word is "E", mark "E" as matched too
                    if word_lower == "gtr" && i + 1 < words.len() {
                        let next_word = words[i + 1].to_lowercase();
                        if next_word == "e" {
                            matched_words.insert("e".to_string());
                        }
                    }
                }
            }
            
            // ============================================
            // PASS 2: Narrow down within the parent group (find children)
            // ============================================
            let child_path = Self::find_child_path(parent_group, &name_to_match, &original_base);
            
            // Build the full prefix path from all parent groups
            let mut prefix_path = Vec::new();
            Self::collect_prefix_path(parent_group, &child_path, &mut prefix_path);
            
            // Check if "GTR E" pattern is present (for Electric Guitar)
            let mut prefix_to_use = if !prefix_path.is_empty() {
                prefix_path.join(" ")
            } else {
                parent_group.prefix.clone()
            };
            
            // If this is Electric Guitar and we have "GTR E" in the name, use "GTR E" as prefix
            if parent_group.name == "Guitar Electric" || parent_group.prefix == "GTR" {
                if name_to_match.contains("gtr e") || name_to_match.contains("gtr  e") {
                    for (i, word) in words.iter().enumerate() {
                        let word_lower = word.to_lowercase();
                        if word_lower == "gtr" && i + 1 < words.len() {
                            let next_word = words[i + 1].to_lowercase();
                            if next_word == "e" {
                                prefix_to_use = "GTR E".to_string();
                                break;
                            }
                        }
                    }
                }
            }
            
            // Store the prefix
            track_name.group_prefix = Some(prefix_to_use.clone());
            
            // Set sub-types from child path and mark them as matched
            if !child_path.is_empty() {
                track_name.sub_type = Some(child_path.clone());
                for sub_type in &child_path {
                    let sub_type_lower = sub_type.to_lowercase();
                    matched_words.insert(sub_type_lower.clone());
                    for word in &words {
                        if word.to_lowercase() == sub_type_lower {
                            matched_words.insert(word.to_lowercase());
                        }
                    }
                }
            } else if parent_group.name == "Bass" {
                // Special case: "Bass" by default should match "Bass Guitar" child
                if let Some(bass_guitar_child) = parent_group.find_child("Bass Guitar") {
                    let bass_guitar_prefix_lower = bass_guitar_child.prefix.to_lowercase();
                    let contains_bass = name_lower == "bass"
                        || name_lower == bass_guitar_prefix_lower
                        || name_lower.starts_with(&format!("{} ", bass_guitar_prefix_lower))
                        || name_lower.ends_with(&format!(" {}", bass_guitar_prefix_lower))
                        || name_lower.contains(&format!(" {} ", bass_guitar_prefix_lower));
                    let pattern_matches = bass_guitar_child.patterns.iter().any(|p| {
                        let p_lower = p.to_lowercase();
                        name_lower == p_lower
                            || name_lower.starts_with(&format!("{} ", p_lower))
                            || name_lower.contains(&format!(" {} ", p_lower))
                    });
                    if contains_bass || pattern_matches {
                        track_name.sub_type = Some(vec!["Bass Guitar".to_string()]);
                        matched_words.insert("bass".to_string());
                        matched_words.insert("guitar".to_string());
                    }
                }
            }
            
            // Determine the group context for component extraction
            // If we found a sub_type, use the deepest child group for group-specific patterns
            let group_for_components = if !child_path.is_empty() {
                let mut current_group = parent_group;
                for child_name in &child_path {
                    if let Some(child) = current_group.find_child(child_name) {
                        current_group = child;
                    } else {
                        break;
                    }
                }
                current_group
            } else {
                parent_group
            };
            
            // ============================================
            // PASS 3: Extract components in fts-naming-parser order
            // This chips away at possibilities systematically, reducing ambiguity at each step
            // Order from config.json: RecTag, Performer, Arrangement, Section, Layers, Multi-Mic, Playlist, Track Type
            // ============================================
            // Note: Track Type was already extracted earlier from parentheses
            
            // 1. RecTag (highest priority after group - helps identify recording sessions)
            Self::extract_rec_tag(&mut track_name, &base_name, &name_to_match, group_for_components, &mut matched_words);
            
            // 2. Performer (helps narrow down who played the track)
            Self::extract_performer(&mut track_name, &base_name, &name_to_match, group_for_components, &mut matched_words);
            
            // 3. Arrangement (helps identify playing style/technique)
            Self::extract_arrangement(&mut track_name, &base_name, &name_to_match, group_for_components, &mut matched_words);
            
            // 4. Section (helps identify song section)
            Self::extract_section(&mut track_name, &base_name, &name_to_match, group_for_components, &mut matched_words);
            
            // 5. Layers (helps identify layering/doubling)
            Self::extract_layers(&mut track_name, &base_name, &name_to_match, group_for_components, &mut matched_words);
            
            // 6. Multi-Mic (use deepest group for multi-mic descriptors - helps identify mic positions)
            let group_for_multi_mic = if !child_path.is_empty() {
                let mut current_group = parent_group;
                for child_name in &child_path {
                    if let Some(child) = current_group.find_child(child_name) {
                        current_group = child;
                    } else {
                        break;
                    }
                }
                current_group
            } else {
                parent_group
            };
            Self::extract_multi_mic(&mut track_name, &base_name, &name_to_match, group_for_multi_mic, &mut matched_words);
            
            // 7. Effect (helps identify effects/sends)
            Self::extract_effect(&mut track_name, &base_name, &name_to_match, group_for_components, &mut matched_words);
            
            // 8. Increment (helps identify numbered instances like Tom 1, Tom 2)
            Self::extract_increment(&mut track_name, &base_name, &name_to_match, group_for_components, &mut matched_words);
            
            // 9. Channel (helps identify L/R/C channels)
            Self::extract_channel(&mut track_name, &base_name, &name_to_match, &mut matched_words);
            
            // 10. Playlist (helps identify playlist variants)
            Self::extract_playlist(&mut track_name, &base_name, &name_to_match, &mut matched_words);
            
            // Track Type was already extracted at the beginning from parentheses
        }
        
        // Mark track type word as matched (if it was extracted from parentheses)
        if let Some(ref tt) = track_name.track_type {
            let tt_lower = tt.to_lowercase();
            matched_words.insert(tt_lower);
        }
        
        // Collect unparsed words (only include if they're not part of the expected structure)
        let mut unparsed: Vec<String> = words.iter()
            .filter(|w| {
                let w_lower = w.to_lowercase();
                // Don't include if already matched
                !matched_words.contains(&w_lower)
            })
            .cloned()
            .collect();
        
        // Filter out single-letter uppercase words when a longer prefix already exists
        // This removes standalone letters like "G" when "GTR" is already the prefix
        // Note: Single letters like "L" or "R" should be extracted as channels, not left in unparsed
        if let Some(ref prefix) = track_name.group_prefix {
            let prefix_lower = prefix.to_lowercase();
            // If we have a multi-character prefix, filter out single uppercase letter words
            if prefix_lower.len() > 1 {
                unparsed.retain(|w| {
                    // Keep words that are longer than 1 character
                    // Single letters should have been extracted as channels/other components
                    w.len() > 1
                });
            }
        }
        
        if !unparsed.is_empty() {
            track_name.unparsed_words = Some(unparsed);
        }
        
        track_name
    }
    
    /// Find the best matching parent (top-level) group for a track name
    /// This is PASS 1: Match parent groups first to narrow down possibilities
    fn find_matching_parent_group(&self, name_lower: &str, original: &str) -> Option<(&Group, i32)> {
        let mut best_match: Option<(&Group, i32)> = None;
        
        for group in &self.groups {
            // Only check if this top-level group matches (don't check children yet)
            if let Some(score) = Self::group_matches(group, name_lower, original) {
                let current_best = best_match.as_ref().map(|(_, s)| *s).unwrap_or(0);
                if score > current_best {
                    best_match = Some((group, score));
                }
            }
        }
        
        best_match
    }
    
    /// Find the best matching group for a track name (legacy method, kept for compatibility)
    /// Returns the top-level group and the full path to the deepest matching child
    /// NOTE: This is now split into find_matching_parent_group (PASS 1) and find_child_path (PASS 2)
    fn find_matching_group(&self, name_lower: &str, original: &str) -> Option<(&Group, Vec<String>)> {
        let mut best_match: Option<(&Group, Vec<String>, i32)> = None;
        
        for group in &self.groups {
            // First check if this group matches
            let parent_score = Self::group_matches(group, name_lower, original);
            
            if let Some(score) = parent_score {
                // Try to find child groups recursively (use original for case preservation)
                let child_path = Self::find_child_path(group, name_lower, original);
                
                // Calculate total score (parent + children)
                let total_score = score + (child_path.len() as i32 * 50);
                
                let current_best = best_match.as_ref().map(|(_, _, s)| *s).unwrap_or(0);
                if total_score > current_best {
                    best_match = Some((group, child_path, total_score));
                }
            }
            
            // Also check if any child matches, but ONLY if:
            // 1. Parent group has some match (score > 50), OR
            // 2. The child match is very strong (score > 200) - this handles cases like "HighHat" matching "Hi Hat" child of Cymbals
            // This prevents false positives like "Vocal" matching "Cymbals" child of Drums
            let should_check_children = if let Some(score) = parent_score {
                score > 50 // Parent has at least a weak match
            } else {
                // No parent match - only check children if we get a very strong child match
                // This handles cases like "HighHat" matching "Hi Hat" child of Cymbals
                false // We'll check this after getting child match
            };
            
            if should_check_children {
                let child_match = Self::find_matching_child_in_group(group, name_lower, original);
                if let Some((child_path, child_score)) = child_match {
                    let current_best = best_match.as_ref().map(|(_, _, s)| *s).unwrap_or(0);
                    if child_score > current_best {
                        // Build full path including parent
                        best_match = Some((group, child_path, child_score));
                    }
                }
            } else if parent_score.is_none() {
                // No parent match - check children but require very strong match (score > 200)
                // This handles cases like "HighHat" matching "Hi Hat" child of Cymbals
                let child_match = Self::find_matching_child_in_group(group, name_lower, original);
                if let Some((child_path, child_score)) = child_match {
                    if child_score > 200 {
                        // Very strong child match - safe to use even without parent match
                        let current_best = best_match.as_ref().map(|(_, _, s)| *s).unwrap_or(0);
                        if child_score > current_best {
                            // Build full path including parent
                            best_match = Some((group, child_path, child_score));
                        }
                    }
                }
            }
        }
        
        best_match.map(|(group, path, _)| (group, path))
    }
    
    /// Find if any child of a group matches, returning the path and score
    fn find_matching_child_in_group(group: &Group, name_lower: &str, original: &str) -> Option<(Vec<String>, i32)> {
        // Context-based matching: If parent is Vocals, require vocal keywords
        // BGVs is separate and doesn't require vocal keywords for its children
        let parent_requires_vocal_keyword = group.name == "Vocals";
        let vocal_keywords = ["vox", "vocal", "vocals", "voice"];
        let has_vocal_keyword = if parent_requires_vocal_keyword {
            vocal_keywords.iter().any(|keyword| {
                name_lower == *keyword
                    || name_lower.starts_with(&format!("{} ", keyword))
                    || name_lower.ends_with(&format!(" {}", keyword))
                    || name_lower.contains(&format!(" {} ", keyword))
            })
        } else {
            true // No requirement for non-vocal groups
        };
        
        // Don't check children of Vocals/BGV if no vocal keyword found
        if parent_requires_vocal_keyword && !has_vocal_keyword {
            return None;
        }
        
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
    fn group_matches(group: &Group, name_lower: &str, original: &str) -> Option<i32> {
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
        
        // Check patterns FIRST - if a pattern matches, allow it regardless of keyword checks
        // This ensures "Vocal" matches Vocals via the "vocal" pattern
        let mut pattern_matched = false;
        for pattern in &group.patterns {
            let pattern_lower = pattern.to_lowercase();
            if name_lower == pattern_lower {
                pattern_matched = true;
                break;
            } else if name_lower.starts_with(&format!("{} ", pattern_lower))
                || name_lower.ends_with(&format!(" {}", pattern_lower))
                || name_lower.contains(&format!(" {} ", pattern_lower)) {
                pattern_matched = true;
                break;
            } else if name_lower.contains(&pattern_lower) {
                pattern_matched = true;
                break;
            }
        }
        
        // Context-based matching: Require explicit keywords for certain groups
        // Vocals group requires vocal keywords (vox, vocal, vocals, voice) to match
        // BUT: If a pattern matched, we already know it's valid, so skip the keyword check
        // BGVs is separate and matches based on its own patterns (bgv, bgvs, backing vocal, etc.)
        if group.name == "Vocals" && !pattern_matched {
            let vocal_keywords = ["vox", "vocal", "vocals", "voice"];
            let is_vocal_keyword = vocal_keywords.iter().any(|keyword| name_lower == *keyword);
            let has_vocal_keyword = is_vocal_keyword || vocal_keywords.iter().any(|keyword| {
                name_lower.starts_with(&format!("{} ", keyword))
                    || name_lower.ends_with(&format!(" {}", keyword))
                    || name_lower.contains(&format!(" {} ", keyword))
            });
            if !has_vocal_keyword {
                // No vocal keyword found and no pattern matched - don't match Vocals group
                return None;
            }
        }
        
        // BGVs group should NOT match if it contains vocal-only keywords without BGV patterns
        // This prevents "Vocal" from matching BGVs when it should match Vocals
        if group.name == "BGVs" {
            // Check if name contains BGV-specific patterns
            let bgv_patterns = ["bgv", "bgvs", "backing", "background", "harm", "harmony", "choir", "stack"];
            let has_bgv_pattern = bgv_patterns.iter().any(|pattern| {
                name_lower.contains(pattern)
            });
            
            // If it's just "vocal" or "vox" without BGV patterns, don't match BGVs
            let vocal_only_keywords = ["vocal", "vox", "vocals", "voice"];
            let is_vocal_only = vocal_only_keywords.iter().any(|keyword| {
                name_lower == *keyword || name_lower == format!("{} ", keyword) || name_lower == format!(" {}", keyword)
            });
            
            if is_vocal_only && !has_bgv_pattern {
                // Just vocal keywords without BGV patterns - don't match BGVs
                return None;
            }
        }
        
        // Check if any pattern matches (using word boundaries)
        let mut has_match = false;
        let mut score = 0; // Removed priority system - score based only on match quality
        
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
        
        // Check patterns FIRST (word boundary matching) - patterns are more specific than prefixes
        // This ensures "BGV1" matches "bgv" pattern in Vocals/BGV before matching "B" prefix in Bass
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
            } else if name_lower.starts_with(&pattern_lower) {
                // Pattern at start of name (e.g., "bgv" in "bgv1")
                // Check if next character is not a letter (number, space, etc.)
                if name_lower.len() > pattern_lower.len() {
                    let next_char = name_lower.chars().nth(pattern_lower.len()).unwrap();
                    if !next_char.is_alphabetic() {
                        has_match = true;
                        score += 100; // Strong match for pattern at start
                        break;
                    }
                }
            } else if name_lower.contains(&pattern_lower) {
                has_match = true;
                score += 10;
                break;
            }
        }
        
        // Check prefix (exact word match) - only if no pattern match yet
        // This prevents "BGV1" from matching Bass just because it starts with "B"
        // Also require word boundaries for single-letter prefixes to avoid false matches
        if !has_match {
            let prefix_lower = group.prefix.to_lowercase();
            if name_lower == prefix_lower {
                has_match = true;
                score += 500;
            } else if name_lower.starts_with(&format!("{} ", prefix_lower))
                || name_lower.ends_with(&format!(" {}", prefix_lower))
                || name_lower.contains(&format!(" {} ", prefix_lower)) {
                has_match = true;
                score += 200;
            } else if prefix_lower.len() > 1 && name_lower.contains(&prefix_lower) {
                // Only allow substring match for multi-character prefixes
                has_match = true;
                score += 50;
            } else if prefix_lower.len() == 1 {
                // For single-letter prefixes, require word boundary (start/end of word)
                // This prevents "BGV1" from matching "B" prefix in Bass
                if name_lower.starts_with(&prefix_lower) && name_lower.len() > 1 && !name_lower.chars().nth(1).unwrap().is_alphanumeric() {
                    // Prefix at start followed by non-alphanumeric (e.g., "B " or "B.")
                    has_match = true;
                    score += 200;
                } else if name_lower.ends_with(&prefix_lower) && name_lower.len() > 1 && !name_lower.chars().nth(name_lower.len() - 2).unwrap().is_alphanumeric() {
                    // Prefix at end preceded by non-alphanumeric (e.g., " B" or ".B")
                    has_match = true;
                    score += 200;
                }
                // Don't match single-letter prefixes as substrings (e.g., "B" in "BGV1")
            }
        }
        
        if has_match {
            Some(score)
        } else {
            None
        }
    }
    
    /// Find the path of child groups that match, preserving original case
    fn find_child_path(group: &Group, name_lower: &str, original: &str) -> Vec<String> {
        let mut path = Vec::new();
        Self::find_child_path_recursive(group, name_lower, original, &mut path);
        path
    }
    
    fn find_child_path_recursive(group: &Group, name_lower: &str, original: &str, path: &mut Vec<String>) {
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
            // For child groups, require that at least one pattern matches (not just prefix)
            // This prevents "B Bass" from matching "Bass Guitar" just because "Bass" is the prefix
            let child_name_lower = child.name.to_lowercase();
            let child_prefix_lower = child.prefix.to_lowercase();
            
            // First check if child name matches exactly (highest priority)
            let name_matches = name_lower == child_name_lower
                || name_lower.starts_with(&format!("{} ", child_name_lower))
                || name_lower.ends_with(&format!(" {}", child_name_lower))
                || name_lower.contains(&format!(" {} ", child_name_lower));
            
            // Then check if any child patterns match (required for child groups to avoid false positives)
            let pattern_matches = child.patterns.iter().any(|p| {
                let p_lower = p.to_lowercase();
                name_lower == p_lower
                    || name_lower.starts_with(&format!("{} ", p_lower))
                    || name_lower.ends_with(&format!(" {}", p_lower))
                    || name_lower.contains(&format!(" {} ", p_lower))
            });
            
            // Check prefix matching with restrictions to avoid false positives
            // Allow prefix matching if:
            // 1. No patterns defined (use prefix as fallback), OR
            // 2. Name starts with prefix (e.g., "Bass DI" starts with "Bass" prefix of "Bass Guitar")
            // But don't allow if name ends with prefix (e.g., "B Bass" ending with "Bass" shouldn't match "Bass Guitar")
            let prefix_matches = if child.patterns.is_empty() {
                // No patterns defined, use prefix as fallback
                name_lower == child_prefix_lower
                    || name_lower.starts_with(&format!("{} ", child_prefix_lower))
                    || name_lower.ends_with(&format!(" {}", child_prefix_lower))
                    || name_lower.contains(&format!(" {} ", child_prefix_lower))
            } else {
                // Patterns are defined - only allow prefix matching if name STARTS with prefix
                // This allows "Bass DI" to match "Bass Guitar" (starts with "Bass")
                // But prevents "B Bass" from matching "Bass Guitar" (ends with "Bass", doesn't start with it)
                name_lower == child_prefix_lower
                    || name_lower.starts_with(&format!("{} ", child_prefix_lower))
                    // Don't check ends_with or contains - only starts_with to avoid false positives
            };
            
            // Child matches if name matches, or if patterns match, or if prefix matches (only when no patterns defined)
            let child_matches = name_matches || pattern_matches || prefix_matches;
            
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
        group: &Group,
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
        group: &Group,
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
        group: &Group,
        matched_words: &mut HashSet<String>,
    ) {
        if track_name.performer.is_some() {
            return;
        }
        
        // First check group-specific performer patterns
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
        
        // Fallback: Check against default performer names list
        // Split the name into words and check each word
        let words: Vec<String> = split_unquoted_whitespace(original)
            .unwrap_quotes(true)
            .map(|w| w.to_string())
            .collect();
        
        for word in &words {
            let word_lower = word.to_lowercase();
            // Skip if already matched
            if matched_words.contains(&word_lower) {
                continue;
            }
            
            // Check if this word is a known performer name
            if let Some(performer_name) = crate::performers::get_performer_name(word) {
                // Found a performer name - use the original case from the input
                track_name.performer = Some(word.clone());
                matched_words.insert(word_lower);
                return;
            }
        }
    }
    
    /// Extract layers component
    fn extract_layers(
        track_name: &mut TrackName,
        original: &str,
        name_lower: &str,
        group: &Group,
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
        group: &Group,
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
        group: &Group,
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
        group: &Group,
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
        group: &Group,
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
    fn collect_prefix_path(group: &Group, child_path: &[String], prefix_path: &mut Vec<String>) {
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

