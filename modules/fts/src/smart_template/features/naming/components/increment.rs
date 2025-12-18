//! Increment component parser
//!
//! Parses increment numbers like "1", "2", "3" for numbered instances (Tom 1, Tom 2, etc.)
//! Also parses word-based increments like "Two", "Three", "Four" (Snare Sample Two, etc.)

use super::{ComponentParser, ComponentParseResult, ParseContext};
use regex::Regex;
use once_cell::sync::Lazy;
use std::collections::HashMap;

/// Regex for matching increment numbers at the end of names
/// Matches both "Tom 1" (with space) and "Tom1" (without space)
/// Pattern: number at end, optionally preceded by space or word boundary
static INCREMENT_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"(\d+)\s*$").unwrap()
});

/// Word-based increment mappings (e.g., "Two" -> "2", "Three" -> "3")
static WORD_INCREMENTS: Lazy<HashMap<&'static str, &'static str>> = Lazy::new(|| {
    let mut map = HashMap::new();
    map.insert("one", "1");
    map.insert("two", "2");
    map.insert("three", "3");
    map.insert("four", "4");
    map.insert("five", "5");
    map.insert("six", "6");
    map.insert("seven", "7");
    map.insert("eight", "8");
    map.insert("nine", "9");
    map.insert("ten", "10");
    map.insert("eleven", "11");
    map.insert("twelve", "12");
    map
});

/// Increment component parser
pub struct IncrementParser {
    /// Whether this group supports increments (e.g., Tom supports increments, Kick doesn't)
    supports_increment: bool,
}

impl IncrementParser {
    pub fn new() -> Self {
        Self {
            supports_increment: true, // Default: supports increments
        }
    }
    
    pub fn with_support(mut self, supports: bool) -> Self {
        self.supports_increment = supports;
        self
    }
}

impl Default for IncrementParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for IncrementParser {
    fn parse_item_properties(&self, context: &mut ParseContext) -> ComponentParseResult {
        // Only parse increments if the group supports them
        if !self.supports_increment {
            return ComponentParseResult::Skipped;
        }
        
        // Check if group config indicates increment support
        if let Some(_config) = context.group_config {
            // Check if group type is Increment or if insert_mode is Increment
            // This would be in the config, but for now we'll check the name for common increment patterns
        }
        
        // First, try to match numeric increments at the end of the name (e.g., "Tom 1", "Tom1")
        if let Some(captures) = INCREMENT_REGEX.captures(context.original_name) {
            if let Some(num_match) = captures.get(1) {
                let num_str = num_match.as_str();
                
                // Check if already matched
                if context.is_matched(num_str) {
                    return ComponentParseResult::Skipped;
                }
                
                // Mark as matched
                context.mark_matched(num_str);
                
                return ComponentParseResult::Found(num_str.to_string());
            }
        }
        
        // Then, try to match word-based increments at the end (e.g., "Two", "Three", "Four")
        let name_lower = context.name_lower.clone();
        for (word, num) in WORD_INCREMENTS.iter() {
            // Check if name ends with the word (with optional space before it)
            if name_lower.ends_with(word) {
                // Check if it's a whole word (not part of another word)
                let word_len = word.len();
                if name_lower.len() >= word_len {
                    let before_word = &name_lower[..name_lower.len() - word_len];
                    // Check if it's at the start or preceded by space
                    if before_word.is_empty() || before_word.ends_with(' ') {
                        // Check if already matched
                        if context.is_matched(word) {
                            return ComponentParseResult::Skipped;
                        }
                        
                        // Mark as matched (use the original case from the name)
                        let original_word = &context.original_name[context.original_name.len() - word_len..];
                        context.mark_matched(original_word);
                        
                        return ComponentParseResult::Found(num.to_string());
                    }
                }
            }
        }
        
        ComponentParseResult::NotFound
    }
    
    fn default_patterns(&self) -> Vec<String> {
        // Increments don't have patterns, they're numbers
        Vec::new()
    }
}
