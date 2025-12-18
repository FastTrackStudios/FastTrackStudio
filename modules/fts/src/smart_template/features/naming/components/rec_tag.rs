//! RecTag component parser
//!
//! Parses recording tags like "PASS-01", "TAKE-02", "REC-01"

use super::{ComponentParser, ComponentParseResult, ParseContext};
use regex::Regex;
use once_cell::sync::Lazy;

/// Default RecTag patterns
static DEFAULT_REC_TAG_PATTERNS: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "PASS".to_string(),
        "TAKE".to_string(),
        "REC".to_string(),
        "RECORDING".to_string(),
    ]
});

/// Regex for matching RecTag patterns (e.g., "PASS-01", "TAKE-02")
static REC_TAG_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"\b(PASS|TAKE|REC|RECORDING)[\s_-]?(\d+)\b").unwrap()
});

/// RecTag component parser
pub struct RecTagParser;

impl RecTagParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for RecTagParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for RecTagParser {
    fn parse_item_properties(&self, context: &mut ParseContext) -> ComponentParseResult {
        // Try to match RecTag pattern using regex
        if let Some(captures) = REC_TAG_REGEX.captures(context.original_name) {
            if let Some(full_match) = captures.get(0) {
                let matched_str = full_match.as_str();
                
                // Check if already matched
                if context.is_matched(matched_str) {
                    return ComponentParseResult::Skipped;
                }
                
                // Mark as matched
                context.mark_matched(matched_str);
                
                // Return the full match (e.g., "PASS-01")
                return ComponentParseResult::Found(matched_str.to_string());
            }
        }
        
        // Fallback: check for RecTag patterns in the name
        for pattern in self.default_patterns() {
            let pattern_lower = pattern.to_lowercase();
            if context.name_lower.contains(&pattern_lower) && !context.is_matched(&pattern) {
                // Try to find the full RecTag (pattern + number)
                if let Some(pos) = context.name_lower.find(&pattern_lower) {
                    // Look for number after the pattern
                    let after_pattern = &context.original_name[pos + pattern.len()..];
                    if let Some(num_match) = Regex::new(r"[\s_-]?(\d+)").unwrap().find(after_pattern) {
                        let full_tag = format!("{}{}", pattern, num_match.as_str());
                        if !context.is_matched(&full_tag) {
                            context.mark_matched(&full_tag);
                            return ComponentParseResult::Found(full_tag);
                        }
                    }
                }
            }
        }
        
        ComponentParseResult::NotFound
    }
    
    fn default_patterns(&self) -> Vec<String> {
        DEFAULT_REC_TAG_PATTERNS.clone()
    }
    
    fn group_patterns(&self, context: &ParseContext) -> Option<Vec<String>> {
        context.group_config.and_then(|config| {
            config.component_patterns.as_ref().and_then(|patterns| {
                patterns.get("rec_tag").cloned()
            })
        })
    }
}
