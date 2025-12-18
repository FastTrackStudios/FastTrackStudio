//! TrackType component parser
//!
//! Parses track type indicators like "BUS", "SUM", "MIDI", "DI", "NOFX"
//! Typically found in parentheses: "Kick (BUS)", "Snare (SUM)"

use super::{ComponentParser, ComponentParseResult, ParseContext};
use regex::Regex;
use once_cell::sync::Lazy;

/// Default TrackType patterns
static DEFAULT_TRACK_TYPE_PATTERNS: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "BUS".to_string(),
        "SUM".to_string(),
        "MIDI".to_string(),
        "DI".to_string(),
        "NOFX".to_string(),
        "FX".to_string(),
    ]
});

/// Regex for matching track type in parentheses (e.g., "Kick (BUS)")
static TRACK_TYPE_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"^(.+?)\s*\(([^)]+)\)$").unwrap()
});

/// TrackType component parser
pub struct TrackTypeParser;

impl TrackTypeParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for TrackTypeParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for TrackTypeParser {
    fn parse_item_properties(&self, context: &mut ParseContext) -> ComponentParseResult {
        // First, try to extract from parentheses
        if let Some(captures) = TRACK_TYPE_REGEX.captures(context.original_name) {
            if let Some(track_type_match) = captures.get(2) {
                let track_type = track_type_match.as_str().trim();
                
                // Check if already matched
                if context.is_matched(track_type) {
                    return ComponentParseResult::Skipped;
                }
                
                // Mark as matched
                context.mark_matched(track_type);
                
                return ComponentParseResult::Found(track_type.to_string());
            }
        }
        
        // Fallback: check if any track type pattern appears in the name
        for pattern in self.default_patterns() {
            let pattern_lower = pattern.to_lowercase();
            if context.name_lower.contains(&pattern_lower) && !context.is_matched(&pattern) {
                context.mark_matched(&pattern);
                return ComponentParseResult::Found(pattern);
            }
        }
        
        ComponentParseResult::NotFound
    }
    
    fn default_patterns(&self) -> Vec<String> {
        DEFAULT_TRACK_TYPE_PATTERNS.clone()
    }
}
