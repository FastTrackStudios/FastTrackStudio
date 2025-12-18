//! Playlist component parser
//!
//! Parses playlist identifiers like ".1", ".2", ".3", ".A", ".B"

use super::{ComponentParser, ComponentParseResult, ParseContext};
use regex::Regex;
use once_cell::sync::Lazy;

/// Regex for matching playlist patterns (e.g., ".1", ".2", ".A", ".B")
static PLAYLIST_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"\.(\d+|[A-Za-z]+)$").unwrap()
});

/// Playlist component parser
pub struct PlaylistParser;

impl PlaylistParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for PlaylistParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for PlaylistParser {
    fn parse_item_properties(&self, context: &mut ParseContext) -> ComponentParseResult {
        // Match playlist patterns at the end of the name
        if let Some(captures) = PLAYLIST_REGEX.captures(context.original_name) {
            if let Some(matched) = captures.get(0) {
                let matched_str = matched.as_str();
                
                // Check if already matched
                if context.is_matched(matched_str) {
                    return ComponentParseResult::Skipped;
                }
                
                // Mark as matched
                context.mark_matched(matched_str);
                
                return ComponentParseResult::Found(matched_str.to_string());
            }
        }
        
        ComponentParseResult::NotFound
    }
    
    fn default_patterns(&self) -> Vec<String> {
        // Playlists don't have patterns, they're matched by regex
        Vec::new()
    }
}
