//! Increment component parser
//!
//! Parses increment numbers like "1", "2", "3" for numbered instances (Tom 1, Tom 2, etc.)

use super::{ComponentParser, ComponentParseResult, ParseContext};
use regex::Regex;
use once_cell::sync::Lazy;

/// Regex for matching increment numbers at the end of names
static INCREMENT_REGEX: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r"\b(\d+)\s*$").unwrap()
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
    fn parse(&self, context: &mut ParseContext) -> ComponentParseResult {
        // Only parse increments if the group supports them
        if !self.supports_increment {
            return ComponentParseResult::Skipped;
        }
        
        // Check if group config indicates increment support
        if let Some(config) = context.group_config {
            // Check if group type is Increment or if insert_mode is Increment
            // This would be in the config, but for now we'll check the name for common increment patterns
        }
        
        // Match numbers at the end of the name (e.g., "Tom 1", "Rack 2")
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
        
        ComponentParseResult::NotFound
    }
    
    fn default_patterns(&self) -> Vec<String> {
        // Increments don't have patterns, they're numbers
        Vec::new()
    }
}
