//! Channel component parser
//!
//! Parses channel information like "L", "R", "C", "Left", "Right", "Center"

use super::{ComponentParser, ComponentParseResult, ParseContext};
use once_cell::sync::Lazy;

/// Default Channel patterns
static DEFAULT_CHANNEL_PATTERNS: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "L".to_string(),
        "R".to_string(),
        "C".to_string(),
        "Left".to_string(),
        "Right".to_string(),
        "Center".to_string(),
        "Centre".to_string(),
    ]
});

/// Channel component parser
pub struct ChannelParser;

impl ChannelParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ChannelParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for ChannelParser {
    fn parse(&self, context: &mut ParseContext) -> ComponentParseResult {
        // Channel patterns typically appear at the end of the name
        for pattern in self.default_patterns() {
            let pattern_lower = pattern.to_lowercase();
            
            // Check if pattern appears at the end or as a whole word
            if (context.name_lower.ends_with(&format!(" {}", pattern_lower))
                || context.name_lower == pattern_lower)
                && !context.is_matched(&pattern)
            {
                context.mark_matched(&pattern);
                // Return the original case from input
                if let Some(pos) = context.original_name.to_lowercase().rfind(&pattern_lower) {
                    let matched = &context.original_name[pos..pos + pattern.len()];
                    return ComponentParseResult::Found(matched.to_string());
                }
                return ComponentParseResult::Found(pattern);
            }
        }
        
        ComponentParseResult::NotFound
    }
    
    fn default_patterns(&self) -> Vec<String> {
        DEFAULT_CHANNEL_PATTERNS.clone()
    }
}
