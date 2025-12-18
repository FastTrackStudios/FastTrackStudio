//! MultiMic component parser
//!
//! Parses multi-mic positions like "In", "Out", "Top", "Bottom"
//! This component supports group-specific descriptors (like Kick's In/Out/Trig/Sub/Ambient)

use super::{ComponentParser, ComponentParseResult, ParseContext};
use once_cell::sync::Lazy;

/// Default MultiMic patterns (generic across all groups)
static DEFAULT_MULTI_MIC_PATTERNS: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "Top".to_string(),
        "Bottom".to_string(),
        "Bot".to_string(),
        "Close".to_string(),
        "Far".to_string(),
        "Room".to_string(),
        "Overhead".to_string(),
        "OH".to_string(),
    ]
});

/// MultiMic component parser
pub struct MultiMicParser;

impl MultiMicParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for MultiMicParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for MultiMicParser {
    fn parse(&self, context: &mut ParseContext) -> ComponentParseResult {
        // First, check group-specific multi-mic descriptors (like Kick's In/Out/Trig/Sub/Ambient)
        if let Some(config) = context.group_config {
            if let Some(pattern_categories) = &config.pattern_categories {
                // Pattern categories are used for group-specific multi-mic descriptors
                for (descriptor_name, category) in pattern_categories {
                    let descriptor_lower = descriptor_name.to_lowercase();
                    
                    // Check if descriptor name matches
                    if context.name_lower.contains(&descriptor_lower) && !context.is_matched(descriptor_name) {
                        context.mark_matched(descriptor_name);
                        return ComponentParseResult::Found(descriptor_name.clone());
                    }
                    
                    // Check if any of the descriptor's patterns match
                    for pattern in &category.patterns {
                        let pattern_lower = pattern.to_lowercase();
                        if context.name_lower.contains(&pattern_lower) && !context.is_matched(pattern) {
                            context.mark_matched(pattern);
                            // Return the descriptor name, not the pattern
                            return ComponentParseResult::Found(descriptor_name.clone());
                        }
                    }
                }
            }
        }
        
        // Fallback to default patterns
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
        DEFAULT_MULTI_MIC_PATTERNS.clone()
    }
    
    fn group_patterns(&self, _context: &ParseContext) -> Option<Vec<String>> {
        // MultiMic uses pattern_categories, not component_patterns
        // So we return None here and handle it in parse()
        None
    }
}
