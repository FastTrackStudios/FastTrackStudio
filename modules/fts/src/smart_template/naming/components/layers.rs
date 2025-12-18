//! Layers component parser
//!
//! Parses layer information like "DBL", "OCT", "L", "R", "Stereo"

use super::{ComponentParser, ComponentParseResult, ParseContext};
use once_cell::sync::Lazy;

/// Default Layers patterns
static DEFAULT_LAYERS_PATTERNS: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "DBL".to_string(),
        "OCT".to_string(),
        "L".to_string(),
        "R".to_string(),
        "Stereo".to_string(),
        "Mono".to_string(),
        "Double".to_string(),
        "Octave".to_string(),
    ]
});

/// Layers component parser
pub struct LayersParser;

impl LayersParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for LayersParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for LayersParser {
    fn parse(&self, context: &mut ParseContext) -> ComponentParseResult {
        // First check group-specific patterns
        if let Some(patterns) = self.group_patterns(context) {
            for pattern in &patterns {
                let pattern_lower = pattern.to_lowercase();
                if context.name_lower.contains(&pattern_lower) && !context.is_matched(pattern) {
                    context.mark_matched(pattern);
                    return ComponentParseResult::Found(pattern.clone());
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
        DEFAULT_LAYERS_PATTERNS.clone()
    }
    
    fn group_patterns(&self, context: &ParseContext) -> Option<Vec<String>> {
        context.group_config.and_then(|config| {
            config.component_patterns.as_ref().and_then(|patterns| {
                patterns.get("layers").cloned()
            })
        })
    }
}
