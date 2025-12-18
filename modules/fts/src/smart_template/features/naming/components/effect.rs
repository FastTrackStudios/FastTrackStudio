//! Effect component parser
//!
//! Parses effect/send indicators like "Verb", "Delay", "Chorus"

use super::{ComponentParser, ComponentParseResult, ParseContext};
use once_cell::sync::Lazy;

/// Default Effect patterns
static DEFAULT_EFFECT_PATTERNS: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "Verb".to_string(),
        "Reverb".to_string(),
        "Delay".to_string(),
        "Chorus".to_string(),
        "Echo".to_string(),
        "Distortion".to_string(),
        "Dist".to_string(),
    ]
});

/// Effect component parser
pub struct EffectParser;

impl EffectParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for EffectParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for EffectParser {
    fn parse_item_properties(&self, context: &mut ParseContext) -> ComponentParseResult {
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
        DEFAULT_EFFECT_PATTERNS.clone()
    }
    
    fn group_patterns(&self, context: &ParseContext) -> Option<Vec<String>> {
        context.group_config.and_then(|config| {
            config.component_patterns.as_ref().and_then(|patterns| {
                patterns.get("effect").cloned()
            })
        })
    }
}
