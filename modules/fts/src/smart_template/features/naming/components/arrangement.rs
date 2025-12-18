//! Arrangement component parser
//!
//! Parses arrangement styles like "Rhythm", "Solo", "Crunch", "Thump", "Click"

use super::{ComponentParser, ComponentParseResult, ParseContext};
use once_cell::sync::Lazy;

/// Default Arrangement patterns (generic across all groups)
static DEFAULT_ARRANGEMENT_PATTERNS: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "Rhythm".to_string(),
        "Solo".to_string(),
        "Crunch".to_string(),
        "Clean".to_string(),
        "Distorted".to_string(),
        "Lead".to_string(),
        "Backing".to_string(),
    ]
});

/// Arrangement component parser
pub struct ArrangementParser;

impl ArrangementParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for ArrangementParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for ArrangementParser {
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
        DEFAULT_ARRANGEMENT_PATTERNS.clone()
    }
    
    fn group_patterns(&self, context: &ParseContext) -> Option<Vec<String>> {
        context.group_config.and_then(|config| {
            config.component_patterns.as_ref().and_then(|patterns| {
                patterns.get("arrangement").cloned()
            })
        })
    }
}
