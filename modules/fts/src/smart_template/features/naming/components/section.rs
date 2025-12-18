//! Section component parser
//!
//! Parses song sections like "Intro", "Verse", "Chorus", "Bridge"

use super::{ComponentParser, ComponentParseResult, ParseContext};
use once_cell::sync::Lazy;

/// Default Section patterns
static DEFAULT_SECTION_PATTERNS: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "Intro".to_string(),
        "Verse".to_string(),
        "Chorus".to_string(),
        "Bridge".to_string(),
        "Outro".to_string(),
        "Pre-Chorus".to_string(),
        "Solo".to_string(),
    ]
});

/// Section component parser
pub struct SectionParser;

impl SectionParser {
    pub fn new() -> Self {
        Self
    }
}

impl Default for SectionParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for SectionParser {
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
        DEFAULT_SECTION_PATTERNS.clone()
    }
    
    fn group_patterns(&self, context: &ParseContext) -> Option<Vec<String>> {
        context.group_config.and_then(|config| {
            config.component_patterns.as_ref().and_then(|patterns| {
                patterns.get("section").cloned()
            })
        })
    }
}
