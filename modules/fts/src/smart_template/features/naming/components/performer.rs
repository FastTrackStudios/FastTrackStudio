//! Performer component parser
//!
//! Parses performer names like "Cody", "Joshua", "Sarah"
//! Uses a reference list of known performers

use super::{ComponentParser, ComponentParseResult, ParseContext};
use once_cell::sync::Lazy;

/// Default performer names (can be extended by groups)
static DEFAULT_PERFORMER_NAMES: Lazy<Vec<String>> = Lazy::new(|| {
    vec![
        "Cody".to_string(),
        "Joshua".to_string(),
        "Sarah".to_string(),
        // Add more default performers as needed
    ]
});

/// Performer component parser
pub struct PerformerParser {
    /// Additional performer names (can be set per-group)
    additional_names: Vec<String>,
}

impl PerformerParser {
    pub fn new() -> Self {
        Self {
            additional_names: Vec::new(),
        }
    }
    
    pub fn with_names(mut self, names: Vec<String>) -> Self {
        self.additional_names = names;
        self
    }
}

impl Default for PerformerParser {
    fn default() -> Self {
        Self::new()
    }
}

impl ComponentParser for PerformerParser {
    fn parse_item_properties(&self, context: &mut ParseContext) -> ComponentParseResult {
        // Combine default and additional names
        let all_names: Vec<String> = self.default_patterns()
            .into_iter()
            .chain(self.additional_names.iter().cloned())
            .chain(self.group_patterns(context).unwrap_or_default().into_iter())
            .collect();
        
        // Split the name into words and check each
        let words: Vec<&str> = context.original_name.split_whitespace().collect();
        
        for word in words {
            let word_trimmed = word.trim_matches(|c: char| !c.is_alphanumeric());
            if word_trimmed.is_empty() {
                continue;
            }
            
            // Check if this word matches any performer name (case-insensitive)
            for performer_name in &all_names {
                if word_trimmed.eq_ignore_ascii_case(performer_name) && !context.is_matched(word_trimmed) {
                    context.mark_matched(word_trimmed);
                    // Return the original case from the input
                    return ComponentParseResult::Found(word_trimmed.to_string());
                }
            }
        }
        
        ComponentParseResult::NotFound
    }
    
    fn default_patterns(&self) -> Vec<String> {
        DEFAULT_PERFORMER_NAMES.clone()
    }
    
    fn group_patterns(&self, context: &ParseContext) -> Option<Vec<String>> {
        context.group_config.and_then(|config| {
            config.component_patterns.as_ref().and_then(|patterns| {
                patterns.get("performer").cloned()
            })
        })
    }
}
