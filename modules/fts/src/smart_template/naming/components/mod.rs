//! Component-based parsing system
//!
//! Each component type has its own module that defines:
//! - Reference lists/patterns for that component
//! - Parsing logic using the fold pattern
//! - Group-specific overrides/extensions
//!
//! This allows for typesafe, stateless parsing where each component
//! can be parsed independently and combined using fold.

pub mod rec_tag;
pub mod performer;
pub mod arrangement;
pub mod section;
pub mod layers;
pub mod multi_mic;
pub mod effect;
pub mod increment;
pub mod channel;
pub mod playlist;
pub mod track_type;
pub mod fold_parser;

pub use rec_tag::*;
pub use performer::*;
pub use arrangement::*;
pub use section::*;
pub use layers::*;
pub use multi_mic::*;
pub use effect::*;
pub use increment::*;
pub use channel::*;
pub use playlist::*;
pub use track_type::*;
pub use fold_parser::*;

/// Context for component parsing
/// 
/// This is passed to each component parser and contains:
/// - The original name being parsed
/// - The group configuration (for group-specific patterns)
/// - Already matched words (to avoid double-matching)
#[derive(Debug, Clone)]
pub struct ParseContext<'a> {
    /// Original track name being parsed
    pub original_name: &'a str,
    /// Lowercase version for matching
    pub name_lower: String,
    /// Words that have already been matched (to avoid double-matching)
    pub matched_words: std::collections::HashSet<String>,
    /// Group configuration (for group-specific component patterns)
    pub group_config: Option<&'a crate::smart_template::config::group_config::GroupConfig>,
}

impl<'a> ParseContext<'a> {
    pub fn new(original_name: &'a str) -> Self {
        Self {
            original_name,
            name_lower: original_name.to_lowercase(),
            matched_words: std::collections::HashSet::new(),
            group_config: None,
        }
    }
    
    pub fn with_group_config(mut self, config: &'a crate::smart_template::config::group_config::GroupConfig) -> Self {
        self.group_config = Some(config);
        self
    }
    
    /// Mark a word as matched
    pub fn mark_matched(&mut self, word: &str) {
        self.matched_words.insert(word.to_lowercase());
    }
    
    /// Check if a word has been matched
    pub fn is_matched(&self, word: &str) -> bool {
        self.matched_words.contains(&word.to_lowercase())
    }
}

/// Result of parsing a component
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ComponentParseResult {
    /// Component was found and parsed
    Found(String),
    /// Component was not found
    NotFound,
    /// Component parsing was skipped (already present, etc.)
    Skipped,
}

/// Trait for component parsers
/// 
/// Each component type implements this trait to provide:
/// - Default reference lists/patterns
/// - Parsing logic using fold pattern
/// - Support for group-specific overrides
pub trait ComponentParser: Send + Sync {
    /// Parse the component from the context
    /// 
    /// Returns the parsed value if found, or None if not found.
    /// Updates the context to mark matched words.
    fn parse(&self, context: &mut ParseContext) -> ComponentParseResult;
    
    /// Get default patterns/reference list for this component
    fn default_patterns(&self) -> Vec<String>;
    
    /// Get group-specific patterns if available
    fn group_patterns(&self, context: &ParseContext) -> Option<Vec<String>> {
        // Default implementation: check group_config for component-specific patterns
        context.group_config.and_then(|config| {
            config.component_patterns.as_ref().and_then(|patterns| {
                // This will be overridden by each component to look in the right place
                None
            })
        })
    }
}
