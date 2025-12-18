//! BGVs (Backing Vocals) group implementation
//!
//! Complete implementation of the BGVs group using ItemProperties for parsing.
//! BGVs is a separate top-level group, not a child of Vocals.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// BGVs group parser
pub struct BGVsParser {
    config: GroupConfig,
}

impl BGVsParser {
    /// Create a new BGVs parser with the default config
    pub fn new() -> Self {
        Self {
            config: Self::default_bgvs_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default BGVs group configuration
    pub fn default_bgvs_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "BGVs".to_string(),
            prefix: "V BGVs".to_string(),
            patterns: vec![
                "v bgvs".to_string(),
                "v bgv".to_string(),
                "bgv".to_string(),
                "bgvs".to_string(),
                "backing vocal".to_string(),
                "background vocal".to_string(),
                "harm".to_string(),
                "harmony".to_string(),
                "choir".to_string(),
                "vocal stack".to_string(),
            ],
            negative_patterns: vec![],
            parent_track: None,
            destination_track: None,
            insert_mode: Some(InsertMode::Increment),
            increment_start: Some(1),
            only_number_when_multiple: Some(true),
            create_if_missing: Some(true),
            priority: Some(10),
            ..Default::default()
        };
        
        let mut pattern_categories = HashMap::new();
        
        // Sub-types
        pattern_categories.insert("Soprano".to_string(), PatternCategory {
            patterns: vec!["soprano".to_string(), "high".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Alto".to_string(), PatternCategory {
            patterns: vec!["alto".to_string(), "mid".to_string(), "middle".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Tenor".to_string(), PatternCategory {
            patterns: vec!["tenor".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Bass".to_string(), PatternCategory {
            patterns: vec!["bass".to_string(), "low".to_string(), "bottom".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Unison".to_string(), PatternCategory {
            patterns: vec!["unison".to_string(), "all".to_string(), "stack".to_string()],
            required: false,
        });
        
        // Arrangement patterns
        pattern_categories.insert("Arrangement".to_string(), PatternCategory {
            patterns: vec![
                "harmony".to_string(),
                "unison".to_string(),
                "octave".to_string(),
                "third".to_string(),
                "fifth".to_string(),
                "stack".to_string(),
                "layer".to_string(),
                "blend".to_string(),
                "call".to_string(),
                "response".to_string(),
                "ah".to_string(),
                "oh".to_string(),
                "la".to_string(),
                "do".to_string(),
            ],
            required: false,
        });
        
        // Multi-mic descriptors
        pattern_categories.insert("Close".to_string(), PatternCategory {
            patterns: vec!["close".to_string(), "Close".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Room".to_string(), PatternCategory {
            patterns: vec!["room".to_string(), "Room".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Condenser".to_string(), PatternCategory {
            patterns: vec!["condenser".to_string(), "Condenser".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Dynamic".to_string(), PatternCategory {
            patterns: vec!["dynamic".to_string(), "Dynamic".to_string()],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for BGVsParser {
    type Output = ItemProperties;
    type Error = BGVsParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        let is_bgvs = props.group_prefix.as_deref() == Some("V BGVs")
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_bgvs {
            return Err(BGVsParseError::NotBGVsTrack);
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "bgvs"
    }
}

#[derive(Debug, thiserror::Error)]
pub enum BGVsParseError {
    #[error("Track name does not match BGVs group")]
    NotBGVsTrack,
}

// Note: Formatter is now handled by ItemPropertiesFormatter
// No group-specific formatter needed since all groups use ItemProperties
