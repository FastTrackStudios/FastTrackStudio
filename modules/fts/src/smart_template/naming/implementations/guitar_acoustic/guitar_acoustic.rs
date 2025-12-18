//! Guitar Acoustic group implementation
//!
//! Complete implementation of the Guitar Acoustic group using ItemProperties for parsing.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Guitar Acoustic group parser
pub struct GuitarAcousticParser {
    config: GroupConfig,
}

impl GuitarAcousticParser {
    /// Create a new Guitar Acoustic parser with the default config
    pub fn new() -> Self {
        Self {
            config: Self::default_guitar_acoustic_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Guitar Acoustic group configuration
    pub fn default_guitar_acoustic_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Guitar Acoustic".to_string(),
            prefix: "AG".to_string(),
            patterns: vec![
                "acoustic guitar".to_string(),
                "acoustic".to_string(),
                "ac".to_string(),
                "a-gtr".to_string(),
                "steel string".to_string(),
                "nylon".to_string(),
                "ag".to_string(),
            ],
            negative_patterns: vec![
                "bass".to_string(),
                "electric".to_string(),
            ],
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
        
        // Arrangement patterns
        pattern_categories.insert("Arrangement".to_string(), PatternCategory {
            patterns: vec![
                "fingerstyle".to_string(),
                "strum".to_string(),
                "pick".to_string(),
                "classical".to_string(),
                "folk".to_string(),
                "country".to_string(),
                "chug".to_string(),
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
        
        pattern_categories.insert("Stereo".to_string(), PatternCategory {
            patterns: vec!["stereo".to_string(), "Stereo".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Mono".to_string(), PatternCategory {
            patterns: vec!["mono".to_string(), "Mono".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Mic".to_string(), PatternCategory {
            patterns: vec!["mic".to_string(), "Mic".to_string(), "microphone".to_string()],
            required: false,
        });
        
        pattern_categories.insert("DI".to_string(), PatternCategory {
            patterns: vec!["di".to_string(), "DI".to_string(), "direct".to_string()],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for GuitarAcousticParser {
    type Output = ItemProperties;
    type Error = GuitarAcousticParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        let is_guitar_acoustic = props.group_prefix.as_deref() == Some("AG")
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_guitar_acoustic {
            return Err(GuitarAcousticParseError::NotGuitarAcousticTrack);
        }
        
        if let Some(original) = &props.original_name {
            let name_lower = original.to_lowercase();
            let matches_negative = self.config.negative_patterns.iter()
                .any(|neg| name_lower.contains(&neg.to_lowercase()));
            
            if matches_negative {
                return Err(GuitarAcousticParseError::MatchesNegativePattern);
            }
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "guitar_acoustic"
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GuitarAcousticParseError {
    #[error("Track name does not match Guitar Acoustic group")]
    NotGuitarAcousticTrack,
    #[error("Track name matches negative pattern")]
    MatchesNegativePattern,
}

// Note: Formatter is now handled by ItemPropertiesFormatter
// No group-specific formatter needed since all groups use ItemProperties
