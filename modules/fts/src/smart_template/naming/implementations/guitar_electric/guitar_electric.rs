//! Guitar Electric group implementation
//!
//! Complete implementation of the Guitar Electric group using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - this just validates
//! and extracts Guitar Electric-specific information.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Guitar Electric group parser
pub struct GuitarElectricParser {
    config: GroupConfig,
}

impl GuitarElectricParser {
    /// Create a new Guitar Electric parser with the default config
    pub fn new() -> Self {
        Self {
            config: Self::default_guitar_electric_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Guitar Electric group configuration
    pub fn default_guitar_electric_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Guitar Electric".to_string(),
            prefix: "GTR".to_string(),
            patterns: vec![
                "guitar".to_string(),
                "gtr".to_string(),
                "gtr e".to_string(),
                "electric guitar".to_string(),
                "electric".to_string(),
                "eg".to_string(),
                "el".to_string(),
                "e-gtr".to_string(),
                "strat".to_string(),
                "les paul".to_string(),
                "tele".to_string(),
                "sg".to_string(),
            ],
            negative_patterns: vec![
                "bass".to_string(),
                "acoustic".to_string(),
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
                "clean".to_string(),
                "crunch".to_string(),
                "distorted".to_string(),
                "lead".to_string(),
                "rhythm".to_string(),
                "power".to_string(),
                "chord".to_string(),
            ],
            required: false,
        });
        
        // Multi-mic descriptors
        pattern_categories.insert("DI".to_string(), PatternCategory {
            patterns: vec![
                "di".to_string(),
                "DI".to_string(),
                "direct".to_string(),
                "Direct".to_string(),
            ],
            required: false,
        });
        
        pattern_categories.insert("Amp".to_string(), PatternCategory {
            patterns: vec![
                "amp".to_string(),
                "Amp".to_string(),
                "amplifier".to_string(),
                "Amplifier".to_string(),
            ],
            required: false,
        });
        
        pattern_categories.insert("Cab".to_string(), PatternCategory {
            patterns: vec![
                "cab".to_string(),
                "Cab".to_string(),
                "cabinet".to_string(),
                "Cabinet".to_string(),
            ],
            required: false,
        });
        
        pattern_categories.insert("Close".to_string(), PatternCategory {
            patterns: vec![
                "close".to_string(),
                "Close".to_string(),
            ],
            required: false,
        });
        
        pattern_categories.insert("Room".to_string(), PatternCategory {
            patterns: vec![
                "room".to_string(),
                "Room".to_string(),
            ],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for GuitarElectricParser {
    type Output = ItemProperties;
    type Error = GuitarElectricParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        let is_guitar_electric = props.group_prefix.as_deref() == Some("GTR")
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_guitar_electric {
            return Err(GuitarElectricParseError::NotGuitarElectricTrack);
        }
        
        if let Some(original) = &props.original_name {
            let name_lower = original.to_lowercase();
            let matches_negative = self.config.negative_patterns.iter()
                .any(|neg| name_lower.contains(&neg.to_lowercase()));
            
            if matches_negative {
                return Err(GuitarElectricParseError::MatchesNegativePattern);
            }
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "guitar_electric"
    }
}

#[derive(Debug, thiserror::Error)]
pub enum GuitarElectricParseError {
    #[error("Track name does not match Guitar Electric group")]
    NotGuitarElectricTrack,
    #[error("Track name matches negative pattern")]
    MatchesNegativePattern,
}

// Note: Formatter is now handled by ItemPropertiesFormatter
// No group-specific formatter needed since all groups use ItemProperties
