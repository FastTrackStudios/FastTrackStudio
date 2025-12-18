//! Keys group implementation
//!
//! Complete implementation of the Keys group using ItemProperties for parsing.
//! Keys group includes Piano, Electric Piano, Organ, and Clavinet as sub-types.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Keys group parser
pub struct KeysParser {
    config: GroupConfig,
}

impl KeysParser {
    /// Create a new Keys parser with the default config
    pub fn new() -> Self {
        Self {
            config: Self::default_keys_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Keys group configuration
    pub fn default_keys_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Keys".to_string(),
            prefix: "K".to_string(),
            patterns: vec![
                "keys".to_string(),
                "piano".to_string(),
                "pno".to_string(),
                "nord".to_string(),
                "rhodes".to_string(),
                "wurli".to_string(),
                "keyboard".to_string(),
                "organ".to_string(),
                "hammond".to_string(),
                "b3".to_string(),
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
        pattern_categories.insert("Piano".to_string(), PatternCategory {
            patterns: vec![
                "piano".to_string(),
                "pno".to_string(),
                "grand".to_string(),
                "upright".to_string(),
            ],
            required: false,
        });
        
        pattern_categories.insert("Electric".to_string(), PatternCategory {
            patterns: vec![
                "electric".to_string(),
                "rhodes".to_string(),
                "wurlitzer".to_string(),
                "ep".to_string(),
            ],
            required: false,
        });
        
        pattern_categories.insert("Organ".to_string(), PatternCategory {
            patterns: vec![
                "organ".to_string(),
                "hammond".to_string(),
                "b3".to_string(),
                "leslie".to_string(),
            ],
            required: false,
        });
        
        pattern_categories.insert("Clav".to_string(), PatternCategory {
            patterns: vec![
                "clav".to_string(),
                "clavinet".to_string(),
                "funk".to_string(),
            ],
            required: false,
        });
        
        // Arrangement patterns
        pattern_categories.insert("Arrangement".to_string(), PatternCategory {
            patterns: vec![
                "chord".to_string(),
                "arpeggio".to_string(),
                "scale".to_string(),
                "run".to_string(),
                "glissando".to_string(),
                "trill".to_string(),
                "tremolo".to_string(),
                "sustain".to_string(),
                "staccato".to_string(),
                "legato".to_string(),
            ],
            required: false,
        });
        
        // Multi-mic descriptors
        pattern_categories.insert("DI".to_string(), PatternCategory {
            patterns: vec!["di".to_string(), "DI".to_string(), "direct".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Amp".to_string(), PatternCategory {
            patterns: vec!["amp".to_string(), "Amp".to_string(), "amplifier".to_string()],
            required: false,
        });
        
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
        
        pattern_categories.insert("Lid".to_string(), PatternCategory {
            patterns: vec!["lid".to_string(), "Lid".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Leslie".to_string(), PatternCategory {
            patterns: vec!["leslie".to_string(), "Leslie".to_string()],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for KeysParser {
    type Output = ItemProperties;
    type Error = KeysParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        let is_keys = props.group_prefix.as_deref() == Some("K")
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_keys {
            return Err(KeysParseError::NotKeysTrack);
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "keys"
    }
}

#[derive(Debug, thiserror::Error)]
pub enum KeysParseError {
    #[error("Track name does not match Keys group")]
    NotKeysTrack,
}

// Note: Formatter is now handled by ItemPropertiesFormatter
// No group-specific formatter needed since all groups use ItemProperties
