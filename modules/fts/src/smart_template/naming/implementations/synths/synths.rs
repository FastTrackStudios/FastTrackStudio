//! Synths group implementation
//!
//! Complete implementation of the Synths group using ItemProperties for parsing.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Synths group parser
pub struct SynthsParser {
    config: GroupConfig,
}

impl SynthsParser {
    /// Create a new Synths parser with the default config
    pub fn new() -> Self {
        Self {
            config: Self::default_synths_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Synths group configuration
    pub fn default_synths_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Synths".to_string(),
            prefix: "SY".to_string(),
            patterns: vec![
                "synth".to_string(),
                "synths".to_string(),
                "nord".to_string(),
                "casio".to_string(),
                "fa06".to_string(),
                "charang".to_string(),
                "briteness".to_string(),
                "moog".to_string(),
                "prophet".to_string(),
                "juno".to_string(),
                "dx7".to_string(),
                "minimoog".to_string(),
                "sub37".to_string(),
                "analog".to_string(),
                "digital".to_string(),
                "vst".to_string(),
                "plugin".to_string(),
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
        
        // Arrangement patterns
        pattern_categories.insert("Arrangement".to_string(), PatternCategory {
            patterns: vec![
                "arp".to_string(),
                "sequence".to_string(),
                "pad".to_string(),
                "lead".to_string(),
                "stab".to_string(),
                "sweep".to_string(),
                "filter".to_string(),
                "cutoff".to_string(),
                "resonance".to_string(),
                "lfo".to_string(),
                "envelope".to_string(),
                "attack".to_string(),
                "decay".to_string(),
                "sustain".to_string(),
                "release".to_string(),
            ],
            required: false,
        });
        
        // Multi-mic descriptors
        pattern_categories.insert("DI".to_string(), PatternCategory {
            patterns: vec!["di".to_string(), "DI".to_string(), "direct".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Line".to_string(), PatternCategory {
            patterns: vec!["line".to_string(), "Line".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Analog".to_string(), PatternCategory {
            patterns: vec!["analog".to_string(), "Analog".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Digital".to_string(), PatternCategory {
            patterns: vec!["digital".to_string(), "Digital".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Mono".to_string(), PatternCategory {
            patterns: vec!["mono".to_string(), "Mono".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Stereo".to_string(), PatternCategory {
            patterns: vec!["stereo".to_string(), "Stereo".to_string()],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for SynthsParser {
    type Output = ItemProperties;
    type Error = SynthsParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        let is_synths = props.group_prefix.as_deref() == Some("SY")
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_synths {
            return Err(SynthsParseError::NotSynthsTrack);
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "synths"
    }
}

#[derive(Debug, thiserror::Error)]
pub enum SynthsParseError {
    #[error("Track name does not match Synths group")]
    NotSynthsTrack,
}

// Note: Formatter is now handled by ItemPropertiesFormatter
// No group-specific formatter needed since all groups use ItemProperties
