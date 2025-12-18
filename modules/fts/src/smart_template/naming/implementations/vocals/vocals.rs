//! Vocals group implementation
//!
//! Complete implementation of the Vocals group using ItemProperties for parsing.
//! Vocals group includes Lead Vocal as a sub-type.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Vocals group parser
pub struct VocalsParser {
    config: GroupConfig,
}

impl VocalsParser {
    /// Create a new Vocals parser with the default config
    pub fn new() -> Self {
        Self {
            config: Self::default_vocals_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Vocals group configuration
    pub fn default_vocals_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Vocals".to_string(),
            prefix: "V".to_string(),
            patterns: vec![
                "vocal".to_string(),
                "vox".to_string(),
                "vocals".to_string(),
                "voice".to_string(),
            ],
            negative_patterns: vec![
                "bgv".to_string(),
                "bgvs".to_string(),
                "backing".to_string(),
                "background".to_string(),
                "backing vocal".to_string(),
                "background vocal".to_string(),
                "harm".to_string(),
                "harmony".to_string(),
                "choir".to_string(),
                "vocal stack".to_string(),
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
        
        // Lead Vocal sub-type
        pattern_categories.insert("Lead Vocal".to_string(), PatternCategory {
            patterns: vec![
                "vocal".to_string(),
                "vox".to_string(),
                "lead vox".to_string(),
                "lead vocal".to_string(),
                "singer".to_string(),
            ],
            required: false,
        });
        
        // Arrangement patterns
        pattern_categories.insert("Arrangement".to_string(), PatternCategory {
            patterns: vec![
                "verse".to_string(),
                "chorus".to_string(),
                "bridge".to_string(),
                "ad-lib".to_string(),
                "run".to_string(),
                "riff".to_string(),
                "vibrato".to_string(),
                "bend".to_string(),
                "slide".to_string(),
                "breath".to_string(),
                "whisper".to_string(),
                "belt".to_string(),
                "head".to_string(),
                "chest".to_string(),
                "mixed".to_string(),
                "shout".to_string(),
                "aggressive".to_string(),
                "power".to_string(),
                "scream".to_string(),
                "growl".to_string(),
                "soft".to_string(),
                "intimate".to_string(),
                "breathy".to_string(),
            ],
            required: false,
        });
        
        // Layers patterns
        pattern_categories.insert("Layers".to_string(), PatternCategory {
            patterns: vec![
                "double".to_string(),
                "dbl".to_string(),
                "stack".to_string(),
                "tight".to_string(),
                "loose".to_string(),
                "octave".to_string(),
                "unison".to_string(),
                "harmony".to_string(),
                "lead".to_string(),
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
        
        pattern_categories.insert("Ribbon".to_string(), PatternCategory {
            patterns: vec!["ribbon".to_string(), "Ribbon".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Intimate".to_string(), PatternCategory {
            patterns: vec!["intimate".to_string(), "Intimate".to_string()],
            required: false,
        });
        
        pattern_categories.insert("Robust".to_string(), PatternCategory {
            patterns: vec!["robust".to_string(), "Robust".to_string()],
            required: false,
        });
        
        // Effect patterns
        pattern_categories.insert("Effect".to_string(), PatternCategory {
            patterns: vec![
                "reverb".to_string(),
                "delay".to_string(),
                "chorus".to_string(),
                "flanger".to_string(),
                "distortion".to_string(),
                "autotune".to_string(),
                "h3000".to_string(),
                "eko".to_string(),
                "plate".to_string(),
                "magic".to_string(),
                "vocoder".to_string(),
            ],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for VocalsParser {
    type Output = ItemProperties;
    type Error = VocalsParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        let is_vocals = props.group_prefix.as_deref() == Some("V")
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_vocals {
            return Err(VocalsParseError::NotVocalsTrack);
        }
        
        if let Some(original) = &props.original_name {
            let name_lower = original.to_lowercase();
            let matches_negative = self.config.negative_patterns.iter()
                .any(|neg| name_lower.contains(&neg.to_lowercase()));
            
            if matches_negative {
                return Err(VocalsParseError::MatchesNegativePattern);
            }
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "vocals"
    }
}

#[derive(Debug, thiserror::Error)]
pub enum VocalsParseError {
    #[error("Track name does not match Vocals group")]
    NotVocalsTrack,
    #[error("Track name matches negative pattern")]
    MatchesNegativePattern,
}

// Note: Formatter is now handled by ItemPropertiesFormatter
// No group-specific formatter needed since all groups use ItemProperties
