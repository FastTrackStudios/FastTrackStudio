//! Snare group implementation
//!
//! Complete implementation of the Snare group using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - this just validates
//! and extracts Snare-specific information.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::naming::formatter::Formatter;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Snare group parser
pub struct SnareParser {
    config: GroupConfig,
}

impl SnareParser {
    /// Create a new Snare parser with the default Snare group config
    pub fn new() -> Self {
        Self {
            config: Self::default_snare_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Snare group configuration
    pub fn default_snare_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Snare".to_string(),
            prefix: "S".to_string(),
            patterns: vec![
                "snare".to_string(),
                "sn".to_string(),
            ],
            negative_patterns: vec![
                "keys".to_string(),
                "guitar".to_string(),
                "gtr".to_string(),
                "bass".to_string(),
            ],
            parent_track: None,
            destination_track: None,
            insert_mode: Some(InsertMode::Increment),
            increment_start: Some(1),
            only_number_when_multiple: Some(true),
            create_if_missing: Some(true),
            ..Default::default()
        };
        
        // Add component patterns for effects (Verb/Reverb)
        let mut component_patterns = HashMap::new();
        component_patterns.insert("effect".to_string(), vec![
            "verb".to_string(),
            "Verb".to_string(),
            "reverb".to_string(),
            "Reverb".to_string(),
        ]);
        config.component_patterns = Some(component_patterns);
        
        // Add pattern categories for multi-mic descriptors
        let mut multi_mic_descriptors = HashMap::new();
        
        // Top descriptor
        multi_mic_descriptors.insert("Top".to_string(), PatternCategory {
            patterns: vec![
                "top".to_string(),
                "Top".to_string(),
            ],
            required: false,
        });
        
        // Bottom descriptor
        multi_mic_descriptors.insert("Bottom".to_string(), PatternCategory {
            patterns: vec![
                "bottom".to_string(),
                "Bottom".to_string(),
                "bot".to_string(),
            ],
            required: false,
        });
        
        // Trig descriptor
        multi_mic_descriptors.insert("Trig".to_string(), PatternCategory {
            patterns: vec![
                "trigger".to_string(),
                "Trigger".to_string(),
                "trig".to_string(),
            ],
            required: false,
        });
        
        config.pattern_categories = Some(multi_mic_descriptors);
        
        config
    }
}

impl Parser for SnareParser {
    type Output = ItemProperties;
    type Error = SnareParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        // Use ItemPropertiesParser to do all the parsing
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        // Validate that this is a Snare track
        let is_snare = props.group_prefix.as_deref() == Some("S")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Snare")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_snare {
            return Err(SnareParseError::NotSnareTrack);
        }
        
        // Check negative patterns
        if let Some(original) = &props.original_name {
            let name_lower = original.to_lowercase();
            let matches_negative = self.config.negative_patterns.iter()
                .any(|p| {
                    let neg_lower = p.to_lowercase();
                    name_lower == neg_lower
                        || name_lower.starts_with(&format!("{} ", neg_lower))
                        || name_lower.ends_with(&format!(" {}", neg_lower))
                        || name_lower.contains(&format!(" {} ", neg_lower))
                });
            
            if matches_negative {
                return Err(SnareParseError::NegativePatternMatch);
            }
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "snare"
    }
}

impl Default for SnareParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Snare parse error
#[derive(Debug, thiserror::Error)]
pub enum SnareParseError {
    #[error("Track name does not match Snare patterns")]
    NotSnareTrack,
    
    #[error("Track name matches negative pattern")]
    NegativePatternMatch,
    
    #[error("Parse error: {0}")]
    Other(String),
}

// Note: Formatter is now handled by ItemPropertiesFormatter
// No group-specific formatter needed since all groups use ItemProperties

#[cfg(test)]
mod tests {
    use super::*;
    use crate::smart_template::naming::parse_fts_item_properties;
    use crate::smart_template::naming::item_properties::ItemProperties;

    #[test]
    fn test_snare_parser_creation() {
        let parser = SnareParser::new();
        assert_eq!(parser.name(), "snare");
    }

    #[test]
    fn test_snare_config() {
        let config = SnareParser::default_snare_config();
        
        assert_eq!(config.name, "Snare");
        assert_eq!(config.prefix, "S");
        assert!(config.patterns.contains(&"snare".to_string()));
        assert!(config.patterns.contains(&"sn".to_string()));
        
        // Verify multi-mic descriptors exist
        if let Some(pattern_categories) = &config.pattern_categories {
            assert!(pattern_categories.contains_key("Top"));
            assert!(pattern_categories.contains_key("Bottom"));
            assert!(pattern_categories.contains_key("Trig"));
        }
        
        // Verify effect patterns
        if let Some(component_patterns) = &config.component_patterns {
            assert!(component_patterns.contains_key("effect"));
            if let Some(effects) = component_patterns.get("effect") {
                assert!(effects.contains(&"verb".to_string()));
                assert!(effects.contains(&"Verb".to_string()));
            }
        }
    }

    #[test]
    fn test_snare_with_group_config_only() {
        let parser = SnareParser::new();
        
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Snare",
                ItemProperties {
                    original_name: Some("Snare".to_string()),
                    group_prefix: Some("S".to_string()),
                    sub_type: Some(vec!["Snare".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "D Snare Top Verb .1",
                ItemProperties {
                    original_name: Some("D Snare Top Verb .1".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Snare".to_string()]),
                    multi_mic: Some(vec!["Top".to_string()]),
                    effect: Some(vec!["Verb".to_string()]),
                    playlist: Some(".1".to_string()),
                    ..Default::default()
                },
            ),
        ];
        
        for (input, expected) in test_cases {
            let result = parser.parse(input);
            assert!(result.is_ok(), "Failed to parse: {}", input);
            let props = result.unwrap();
            
            assert_eq!(props.original_name, expected.original_name, "original_name mismatch for: {}", input);
            assert_eq!(props.group_prefix, expected.group_prefix, "group_prefix mismatch for: {}", input);
            assert_eq!(props.sub_type, expected.sub_type, "sub_type mismatch for: {}", input);
            assert_eq!(props.multi_mic, expected.multi_mic, "multi_mic mismatch for: {}", input);
            assert_eq!(props.effect, expected.effect, "effect mismatch for: {}", input);
            assert_eq!(props.playlist, expected.playlist, "playlist mismatch for: {}", input);
        }
    }

    #[test]
    fn test_snare_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Snare Top",
                ItemProperties {
                    original_name: Some("Snare Top".to_string()),
                    group_prefix: Some("S".to_string()),
                    sub_type: Some(vec!["Snare".to_string()]),
                    multi_mic: Some(vec!["Top".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "D Snare Top Verb .1",
                ItemProperties {
                    original_name: Some("D Snare Top Verb .1".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Snare".to_string()]),
                    multi_mic: Some(vec!["Top".to_string()]),
                    effect: Some(vec!["Verb".to_string()]),
                    playlist: Some(".1".to_string()),
                    ..Default::default()
                },
            ),
        ];
        
        for (input, expected) in test_cases {
            let props = parse_fts_item_properties(input, None);
            
            assert_eq!(props.original_name, expected.original_name, "original_name mismatch for: {}", input);
            assert_eq!(props.group_prefix, expected.group_prefix, "group_prefix mismatch for: {}", input);
            assert_eq!(props.sub_type, expected.sub_type, "sub_type mismatch for: {}", input);
            assert_eq!(props.multi_mic, expected.multi_mic, "multi_mic mismatch for: {}", input);
            assert_eq!(props.effect, expected.effect, "effect mismatch for: {}", input);
            assert_eq!(props.playlist, expected.playlist, "playlist mismatch for: {}", input);
        }
    }

    #[test]
    fn test_snare_parser_invalid_names() {
        let parser = SnareParser::new();
        
        let invalid_inputs = vec!["Guitar", "Keys", "Kick", "Tom"];
        
        for input in invalid_inputs {
            let result = parser.parse(input);
            assert!(result.is_err(), "Should have failed to parse: {}", input);
        }
    }
}

/// Integration tests with full default groups configuration
#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::smart_template::naming::parse_fts_item_properties;
    use crate::smart_template::naming::item_properties::ItemProperties;

    #[test]
    fn test_snare_conflicts_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Snare Top",
                ItemProperties {
                    original_name: Some("Snare Top".to_string()),
                    group_prefix: Some("S".to_string()),
                    sub_type: Some(vec!["Snare".to_string()]),
                    multi_mic: Some(vec!["Top".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "Keys",
                ItemProperties {
                    original_name: Some("Keys".to_string()),
                    // Should NOT be Snare
                    ..Default::default()
                },
            ),
        ];
        
        for (input, expected) in test_cases {
            let props = parse_fts_item_properties(input, None);
            
            if input == "Snare Top" {
                assert_eq!(props.group_prefix, expected.group_prefix, "Snare should have group_prefix S");
                assert_eq!(props.sub_type, expected.sub_type, "Snare should have sub_type Snare");
            } else if input == "Keys" {
                assert_ne!(props.group_prefix, Some("S".to_string()), "Keys should NOT be identified as Snare");
            }
        }
    }
}
