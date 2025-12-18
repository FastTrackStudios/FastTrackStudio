//! Bass group implementation
//!
//! Complete implementation of the Bass group using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - this just validates
//! and extracts Bass-specific information.
//!
//! Bass group includes:
//! - **Bass Guitar**: Multi-mic descriptors (DI, Amp)
//! - **Synth Bass**: No multi-mic descriptors

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Bass group parser
pub struct BassParser {
    config: GroupConfig,
}

impl BassParser {
    /// Create a new Bass parser with the default Bass group config
    pub fn new() -> Self {
        Self {
            config: Self::default_bass_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Bass group configuration
    pub fn default_bass_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Bass".to_string(),
            prefix: "Bass".to_string(),
            patterns: vec![
                "bass".to_string(),
            ],
            negative_patterns: vec![
                "bassdrum".to_string(),
                "bd".to_string(),
            ],
            parent_track: None,
            destination_track: None,
            insert_mode: Some(InsertMode::Increment),
            increment_start: Some(1),
            only_number_when_multiple: Some(true),
            create_if_missing: Some(true),
            priority: Some(10), // Higher priority to avoid conflicts with "bassdrum"
            ..Default::default()
        };
        
        // Add sub-type patterns and multi-mic descriptors as pattern categories
        // Pattern categories are used for both sub-types and multi-mic descriptors
        let mut pattern_categories = HashMap::new();
        
        // Bass Guitar sub-type (with multi-mic descriptors)
        pattern_categories.insert("Bass Guitar".to_string(), PatternCategory {
            patterns: vec![
                "bass guitar".to_string(),
                "bassguitar".to_string(),
                "bg".to_string(),
                "electric bass".to_string(),
            ],
            required: false,
        });
        
        // Synth Bass sub-type
        pattern_categories.insert("Synth Bass".to_string(), PatternCategory {
            patterns: vec![
                "synth bass".to_string(),
                "synthbass".to_string(),
                "sb".to_string(),
                "bass synth".to_string(),
            ],
            required: false,
        });
        
        // Add multi-mic descriptors for Bass Guitar (DI, Amp)
        // These are stored as pattern categories - the MultiMic component parser will check these
        // DI descriptor (for Bass Guitar)
        pattern_categories.insert("DI".to_string(), PatternCategory {
            patterns: vec![
                "di".to_string(),
                "DI".to_string(),
                "direct".to_string(),
                "Direct".to_string(),
            ],
            required: false,
        });
        
        // Amp descriptor (for Bass Guitar)
        pattern_categories.insert("Amp".to_string(), PatternCategory {
            patterns: vec![
                "amp".to_string(),
                "Amp".to_string(),
                "amplifier".to_string(),
                "Amplifier".to_string(),
            ],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for BassParser {
    type Output = ItemProperties;
    type Error = BassParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        // Use ItemPropertiesParser to do all the parsing
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        // Validate that this is a Bass track
        // Check if group_prefix matches "Bass" or if sub_type contains "Bass" or bass-related sub-types
        let is_bass = props.group_prefix.as_deref() == Some("Bass")
            || props.sub_type.as_ref()
                .map(|st| {
                    st.iter().any(|s| {
                        s.eq_ignore_ascii_case("Bass")
                            || s.eq_ignore_ascii_case("Bass Guitar")
                            || s.eq_ignore_ascii_case("Synth Bass")
                            || s.eq_ignore_ascii_case("BG")
                            || s.eq_ignore_ascii_case("SB")
                    })
                })
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                        || self.config.pattern_categories.as_ref()
                            .map(|cats| {
                                cats.iter().any(|(_, cat)| {
                                    cat.patterns.iter()
                                        .any(|p| name_lower.contains(&p.to_lowercase()))
                                })
                            })
                            .unwrap_or(false)
                })
                .unwrap_or(false);
        
        if !is_bass {
            return Err(BassParseError::NotBassTrack);
        }
        
        // Check negative patterns (bassdrum, bd to avoid conflicts)
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
                return Err(BassParseError::NegativePatternMatch);
            }
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "bass"
    }
}

impl Default for BassParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Bass parse error
#[derive(Debug, thiserror::Error)]
pub enum BassParseError {
    #[error("Track name does not match bass patterns")]
    NotBassTrack,
    
    #[error("Track name matches negative pattern (bassdrum/bd)")]
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
    fn test_bass_parser_creation() {
        let parser = BassParser::new();
        assert_eq!(parser.name(), "bass");
    }

    #[test]
    fn test_bass_config() {
        let config = BassParser::default_bass_config();
        
        assert_eq!(config.name, "Bass");
        assert_eq!(config.prefix, "Bass");
        assert!(config.patterns.contains(&"bass".to_string()));
        
        // Verify negative patterns
        assert!(config.negative_patterns.contains(&"bassdrum".to_string()));
        assert!(config.negative_patterns.contains(&"bd".to_string()));
        
        // Verify priority
        assert_eq!(config.priority, Some(10));
        
        // Verify sub-type patterns exist
        if let Some(pattern_categories) = &config.pattern_categories {
            assert!(pattern_categories.contains_key("Bass Guitar"));
            assert!(pattern_categories.contains_key("Synth Bass"));
        }
        
        // Verify multi-mic descriptors exist (for Bass Guitar)
        if let Some(pattern_categories) = &config.pattern_categories {
            assert!(pattern_categories.contains_key("DI"));
            assert!(pattern_categories.contains_key("Amp"));
        }
    }

    #[test]
    fn test_bass_with_group_config_only() {
        let parser = BassParser::new();
        
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Bass Guitar DI",
                ItemProperties {
                    original_name: Some("Bass Guitar DI".to_string()),
                    group_prefix: Some("Bass".to_string()),
                    sub_type: Some(vec!["Bass Guitar".to_string()]),
                    multi_mic: Some(vec!["DI".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "Bass Guitar DI .1",
                ItemProperties {
                    original_name: Some("Bass Guitar DI .1".to_string()),
                    group_prefix: Some("Bass".to_string()),
                    sub_type: Some(vec!["Bass Guitar".to_string()]),
                    multi_mic: Some(vec!["DI".to_string()]),
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
            assert_eq!(props.playlist, expected.playlist, "playlist mismatch for: {}", input);
        }
    }

    #[test]
    fn test_bass_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Bass Guitar DI",
                ItemProperties {
                    original_name: Some("Bass Guitar DI".to_string()),
                    group_prefix: Some("Bass".to_string()),
                    sub_type: Some(vec!["Bass Guitar".to_string()]),
                    multi_mic: Some(vec!["DI".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "Bass Guitar DI .1",
                ItemProperties {
                    original_name: Some("Bass Guitar DI .1".to_string()),
                    group_prefix: Some("Bass".to_string()),
                    sub_type: Some(vec!["Bass Guitar".to_string()]),
                    multi_mic: Some(vec!["DI".to_string()]),
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
            assert_eq!(props.playlist, expected.playlist, "playlist mismatch for: {}", input);
        }
    }

    #[test]
    fn test_bass_parser_invalid_names() {
        let parser = BassParser::new();
        
        let test_cases = vec![
            "Guitar",
            "Kick",
            "Snare",
            "Tom",
        ];
        
        for name in test_cases {
            let result = parser.parse(name);
            assert!(result.is_err(), "Should have failed to parse: {}", name);
        }
    }

    #[test]
    fn test_bass_parser_negative_patterns() {
        let parser = BassParser::new();
        
        // Test that bassdrum and bd are rejected (negative patterns)
        let result = parser.parse("bassdrum");
        assert!(result.is_err());
        if let Err(e) = result {
            match e {
                BassParseError::NegativePatternMatch => {
                    // Expected
                }
                _ => panic!("Expected NegativePatternMatch, got: {:?}", e),
            }
        }
        
        let result = parser.parse("bd");
        assert!(result.is_err());
    }

    #[test]
    fn test_bass_parser_sub_types() {
        let parser = BassParser::new();
        
        // Test Bass Guitar
        let props = parser.parse("Bass Guitar").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Bass Guitar")));
        
        // Test Synth Bass
        let props = parser.parse("Synth Bass").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Synth Bass")));
    }

    #[test]
    fn test_bass_parser_multi_mic() {
        let parser = BassParser::new();
        
        // Test DI (for Bass Guitar)
        let props = parser.parse("Bass Guitar DI").unwrap();
        assert!(props.multi_mic.is_some());
        assert!(props.multi_mic.as_ref().unwrap().contains(&"DI".to_string()));
        
        // Test Amp (for Bass Guitar)
        let props = parser.parse("Bass Guitar Amp").unwrap();
        assert!(props.multi_mic.is_some());
        assert!(props.multi_mic.as_ref().unwrap().contains(&"Amp".to_string()));
        
        // Test Direct (alternative pattern for DI)
        let props = parser.parse("Bass Guitar Direct").unwrap();
        assert!(props.multi_mic.is_some());
        assert!(props.multi_mic.as_ref().unwrap().contains(&"DI".to_string()));
    }

    #[test]
    fn test_bass_parser_playlist() {
        let parser = BassParser::new();
        
        let props = parser.parse("Bass Guitar DI .1").unwrap();
        assert_eq!(props.playlist, Some(".1".to_string()));
    }

    #[test]
    fn test_bass_parser_group_identification() {
        let parser = BassParser::new();
        
        // Test that group_prefix is correctly identified as "Bass"
        let props = parser.parse("Bass Guitar DI").unwrap();
        assert_eq!(props.group_prefix, Some("Bass".to_string()));
        
        // Test that sub_type contains "Bass Guitar" when parsed
        let props = parser.parse("Bass Guitar").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Bass Guitar")));
        
        // Test Synth Bass
        let props = parser.parse("Synth Bass").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Synth Bass")));
        
        // Test that just "Bass" is recognized
        let props = parser.parse("Bass").unwrap();
        assert!(
            props.group_prefix.as_deref() == Some("Bass")
            || props.sub_type.as_ref().map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Bass"))).unwrap_or(false),
            "Bass should be identified in group_prefix or sub_type"
        );
    }

    #[test]
    fn test_bass_parser_full_properties() {
        let parser = BassParser::new();
        
        // Test a full track name with all components
        let props = parser.parse("Bass Guitar DI .1").unwrap();
        
        // Verify all components are parsed correctly
        assert_eq!(props.original_name, Some("Bass Guitar DI .1".to_string()));
        assert_eq!(props.group_prefix, Some("Bass".to_string()));
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Bass Guitar")));
        assert!(props.multi_mic.is_some());
        assert_eq!(props.multi_mic.as_ref().unwrap().first(), Some(&"DI".to_string()));
        assert_eq!(props.playlist, Some(".1".to_string()));
    }

    #[test]
    fn test_bass_parser_bass_guitar_vs_synth_bass() {
        let parser = BassParser::new();
        
        // Test that Bass Guitar is correctly identified
        let props = parser.parse("Bass Guitar DI").unwrap();
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Bass Guitar")));
        assert!(props.multi_mic.is_some()); // Should have DI
        
        // Test that Synth Bass is correctly identified
        let props = parser.parse("Synth Bass").unwrap();
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Synth Bass")));
        // Synth Bass doesn't have multi-mic descriptors
    }
}

/// Integration tests with full default groups configuration
#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::smart_template::naming::parse_fts_item_properties;
    use crate::smart_template::naming::item_properties::ItemProperties;

    #[test]
    fn test_bass_conflicts_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Bass",
                ItemProperties {
                    original_name: Some("Bass".to_string()),
                    group_prefix: Some("Bass".to_string()),
                    sub_type: Some(vec!["Bass".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "Bass Guitar DI",
                ItemProperties {
                    original_name: Some("Bass Guitar DI".to_string()),
                    group_prefix: Some("Bass".to_string()),
                    sub_type: Some(vec!["Bass Guitar".to_string()]),
                    multi_mic: Some(vec!["DI".to_string()]),
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
        }
    }
}
