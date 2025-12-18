//! Cymbals group implementation
//!
//! Complete implementation of the Cymbals group using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - this just validates
//! and extracts Cymbals-specific information.
//! Cymbals includes sub-types: Hi Hat, Ride, and Overheads.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::naming::formatter::Formatter;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Cymbals group parser
pub struct CymbalsParser {
    config: GroupConfig,
}

impl CymbalsParser {
    /// Create a new Cymbals parser with the default Cymbals group config
    pub fn new() -> Self {
        Self {
            config: Self::default_cymbals_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Cymbals group configuration
    pub fn default_cymbals_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Cymbals".to_string(),
            prefix: "C".to_string(),
            patterns: vec![
                "cymbal".to_string(),
                "cymbals".to_string(),
                "hi hat".to_string(),
                "hihat".to_string(),
                "highhat".to_string(),
                "hat".to_string(),
                "oh".to_string(),
                "overhead".to_string(),
                "overheads".to_string(),
            ],
            negative_patterns: vec![],
            parent_track: None,
            destination_track: None,
            insert_mode: Some(InsertMode::Increment),
            increment_start: Some(1),
            only_number_when_multiple: Some(true),
            create_if_missing: Some(true),
            ..Default::default()
        };
        
        // Add sub-type patterns as pattern categories
        let mut pattern_categories = HashMap::new();
        
        // Hi Hat sub-type
        pattern_categories.insert("Hi Hat".to_string(), PatternCategory {
            patterns: vec![
                "hi hat".to_string(),
                "hihat".to_string(),
                "hi%-hat".to_string(),
                "hh".to_string(),
                "hat".to_string(),
                "closed hat".to_string(),
                "open hat".to_string(),
            ],
            required: false,
        });
        
        // Ride sub-type
        pattern_categories.insert("Ride".to_string(), PatternCategory {
            patterns: vec![
                "ride".to_string(),
            ],
            required: false,
        });
        
        // Overheads sub-type
        pattern_categories.insert("Overheads".to_string(), PatternCategory {
            patterns: vec![
                "overhead".to_string(),
                "overheads".to_string(),
                "oh".to_string(),
            ],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for CymbalsParser {
    type Output = ItemProperties;
    type Error = CymbalsParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        // Use ItemPropertiesParser to do all the parsing
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        // Validate that this is a Cymbals track
        let is_cymbals = props.group_prefix.as_deref() == Some("C")
            || props.sub_type.as_ref()
                .map(|st| {
                    st.iter().any(|s| {
                        s.eq_ignore_ascii_case("Cymbals")
                            || s.eq_ignore_ascii_case("Hi Hat")
                            || s.eq_ignore_ascii_case("Ride")
                            || s.eq_ignore_ascii_case("Overheads")
                            || s.eq_ignore_ascii_case("HiHat")
                            || s.eq_ignore_ascii_case("HH")
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
        
        if !is_cymbals {
            return Err(CymbalsParseError::NotCymbalsTrack);
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "cymbals"
    }
}

impl Default for CymbalsParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Cymbals parse error
#[derive(Debug, thiserror::Error)]
pub enum CymbalsParseError {
    #[error("Track name does not match cymbals patterns")]
    NotCymbalsTrack,
    
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
    fn test_cymbals_parser_creation() {
        let parser = CymbalsParser::new();
        assert_eq!(parser.name(), "cymbals");
    }

    #[test]
    fn test_cymbals_config() {
        let config = CymbalsParser::default_cymbals_config();
        
        assert_eq!(config.name, "Cymbals");
        assert_eq!(config.prefix, "C");
        assert!(config.patterns.contains(&"cymbal".to_string()));
        assert!(config.patterns.contains(&"cymbals".to_string()));
        
        // Verify sub-type patterns exist
        if let Some(pattern_categories) = &config.pattern_categories {
            assert!(pattern_categories.contains_key("Hi Hat"));
            assert!(pattern_categories.contains_key("Ride"));
            assert!(pattern_categories.contains_key("Overheads"));
        }
    }

    #[test]
    fn test_cymbals_with_group_config_only() {
        let parser = CymbalsParser::new();
        
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Hi Hat",
                ItemProperties {
                    original_name: Some("Hi Hat".to_string()),
                    group_prefix: Some("C".to_string()),
                    sub_type: Some(vec!["Hi Hat".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "D Hi Hat .1",
                ItemProperties {
                    original_name: Some("D Hi Hat .1".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Hi Hat".to_string()]),
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
            assert_eq!(props.playlist, expected.playlist, "playlist mismatch for: {}", input);
        }
    }

    #[test]
    fn test_cymbals_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Hi Hat",
                ItemProperties {
                    original_name: Some("Hi Hat".to_string()),
                    group_prefix: Some("C".to_string()),
                    sub_type: Some(vec!["Hi Hat".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "D Hi Hat .1",
                ItemProperties {
                    original_name: Some("D Hi Hat .1".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Hi Hat".to_string()]),
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
            assert_eq!(props.playlist, expected.playlist, "playlist mismatch for: {}", input);
        }
    }

    #[test]
    fn test_cymbals_parser_invalid_names() {
        let parser = CymbalsParser::new();
        
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
    fn test_cymbals_parser_sub_types() {
        let parser = CymbalsParser::new();
        
        // Test Hi Hat
        let props = parser.parse("Hi Hat").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Hi Hat") || s.eq_ignore_ascii_case("HiHat")));
        
        // Test Ride
        let props = parser.parse("Ride").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Ride")));
        
        // Test Overheads
        let props = parser.parse("Overheads").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Overheads")));
    }

    #[test]
    fn test_cymbals_parser_playlist() {
        let parser = CymbalsParser::new();
        
        let props = parser.parse("Hi Hat .1").unwrap();
        assert_eq!(props.playlist, Some(".1".to_string()));
    }

    #[test]
    fn test_cymbals_parser_group_identification() {
        let parser = CymbalsParser::new();
        
        // Test that group_prefix is correctly identified
        let props = parser.parse("D Hi Hat").unwrap();
        assert_eq!(props.group_prefix, Some("D".to_string())); // "D" is the Drums prefix
        
        // Test that sub_type contains cymbals-related sub-types when parsed
        let props = parser.parse("Hi Hat").unwrap();
        // The sub_type should contain "Hi Hat", "Ride", or "Overheads"
        assert!(
            props.group_prefix.as_deref() == Some("C") 
            || props.sub_type.as_ref().map(|st| {
                st.iter().any(|s| {
                    s.eq_ignore_ascii_case("Hi Hat")
                        || s.eq_ignore_ascii_case("Ride")
                        || s.eq_ignore_ascii_case("Overheads")
                        || s.eq_ignore_ascii_case("HiHat")
                        || s.eq_ignore_ascii_case("HH")
                })
            }).unwrap_or(false),
            "Cymbals should be identified in group_prefix or sub_type"
        );
        
        // Test with specific sub-type
        let props = parser.parse("Ride").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Ride")));
    }

    #[test]
    fn test_cymbals_parser_full_properties() {
        let parser = CymbalsParser::new();
        
        // Test a full track name with all components
        let props = parser.parse("D Hi Hat .1").unwrap();
        
        // Verify all components are parsed correctly
        assert_eq!(props.original_name, Some("D Hi Hat .1".to_string()));
        assert_eq!(props.group_prefix, Some("D".to_string()));
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| {
            s.eq_ignore_ascii_case("Hi Hat") || s.eq_ignore_ascii_case("HiHat") || s.eq_ignore_ascii_case("HH")
        }));
        assert_eq!(props.playlist, Some(".1".to_string()));
    }
}

/// Integration tests with full default groups configuration
#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::smart_template::naming::parse_fts_item_properties;
    use crate::smart_template::naming::item_properties::ItemProperties;

    #[test]
    fn test_cymbals_conflicts_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Hi Hat",
                ItemProperties {
                    original_name: Some("Hi Hat".to_string()),
                    group_prefix: Some("C".to_string()),
                    sub_type: Some(vec!["Hi Hat".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "Ride",
                ItemProperties {
                    original_name: Some("Ride".to_string()),
                    group_prefix: Some("C".to_string()),
                    sub_type: Some(vec!["Ride".to_string()]),
                    ..Default::default()
                },
            ),
        ];
        
        for (input, expected) in test_cases {
            let props = parse_fts_item_properties(input, None);
            
            assert_eq!(props.original_name, expected.original_name, "original_name mismatch for: {}", input);
            assert_eq!(props.group_prefix, expected.group_prefix, "group_prefix mismatch for: {}", input);
            assert_eq!(props.sub_type, expected.sub_type, "sub_type mismatch for: {}", input);
        }
    }
}
