//! Tom group implementation
//!
//! Complete implementation of the Tom group using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - this just validates
//! and extracts Tom-specific information.
//! Tom supports increment-based naming (Tom 1, Tom 2, etc.) and sub-types (Rack, Floor, Hi, Mid, Low).

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::naming::formatter::Formatter;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Tom group parser
pub struct TomParser {
    config: GroupConfig,
}

impl TomParser {
    /// Create a new Tom parser with the default Tom group config
    pub fn new() -> Self {
        Self {
            config: Self::default_tom_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Tom group configuration
    pub fn default_tom_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Tom".to_string(),
            prefix: "T".to_string(),
            patterns: vec![
                "tom".to_string(),
                "tom1".to_string(),
                "tom2".to_string(),
                "tom3".to_string(),
                "tom 1".to_string(),
                "tom 2".to_string(),
                "tom 3".to_string(),
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
        
        // Rack sub-type
        pattern_categories.insert("Rack".to_string(), PatternCategory {
            patterns: vec![
                "rack".to_string(),
                "rack tom".to_string(),
            ],
            required: false,
        });
        
        // Floor sub-type
        pattern_categories.insert("Floor".to_string(), PatternCategory {
            patterns: vec![
                "floor".to_string(),
                "floor tom".to_string(),
            ],
            required: false,
        });
        
        // Hi sub-type
        pattern_categories.insert("Hi".to_string(), PatternCategory {
            patterns: vec![
                "hi".to_string(),
                "hi tom".to_string(),
                "high".to_string(),
                "high tom".to_string(),
            ],
            required: false,
        });
        
        // Mid sub-type
        pattern_categories.insert("Mid".to_string(), PatternCategory {
            patterns: vec![
                "mid".to_string(),
                "mid tom".to_string(),
                "med".to_string(),
                "med tom".to_string(),
            ],
            required: false,
        });
        
        // Low sub-type
        pattern_categories.insert("Low".to_string(), PatternCategory {
            patterns: vec![
                "low".to_string(),
                "low tom".to_string(),
            ],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for TomParser {
    type Output = ItemProperties;
    type Error = TomParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        // Use ItemPropertiesParser to do all the parsing
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        // Validate that this is a Tom track
        // Check if group_prefix matches "T" or if sub_type contains "Tom" or tom-related sub-types
        let is_tom = props.group_prefix.as_deref() == Some("T")
            || props.sub_type.as_ref()
                .map(|st| {
                    st.iter().any(|s| {
                        s.eq_ignore_ascii_case("Tom")
                            || s.eq_ignore_ascii_case("Rack")
                            || s.eq_ignore_ascii_case("Floor")
                            || s.eq_ignore_ascii_case("Hi")
                            || s.eq_ignore_ascii_case("Mid")
                            || s.eq_ignore_ascii_case("Low")
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
        
        if !is_tom {
            return Err(TomParseError::NotTomTrack);
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "tom"
    }
}

impl Default for TomParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Tom parse error
#[derive(Debug, thiserror::Error)]
pub enum TomParseError {
    #[error("Track name does not match tom patterns")]
    NotTomTrack,
    
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
    fn test_tom_parser_creation() {
        let parser = TomParser::new();
        assert_eq!(parser.name(), "tom");
    }

    #[test]
    fn test_tom_config() {
        let config = TomParser::default_tom_config();
        
        assert_eq!(config.name, "Tom");
        assert_eq!(config.prefix, "T");
        assert!(config.patterns.contains(&"tom".to_string()));
        
        // Verify sub-type patterns exist
        if let Some(pattern_categories) = &config.pattern_categories {
            assert!(pattern_categories.contains_key("Rack"));
            assert!(pattern_categories.contains_key("Floor"));
            assert!(pattern_categories.contains_key("Hi"));
            assert!(pattern_categories.contains_key("Mid"));
            assert!(pattern_categories.contains_key("Low"));
        }
    }

    #[test]
    fn test_tom_with_group_config_only() {
        let parser = TomParser::new();
        
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Tom 1",
                ItemProperties {
                    original_name: Some("Tom 1".to_string()),
                    group_prefix: Some("T".to_string()),
                    sub_type: Some(vec!["Tom".to_string()]),
                    increment: Some("1".to_string()),
                    ..Default::default()
                },
            ),
            (
                "D Rack Tom 1 .1",
                ItemProperties {
                    original_name: Some("D Rack Tom 1 .1".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Rack".to_string()]),
                    increment: Some("1".to_string()),
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
            assert_eq!(props.increment, expected.increment, "increment mismatch for: {}", input);
            assert_eq!(props.playlist, expected.playlist, "playlist mismatch for: {}", input);
        }
    }

    #[test]
    fn test_tom_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Tom 1",
                ItemProperties {
                    original_name: Some("Tom 1".to_string()),
                    group_prefix: Some("T".to_string()),
                    sub_type: Some(vec!["Tom".to_string()]),
                    increment: Some("1".to_string()),
                    ..Default::default()
                },
            ),
            (
                "D Rack Tom 1 .1",
                ItemProperties {
                    original_name: Some("D Rack Tom 1 .1".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Rack".to_string()]),
                    increment: Some("1".to_string()),
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
            assert_eq!(props.increment, expected.increment, "increment mismatch for: {}", input);
            assert_eq!(props.playlist, expected.playlist, "playlist mismatch for: {}", input);
        }
    }

    #[test]
    fn test_tom_parser_invalid_names() {
        let parser = TomParser::new();
        
        let test_cases = vec![
            "Guitar",
            "Kick",
            "Snare",
            "Cymbals",
        ];
        
        for name in test_cases {
            let result = parser.parse(name);
            assert!(result.is_err(), "Should have failed to parse: {}", name);
        }
    }

    #[test]
    fn test_tom_parser_increment() {
        let parser = TomParser::new();
        
        // Test increment numbers
        let props = parser.parse("Tom 1").unwrap();
        assert_eq!(props.increment, Some("1".to_string()));
        
        let props = parser.parse("Tom 2").unwrap();
        assert_eq!(props.increment, Some("2".to_string()));
        
        let props = parser.parse("Rack Tom 1").unwrap();
        assert_eq!(props.increment, Some("1".to_string()));
        
        let props = parser.parse("Floor 2").unwrap();
        assert_eq!(props.increment, Some("2".to_string()));
    }

    #[test]
    fn test_tom_parser_sub_types() {
        let parser = TomParser::new();
        
        // Test Rack
        let props = parser.parse("Rack Tom 1").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Rack")));
        
        // Test Floor
        let props = parser.parse("Floor Tom 2").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Floor")));
        
        // Test Hi
        let props = parser.parse("Hi Tom 1").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Hi")));
        
        // Test Mid
        let props = parser.parse("Mid Tom 2").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Mid")));
        
        // Test Low
        let props = parser.parse("Low Tom 1").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Low")));
    }

    #[test]
    fn test_tom_parser_playlist() {
        let parser = TomParser::new();
        
        let props = parser.parse("Tom 1 .1").unwrap();
        assert_eq!(props.playlist, Some(".1".to_string()));
    }

    #[test]
    fn test_tom_parser_group_identification() {
        let parser = TomParser::new();
        
        // Test that group_prefix is correctly identified
        let props = parser.parse("D Tom 1").unwrap();
        assert_eq!(props.group_prefix, Some("D".to_string())); // "D" is the Drums prefix
        
        // Test that sub_type contains "Tom" or tom-related sub-types when parsed
        let props = parser.parse("Tom 1").unwrap();
        // The sub_type should contain "Tom" or the group_prefix should be "T"
        assert!(
            props.group_prefix.as_deref() == Some("T") 
            || props.sub_type.as_ref().map(|st| {
                st.iter().any(|s| {
                    s.eq_ignore_ascii_case("Tom")
                        || s.eq_ignore_ascii_case("Rack")
                        || s.eq_ignore_ascii_case("Floor")
                })
            }).unwrap_or(false),
            "Tom should be identified in group_prefix or sub_type"
        );
        
        // Test with sub-type
        let props = parser.parse("Rack Tom 1").unwrap();
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Rack")));
        assert_eq!(props.increment, Some("1".to_string()));
    }

    #[test]
    fn test_tom_parser_full_properties() {
        let parser = TomParser::new();
        
        // Test a full track name with all components
        let props = parser.parse("D Rack Tom 1 .1").unwrap();
        
        // Verify all components are parsed correctly
        assert_eq!(props.original_name, Some("D Rack Tom 1 .1".to_string()));
        assert_eq!(props.group_prefix, Some("D".to_string()));
        assert!(props.sub_type.is_some());
        assert!(props.sub_type.as_ref().unwrap().iter().any(|s| s.eq_ignore_ascii_case("Rack")));
        assert_eq!(props.increment, Some("1".to_string()));
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
    fn test_tom_conflicts_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Tom 1",
                ItemProperties {
                    original_name: Some("Tom 1".to_string()),
                    group_prefix: Some("T".to_string()),
                    sub_type: Some(vec!["Tom".to_string()]),
                    increment: Some("1".to_string()),
                    ..Default::default()
                },
            ),
            (
                "Rack Tom 1",
                ItemProperties {
                    original_name: Some("Rack Tom 1".to_string()),
                    group_prefix: Some("T".to_string()),
                    sub_type: Some(vec!["Rack".to_string()]),
                    increment: Some("1".to_string()),
                    ..Default::default()
                },
            ),
        ];
        
        for (input, expected) in test_cases {
            let props = parse_fts_item_properties(input, None);
            
            assert_eq!(props.original_name, expected.original_name, "original_name mismatch for: {}", input);
            assert_eq!(props.group_prefix, expected.group_prefix, "group_prefix mismatch for: {}", input);
            assert_eq!(props.sub_type, expected.sub_type, "sub_type mismatch for: {}", input);
            assert_eq!(props.increment, expected.increment, "increment mismatch for: {}", input);
        }
    }
}
