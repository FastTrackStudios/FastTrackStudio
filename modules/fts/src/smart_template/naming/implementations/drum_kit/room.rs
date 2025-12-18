//! Room group implementation
//!
//! Complete implementation of the Room group using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - this just validates
//! and extracts Room-specific information.
//! Room uses multi-mic descriptors: Short, Far, Mono.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::naming::formatter::Formatter;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Room group parser
pub struct RoomParser {
    config: GroupConfig,
}

impl RoomParser {
    /// Create a new Room parser with the default Room group config
    pub fn new() -> Self {
        Self {
            config: Self::default_room_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Room group configuration
    pub fn default_room_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Room".to_string(),
            prefix: "R".to_string(),
            patterns: vec![
                "room".to_string(),
                "rooms".to_string(),
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
        
        // Add multi-mic descriptors as pattern categories
        let mut pattern_categories = HashMap::new();
        
        // Short descriptor
        pattern_categories.insert("Short".to_string(), PatternCategory {
            patterns: vec![
                "short".to_string(),
                "Short".to_string(),
                "close".to_string(),
                "Close".to_string(),
            ],
            required: false,
        });
        
        // Far descriptor
        pattern_categories.insert("Far".to_string(), PatternCategory {
            patterns: vec![
                "far".to_string(),
                "Far".to_string(),
                "distant".to_string(),
                "Distant".to_string(),
            ],
            required: false,
        });
        
        // Mono descriptor
        pattern_categories.insert("Mono".to_string(), PatternCategory {
            patterns: vec![
                "mono".to_string(),
                "Mono".to_string(),
                "mon".to_string(),
                "Mon".to_string(),
            ],
            required: false,
        });
        
        config.pattern_categories = Some(pattern_categories);
        
        config
    }
}

impl Parser for RoomParser {
    type Output = ItemProperties;
    type Error = RoomParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        // Use ItemPropertiesParser to do all the parsing
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        // Validate that this is a Room track
        let is_room = props.group_prefix.as_deref() == Some("R")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Room") || s.eq_ignore_ascii_case("Rooms")))
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
        
        if !is_room {
            return Err(RoomParseError::NotRoomTrack);
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "room"
    }
}

impl Default for RoomParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Room parse error
#[derive(Debug, thiserror::Error)]
pub enum RoomParseError {
    #[error("Track name does not match room patterns")]
    NotRoomTrack,
    
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
    fn test_room_parser_creation() {
        let parser = RoomParser::new();
        assert_eq!(parser.name(), "room");
    }

    #[test]
    fn test_room_config() {
        let config = RoomParser::default_room_config();
        
        assert_eq!(config.name, "Room");
        assert_eq!(config.prefix, "R");
        assert!(config.patterns.contains(&"room".to_string()));
        assert!(config.patterns.contains(&"rooms".to_string()));
        
        // Verify multi-mic descriptors exist
        if let Some(pattern_categories) = &config.pattern_categories {
            assert!(pattern_categories.contains_key("Short"));
            assert!(pattern_categories.contains_key("Far"));
            assert!(pattern_categories.contains_key("Mono"));
        }
    }

    #[test]
    fn test_room_with_group_config_only() {
        let parser = RoomParser::new();
        
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Rooms Short",
                ItemProperties {
                    original_name: Some("Rooms Short".to_string()),
                    group_prefix: Some("R".to_string()),
                    sub_type: Some(vec!["Room".to_string()]),
                    multi_mic: Some(vec!["Short".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "D Rooms Short .1",
                ItemProperties {
                    original_name: Some("D Rooms Short .1".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Room".to_string()]),
                    multi_mic: Some(vec!["Short".to_string()]),
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
    fn test_room_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Rooms Short",
                ItemProperties {
                    original_name: Some("Rooms Short".to_string()),
                    group_prefix: Some("R".to_string()),
                    sub_type: Some(vec!["Room".to_string()]),
                    multi_mic: Some(vec!["Short".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "D Rooms Short .1",
                ItemProperties {
                    original_name: Some("D Rooms Short .1".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Room".to_string()]),
                    multi_mic: Some(vec!["Short".to_string()]),
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
    fn test_room_parser_invalid_names() {
        let parser = RoomParser::new();
        
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
    fn test_room_parser_multi_mic() {
        let parser = RoomParser::new();
        
        // Test Short
        let props = parser.parse("Rooms Short").unwrap();
        assert!(props.multi_mic.is_some());
        assert!(props.multi_mic.as_ref().unwrap().contains(&"Short".to_string()));
        
        // Test Far
        let props = parser.parse("Rooms Far").unwrap();
        assert!(props.multi_mic.is_some());
        assert!(props.multi_mic.as_ref().unwrap().contains(&"Far".to_string()));
        
        // Test Mono
        let props = parser.parse("Rooms Mono").unwrap();
        assert!(props.multi_mic.is_some());
        assert!(props.multi_mic.as_ref().unwrap().contains(&"Mono".to_string()));
    }

    #[test]
    fn test_room_parser_playlist() {
        let parser = RoomParser::new();
        
        let props = parser.parse("Rooms Short .1").unwrap();
        assert_eq!(props.playlist, Some(".1".to_string()));
    }

    #[test]
    fn test_room_parser_group_identification() {
        let parser = RoomParser::new();
        
        // Test that group_prefix is correctly identified
        let props = parser.parse("D Rooms Short").unwrap();
        assert_eq!(props.group_prefix, Some("D".to_string())); // "D" is the Drums prefix
        
        // Test that sub_type contains "Room" or "Rooms" when parsed
        let props = parser.parse("Rooms Short").unwrap();
        // The sub_type should contain "Room" or "Rooms" or the group_prefix should be "R"
        assert!(
            props.group_prefix.as_deref() == Some("R") 
            || props.sub_type.as_ref().map(|st| {
                st.iter().any(|s| s.eq_ignore_ascii_case("Room") || s.eq_ignore_ascii_case("Rooms"))
            }).unwrap_or(false),
            "Room should be identified in group_prefix or sub_type"
        );
    }

    #[test]
    fn test_room_parser_full_properties() {
        let parser = RoomParser::new();
        
        // Test a full track name with all components
        let props = parser.parse("D Rooms Short .1").unwrap();
        
        // Verify all components are parsed correctly
        assert_eq!(props.original_name, Some("D Rooms Short .1".to_string()));
        assert_eq!(props.group_prefix, Some("D".to_string()));
        assert!(props.multi_mic.is_some());
        assert_eq!(props.multi_mic.as_ref().unwrap().first(), Some(&"Short".to_string()));
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
    fn test_room_conflicts_with_global_config() {
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Rooms Short",
                ItemProperties {
                    original_name: Some("Rooms Short".to_string()),
                    group_prefix: Some("R".to_string()),
                    sub_type: Some(vec!["Room".to_string()]),
                    multi_mic: Some(vec!["Short".to_string()]),
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
