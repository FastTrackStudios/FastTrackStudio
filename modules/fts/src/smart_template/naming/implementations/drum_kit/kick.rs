//! Kick group implementation
//!
//! Complete implementation of the Kick group using ItemProperties for parsing.
//! All parsing logic is handled by ItemPropertiesParser - this just validates
//! and extracts Kick-specific information.

use crate::smart_template::naming::item_properties::ItemProperties;
use crate::smart_template::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::naming::formatter::Formatter;
use crate::smart_template::config::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;

/// Kick group parser
pub struct KickParser {
    config: GroupConfig,
}

impl KickParser {
    /// Create a new Kick parser with the default Kick group config
    pub fn new() -> Self {
        Self {
            config: Self::default_kick_config(),
        }
    }
    
    /// Create a parser with a custom config
    pub fn with_config(config: GroupConfig) -> Self {
        Self { config }
    }
    
    /// Get the default Kick group configuration
    pub fn default_kick_config() -> GroupConfig {
        let mut config = GroupConfig {
            name: "Kick".to_string(),
            prefix: "K".to_string(),
            patterns: vec![
                "kick".to_string(),
                "kik".to_string(),
                "bd".to_string(),
                "bassdrum".to_string(),
            ],
            negative_patterns: vec![
                "keys".to_string(),
                "guitar".to_string(),
                "gtr".to_string(),
                "k".to_string(),
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
        
        // Add component patterns for arrangement and layers
        // These match the legacy kick.rs arrangement_patterns and layers_patterns
        let mut component_patterns = HashMap::new();
        component_patterns.insert("arrangement".to_string(), vec![
            "Thump".to_string(),
            "Click".to_string(),
            "Sub".to_string(),
            "Beater".to_string(),
            "Shell".to_string(),
            "Fundamental".to_string(),
            "Harmonic".to_string(),
            "Trig".to_string(),
        ]);
        component_patterns.insert("layers".to_string(), vec![
            "Click".to_string(),
            "Thump".to_string(),
            "Attack".to_string(),
            "Body".to_string(),
        ]);
        config.component_patterns = Some(component_patterns);
        
        // Add multi-mic descriptors as pattern categories
        let mut multi_mic_descriptors = HashMap::new();
        
        // In descriptor
        multi_mic_descriptors.insert("In".to_string(), PatternCategory {
            patterns: vec![
                "Inside".to_string(),
                "Internal".to_string(),
                "Beater".to_string(),
                "Attack".to_string(),
            ],
            required: false,
        });
        
        // Out descriptor
        multi_mic_descriptors.insert("Out".to_string(), PatternCategory {
            patterns: vec![
                "Outside".to_string(),
                "External".to_string(),
                "Shell".to_string(),
                "Body".to_string(),
            ],
            required: false,
        });
        
        // Trig descriptor
        multi_mic_descriptors.insert("Trig".to_string(), PatternCategory {
            patterns: vec![
                "trigger".to_string(),
                "Trigger".to_string(),
                "Sample".to_string(),
                "Replacement".to_string(),
            ],
            required: false,
        });
        
        // Sub descriptor
        multi_mic_descriptors.insert("Sub".to_string(), PatternCategory {
            patterns: vec![
                "Deep".to_string(),
                "Low".to_string(),
                "Fundamental".to_string(),
                "Thump".to_string(),
            ],
            required: false,
        });
        
        // Ambient descriptor (no patterns, but name matches)
        multi_mic_descriptors.insert("Ambient".to_string(), PatternCategory {
            patterns: vec![],
            required: false,
        });
        
        config.pattern_categories = Some(multi_mic_descriptors);
        
        config
    }
}

impl Parser for KickParser {
    type Output = ItemProperties;
    type Error = KickParseError;
    
    fn parse(&self, name: &str) -> Result<Self::Output, Self::Error> {
        // Use ItemPropertiesParser to do all the parsing
        let parser = ItemPropertiesParser::new();
        let props = parser.parse(name);
        
        // Validate that this is a Kick track
        // Check if group_prefix matches "K" or if sub_type contains "Kick"
        let is_kick = props.group_prefix.as_deref() == Some("K")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Kick")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    self.config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_kick {
            return Err(KickParseError::NotKickTrack);
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
                return Err(KickParseError::NegativePatternMatch);
            }
        }
        
        Ok(props)
    }
    
    fn name(&self) -> &str {
        "kick"
    }
}

impl Default for KickParser {
    fn default() -> Self {
        Self::new()
    }
}

/// Kick parse error
#[derive(Debug, thiserror::Error)]
pub enum KickParseError {
    #[error("Track name does not match kick patterns")]
    NotKickTrack,
    
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
    use crate::smart_template::naming::default_groups::create_default_groups;

    #[test]
    fn test_kick_parser_creation() {
        let parser = KickParser::new();
        assert_eq!(parser.name(), "kick");
    }

    #[test]
    fn test_kick_config() {
        let config = KickParser::default_kick_config();
        
        assert_eq!(config.name, "Kick");
        assert_eq!(config.prefix, "K");
        assert!(config.patterns.contains(&"kick".to_string()));
        assert!(config.patterns.contains(&"kik".to_string()));
        assert!(config.patterns.contains(&"bd".to_string()));
        assert!(config.patterns.contains(&"bassdrum".to_string()));
        
        // Verify negative patterns
        assert!(config.negative_patterns.contains(&"keys".to_string()));
        assert!(config.negative_patterns.contains(&"guitar".to_string()));
        
        // Verify multi-mic descriptors exist
        if let Some(pattern_categories) = &config.pattern_categories {
            assert!(pattern_categories.contains_key("In"));
            assert!(pattern_categories.contains_key("Out"));
            assert!(pattern_categories.contains_key("Trig"));
            assert!(pattern_categories.contains_key("Sub"));
            assert!(pattern_categories.contains_key("Ambient"));
        }
    }

    #[test]
    fn test_kick_with_group_config_only() {
        // Test with ONLY kick group config
        let kick_config = KickParser::default_kick_config();
        // Convert GroupConfig to Group (simplified - using parser directly)
        let parser = KickParser::new();
        
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Kick",
                ItemProperties {
                    original_name: Some("Kick".to_string()),
                    group_prefix: Some("K".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "Kick In",
                ItemProperties {
                    original_name: Some("Kick In".to_string()),
                    group_prefix: Some("K".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    multi_mic: Some(vec!["In".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "D Kick In .02",
                ItemProperties {
                    original_name: Some("D Kick In .02".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    multi_mic: Some(vec!["In".to_string()]),
                    playlist: Some(".02".to_string()),
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
    fn test_kick_with_global_config() {
        // Test with global default config (all groups)
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Kick",
                ItemProperties {
                    original_name: Some("Kick".to_string()),
                    group_prefix: Some("K".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "D Kick In .02",
                ItemProperties {
                    original_name: Some("D Kick In .02".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    multi_mic: Some(vec!["In".to_string()]),
                    playlist: Some(".02".to_string()),
                    ..Default::default()
                },
            ),
            (
                "Kick Out",
                ItemProperties {
                    original_name: Some("Kick Out".to_string()),
                    group_prefix: Some("K".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    multi_mic: Some(vec!["Out".to_string()]),
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
    fn test_kick_parser_invalid_names() {
        let parser = KickParser::new();
        
        let invalid_inputs = vec!["Guitar", "Keys", "Bass", "Snare", "Tom"];
        
        for input in invalid_inputs {
            let result = parser.parse(input);
            assert!(result.is_err(), "Should have failed to parse: {}", input);
        }
    }
}

/// Integration tests with full default groups configuration
/// 
/// These tests use the complete default groups setup to test:
/// - Group conflicts and priority
/// - Negative pattern matching across groups
/// - Correct group selection when multiple groups could match
#[cfg(test)]
mod integration_tests {
    use super::*;
    use crate::smart_template::naming::parse_fts_item_properties;

    #[test]
    fn test_kick_conflicts_with_global_config() {
        // Test conflicts and priority with global config
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "Kick",
                ItemProperties {
                    original_name: Some("Kick".to_string()),
                    group_prefix: Some("K".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    ..Default::default()
                },
            ),
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
                "Keys",
                ItemProperties {
                    original_name: Some("Keys".to_string()),
                    // Should NOT be Kick - will be Keys group
                    ..Default::default()
                },
            ),
        ];
        
        for (input, expected) in test_cases {
            let props = parse_fts_item_properties(input, None);
            
            // Verify group identification
            if input == "Kick" {
                assert_eq!(props.group_prefix, expected.group_prefix, "Kick should have group_prefix K");
                assert_eq!(props.sub_type, expected.sub_type, "Kick should have sub_type Kick");
            } else if input == "Bass" {
                // Bass should be identified as Bass, not Kick
                assert_eq!(props.group_prefix, expected.group_prefix, "Bass should have group_prefix Bass");
                assert_ne!(props.group_prefix, Some("K".to_string()), "Bass should NOT be identified as Kick");
            } else if input == "Keys" {
                // Keys should NOT be identified as Kick (negative pattern)
                assert_ne!(props.group_prefix, Some("K".to_string()), "Keys should NOT be identified as Kick");
            }
        }
    }

    #[test]
    fn test_kick_priority_with_global_config() {
        // Test that Kick has correct priority with all groups present
        let test_cases: Vec<(&str, ItemProperties)> = vec![
            (
                "D Kick In",
                ItemProperties {
                    original_name: Some("D Kick In".to_string()),
                    group_prefix: Some("D".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    multi_mic: Some(vec!["In".to_string()]),
                    ..Default::default()
                },
            ),
            (
                "Kick Out",
                ItemProperties {
                    original_name: Some("Kick Out".to_string()),
                    group_prefix: Some("K".to_string()),
                    sub_type: Some(vec!["Kick".to_string()]),
                    multi_mic: Some(vec!["Out".to_string()]),
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
