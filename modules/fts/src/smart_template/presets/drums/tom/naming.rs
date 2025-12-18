//! Tom naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;
use super::Tom;

/// Get the default Tom group configuration
pub fn default_tom_config() -> GroupConfig {
    let mut config = GroupConfig {
        name: "Tom".to_string(),
        prefix: "Tom".to_string(),
        patterns: vec!["tom".to_string(), "tom1".to_string(), "tom2".to_string(), "tom3".to_string(), "tom 1".to_string(), "tom 2".to_string(), "tom 3".to_string()],
        negative_patterns: vec![],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    };
    
    let mut pattern_categories = HashMap::new();
    pattern_categories.insert("Rack".to_string(), PatternCategory { patterns: vec!["rack".to_string(), "rack tom".to_string()], required: false });
    pattern_categories.insert("Floor".to_string(), PatternCategory { patterns: vec!["floor".to_string(), "floor tom".to_string()], required: false });
    pattern_categories.insert("Hi".to_string(), PatternCategory { patterns: vec!["hi".to_string(), "hi tom".to_string(), "high".to_string(), "high tom".to_string()], required: false });
    pattern_categories.insert("Mid".to_string(), PatternCategory { patterns: vec!["mid".to_string(), "mid tom".to_string(), "med".to_string(), "med tom".to_string()], required: false });
    pattern_categories.insert("Low".to_string(), PatternCategory { patterns: vec!["low".to_string(), "low tom".to_string()], required: false });
    
    config.pattern_categories = Some(pattern_categories);
    config
}

/// Parse a track name into Tom properties
pub fn parse_tom(tom: &Tom, name: &str) -> Result<ItemProperties, TomParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
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
                tom.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
                    || tom.config.pattern_categories.as_ref()
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

#[derive(Debug, thiserror::Error)]
pub enum TomParseError {
    #[error("Track name does not match tom patterns")]
    NotTomTrack,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tom_parse() {
        let tom = Tom::new();
        assert!(tom.parse("Tom 1").is_ok());
        assert!(tom.parse("Kick").is_err());
    }
}
