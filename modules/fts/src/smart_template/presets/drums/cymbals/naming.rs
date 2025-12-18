//! Cymbals naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;
use super::Cymbals;

/// Get the default Cymbals group configuration
pub fn default_cymbals_config() -> GroupConfig {
    let mut config = GroupConfig {
        name: "Cymbals".to_string(),
        prefix: "Cymbals".to_string(),
        patterns: vec!["cymbal".to_string(), "cymbals".to_string(), "hi hat".to_string(), "hihat".to_string(), "highhat".to_string(), "hat".to_string(), "oh".to_string(), "overhead".to_string(), "overheads".to_string()],
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
    pattern_categories.insert("Hi Hat".to_string(), PatternCategory { patterns: vec!["hi hat".to_string(), "hihat".to_string(), "hi%-hat".to_string(), "hh".to_string(), "hat".to_string(), "closed hat".to_string(), "open hat".to_string()], required: false });
    pattern_categories.insert("Ride".to_string(), PatternCategory { patterns: vec!["ride".to_string()], required: false });
    pattern_categories.insert("Overheads".to_string(), PatternCategory { patterns: vec!["overhead".to_string(), "overheads".to_string(), "oh".to_string()], required: false });
    
    config.pattern_categories = Some(pattern_categories);
    config
}

/// Parse a track name into Cymbals properties
pub fn parse_cymbals(cymbals: &Cymbals, name: &str) -> Result<ItemProperties, CymbalsParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
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
                cymbals.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
                    || cymbals.config.pattern_categories.as_ref()
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

#[derive(Debug, thiserror::Error)]
pub enum CymbalsParseError {
    #[error("Track name does not match cymbals patterns")]
    NotCymbalsTrack,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_cymbals_parse() {
        let cymbals = Cymbals::new();
        assert!(cymbals.parse("Hi Hat").is_ok());
        assert!(cymbals.parse("Kick").is_err());
    }
}
