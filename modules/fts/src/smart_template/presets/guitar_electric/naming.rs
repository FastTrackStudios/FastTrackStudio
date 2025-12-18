//! Guitar Electric naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use super::GuitarElectric;

/// Get the default Guitar Electric group configuration
pub fn default_guitar_electric_config() -> GroupConfig {
    GroupConfig {
        name: "Guitar Electric".to_string(),
        prefix: "EG".to_string(),
        patterns: vec!["egtr".to_string(), "electric guitar".to_string()],
        negative_patterns: vec!["acoustic".to_string(), "agtr".to_string()],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    }
}

/// Parse a track name into Guitar Electric properties
pub fn parse_guitar_electric(guitar: &GuitarElectric, name: &str) -> Result<ItemProperties, GuitarElectricParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_guitar = props.group_prefix.as_deref() == Some("EG")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("EG") || s.eq_ignore_ascii_case("Electric Guitar")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                guitar.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
            })
            .unwrap_or(false);
    
    if !is_guitar {
        return Err(GuitarElectricParseError::NotGuitarElectricTrack);
    }
    
    Ok(props)
}

#[derive(Debug, thiserror::Error)]
pub enum GuitarElectricParseError {
    #[error("Track name does not match guitar electric patterns")]
    NotGuitarElectricTrack,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guitar_electric_parse() {
        let guitar = GuitarElectric::new();
        assert!(guitar.parse("EGtr 1").is_ok());
        assert!(guitar.parse("Kick").is_err());
    }
}
