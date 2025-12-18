//! Guitar Acoustic naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use super::GuitarAcoustic;

/// Get the default Guitar Acoustic group configuration
pub fn default_guitar_acoustic_config() -> GroupConfig {
    GroupConfig {
        name: "Guitar Acoustic".to_string(),
        prefix: "AG".to_string(),
        patterns: vec!["agtr".to_string(), "acoustic guitar".to_string()],
        negative_patterns: vec!["electric".to_string(), "egtr".to_string()],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    }
}

/// Parse a track name into Guitar Acoustic properties
pub fn parse_guitar_acoustic(guitar: &GuitarAcoustic, name: &str) -> Result<ItemProperties, GuitarAcousticParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_guitar = props.group_prefix.as_deref() == Some("AG")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("AG") || s.eq_ignore_ascii_case("Acoustic Guitar")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                guitar.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
            })
            .unwrap_or(false);
    
    if !is_guitar {
        return Err(GuitarAcousticParseError::NotGuitarAcousticTrack);
    }
    
    Ok(props)
}

#[derive(Debug, thiserror::Error)]
pub enum GuitarAcousticParseError {
    #[error("Track name does not match guitar acoustic patterns")]
    NotGuitarAcousticTrack,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_guitar_acoustic_parse() {
        let guitar = GuitarAcoustic::new();
        assert!(guitar.parse("AGtr 1").is_ok());
        assert!(guitar.parse("Kick").is_err());
    }
}
