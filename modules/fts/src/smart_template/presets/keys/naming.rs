//! Keys naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use super::Keys;

/// Get the default Keys group configuration
pub fn default_keys_config() -> GroupConfig {
    GroupConfig {
        name: "Keys".to_string(),
        prefix: "Keys".to_string(),
        patterns: vec!["keys".to_string(), "keyboard".to_string(), "piano".to_string()],
        negative_patterns: vec![],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    }
}

/// Parse a track name into Keys properties
pub fn parse_keys(keys: &Keys, name: &str) -> Result<ItemProperties, KeysParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_keys = props.group_prefix.as_deref() == Some("Keys")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Keys") || s.eq_ignore_ascii_case("Keyboard") || s.eq_ignore_ascii_case("Piano")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                keys.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
            })
            .unwrap_or(false);
    
    if !is_keys {
        return Err(KeysParseError::NotKeysTrack);
    }
    
    Ok(props)
}

#[derive(Debug, thiserror::Error)]
pub enum KeysParseError {
    #[error("Track name does not match keys patterns")]
    NotKeysTrack,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keys_parse() {
        let keys = Keys::new();
        assert!(keys.parse("Piano").is_ok());
        assert!(keys.parse("Kick").is_err());
    }
}
