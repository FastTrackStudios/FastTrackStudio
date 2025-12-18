//! Synths naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use super::Synths;

/// Get the default Synths group configuration
pub fn default_synths_config() -> GroupConfig {
    GroupConfig {
        name: "Synths".to_string(),
        prefix: "Synths".to_string(),
        patterns: vec!["synth".to_string(), "synths".to_string(), "pad".to_string(), "lead".to_string()],
        negative_patterns: vec!["bass".to_string()],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    }
}

/// Parse a track name into Synths properties
pub fn parse_synths(synths: &Synths, name: &str) -> Result<ItemProperties, SynthsParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_synths = props.group_prefix.as_deref() == Some("Synths")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Synth") || s.eq_ignore_ascii_case("Synths")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                synths.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
            })
            .unwrap_or(false);
    
    if !is_synths {
        return Err(SynthsParseError::NotSynthsTrack);
    }
    
    Ok(props)
}

#[derive(Debug, thiserror::Error)]
pub enum SynthsParseError {
    #[error("Track name does not match synths patterns")]
    NotSynthsTrack,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_synths_parse() {
        let synths = Synths::new();
        assert!(synths.parse("Synth Pad").is_ok());
        assert!(synths.parse("Kick").is_err());
    }
}
