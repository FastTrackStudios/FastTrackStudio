//! Vocals naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use super::Vocals;

/// Get the default Vocals group configuration
pub fn default_vocals_config() -> GroupConfig {
    GroupConfig {
        name: "Vocals".to_string(),
        prefix: "V".to_string(),
        patterns: vec!["vox".to_string(), "vocal".to_string(), "vocals".to_string()],
        negative_patterns: vec!["bgv".to_string(), "bkg".to_string(), "back".to_string()],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    }
}

/// Parse a track name into Vocals properties
pub fn parse_vocals(vocals: &Vocals, name: &str) -> Result<ItemProperties, VocalsParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_vocals = props.group_prefix.as_deref() == Some("Vocals")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Vocal") || s.eq_ignore_ascii_case("Vocals")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                vocals.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
            })
            .unwrap_or(false);
    
    if !is_vocals {
        return Err(VocalsParseError::NotVocalsTrack);
    }
    
    if let Some(original) = &props.original_name {
        let name_lower = original.to_lowercase();
        let matches_negative = vocals.config.negative_patterns.iter()
            .any(|p| {
                let neg_lower = p.to_lowercase();
                name_lower.contains(&neg_lower)
            });
        
        if matches_negative {
            return Err(VocalsParseError::NegativePatternMatch);
        }
    }
    
    Ok(props)
}

#[derive(Debug, thiserror::Error)]
pub enum VocalsParseError {
    #[error("Track name does not match vocals patterns")]
    NotVocalsTrack,
    #[error("Track name matches negative pattern (bgv/bkg)")]
    NegativePatternMatch,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vocals_parse() {
        let vocals = Vocals::new();
        assert!(vocals.parse("Lead Vocal").is_ok());
        assert!(vocals.parse("Kick").is_err());
    }
}
