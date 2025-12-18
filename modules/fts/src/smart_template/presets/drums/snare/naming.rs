//! Snare naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;
use super::Snare;

/// Get the default Snare group configuration
pub fn default_snare_config() -> GroupConfig {
    let mut config = GroupConfig {
        name: "Snare".to_string(),
        prefix: "Snare".to_string(),
        patterns: vec!["snare".to_string(), "sn".to_string()],
        negative_patterns: vec!["keys".to_string(), "guitar".to_string(), "gtr".to_string(), "bass".to_string()],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    };
    
    let mut component_patterns = HashMap::new();
    component_patterns.insert("effect".to_string(), vec!["verb".to_string(), "Verb".to_string(), "reverb".to_string(), "Reverb".to_string()]);
    config.component_patterns = Some(component_patterns);
    
    let mut multi_mic_descriptors = HashMap::new();
    multi_mic_descriptors.insert("Top".to_string(), PatternCategory { patterns: vec!["top".to_string(), "Top".to_string()], required: false });
    multi_mic_descriptors.insert("Bottom".to_string(), PatternCategory { patterns: vec!["bottom".to_string(), "Bottom".to_string(), "bot".to_string()], required: false });
    multi_mic_descriptors.insert("Trig".to_string(), PatternCategory { patterns: vec!["trigger".to_string(), "Trigger".to_string(), "trig".to_string(), "Sample".to_string(), "sample".to_string()], required: false });
    
    config.pattern_categories = Some(multi_mic_descriptors);
    config
}

/// Parse a track name into Snare properties
pub fn parse_snare(snare: &Snare, name: &str) -> Result<ItemProperties, SnareParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_snare = props.group_prefix.as_deref() == Some("Snare")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Snare")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                snare.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
            })
            .unwrap_or(false);
    
    if !is_snare {
        return Err(SnareParseError::NotSnareTrack);
    }
    
    if let Some(original) = &props.original_name {
        let name_lower = original.to_lowercase();
        let matches_negative = snare.config.negative_patterns.iter()
            .any(|p| {
                let neg_lower = p.to_lowercase();
                name_lower == neg_lower
                    || name_lower.starts_with(&format!("{} ", neg_lower))
                    || name_lower.ends_with(&format!(" {}", neg_lower))
                    || name_lower.contains(&format!(" {} ", neg_lower))
            });
        
        if matches_negative {
            return Err(SnareParseError::NegativePatternMatch);
        }
    }
    
    Ok(props)
}

#[derive(Debug, thiserror::Error)]
pub enum SnareParseError {
    #[error("Track name does not match Snare patterns")]
    NotSnareTrack,
    #[error("Track name matches negative pattern")]
    NegativePatternMatch,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snare_parse() {
        let snare = Snare::new();
        assert!(snare.parse("Snare Top").is_ok());
        assert!(snare.parse("Kick").is_err());
    }
}
