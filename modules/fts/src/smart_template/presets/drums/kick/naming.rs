//! Kick naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;
use super::Kick;

/// Get the default Kick group configuration
pub fn default_kick_config() -> GroupConfig {
    let mut config = GroupConfig {
        name: "Kick".to_string(),
        prefix: "Kick".to_string(),
        patterns: vec!["kick".to_string(), "kik".to_string(), "bd".to_string(), "bassdrum".to_string()],
        negative_patterns: vec!["keys".to_string(), "guitar".to_string(), "gtr".to_string(), "k".to_string(), "bass".to_string()],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    };
    
    let mut component_patterns = HashMap::new();
    component_patterns.insert("arrangement".to_string(), vec!["Thump".to_string(), "Click".to_string(), "Sub".to_string(), "Beater".to_string(), "Shell".to_string(), "Fundamental".to_string(), "Harmonic".to_string(), "Trig".to_string()]);
    component_patterns.insert("layers".to_string(), vec!["Click".to_string(), "Thump".to_string(), "Attack".to_string(), "Body".to_string()]);
    config.component_patterns = Some(component_patterns);
    
    let mut multi_mic_descriptors = HashMap::new();
    multi_mic_descriptors.insert("In".to_string(), PatternCategory { patterns: vec!["Inside".to_string(), "Internal".to_string(), "Beater".to_string(), "Attack".to_string()], required: false });
    multi_mic_descriptors.insert("Out".to_string(), PatternCategory { patterns: vec!["Outside".to_string(), "External".to_string(), "Shell".to_string(), "Body".to_string()], required: false });
    multi_mic_descriptors.insert("Trig".to_string(), PatternCategory { patterns: vec!["trigger".to_string(), "Trigger".to_string(), "Sample".to_string(), "Replacement".to_string()], required: false });
    multi_mic_descriptors.insert("Sub".to_string(), PatternCategory { patterns: vec!["Deep".to_string(), "Low".to_string(), "Fundamental".to_string(), "Thump".to_string()], required: false });
    multi_mic_descriptors.insert("Ambient".to_string(), PatternCategory { patterns: vec![], required: false });
    
    config.pattern_categories = Some(multi_mic_descriptors);
    config
}

/// Parse a track name into Kick properties
pub fn parse_kick(kick: &Kick, name: &str) -> Result<ItemProperties, KickParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_kick = props.group_prefix.as_deref() == Some("Kick")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Kick")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                kick.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
            })
            .unwrap_or(false);
    
    if !is_kick {
        return Err(KickParseError::NotKickTrack);
    }
    
    if let Some(original) = &props.original_name {
        let name_lower = original.to_lowercase();
        let matches_negative = kick.config.negative_patterns.iter()
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

#[derive(Debug, thiserror::Error)]
pub enum KickParseError {
    #[error("Track name does not match kick patterns")]
    NotKickTrack,
    #[error("Track name matches negative pattern")]
    NegativePatternMatch,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_kick_parse() {
        let kick = Kick::new();
        assert!(kick.parse("Kick In").is_ok());
        assert!(kick.parse("Snare").is_err());
    }
}
