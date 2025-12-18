use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;
use super::Cymbals;

use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;

impl Group for Cymbals {
    fn group_name(&self) -> &str {
        "Cymbals"
    }

    fn group_config(&self) -> GroupConfig {
        let mut config = GroupConfig {
            name: "Cymbals".to_string(),
            prefix: "Cymbals".to_string(),
            patterns: vec!["cymbal".to_string(), "oh".to_string(), "overhead".to_string(), "hat".to_string(), "hh".to_string(), "ride".to_string(), "crash".to_string()],
            negative_patterns: vec![],
            parent_track: None,
            destination_track: None,
            insert_mode: Some(InsertMode::Increment),
            increment_start: Some(1),
            only_number_when_multiple: Some(true),
            create_if_missing: Some(true),
            ..Default::default()
        };
        
        let mut multi_mic_descriptors = HashMap::new();
        multi_mic_descriptors.insert("Hi Hat".to_string(), PatternCategory { patterns: vec!["Hat".to_string(), "hh".to_string(), "High Hat".to_string()], required: false });
        multi_mic_descriptors.insert("Ride".to_string(), PatternCategory { patterns: vec!["ride".to_string()], required: false });
        multi_mic_descriptors.insert("Overheads".to_string(), PatternCategory { patterns: vec!["OH".to_string(), "overhead".to_string()], required: false });
        
        config.pattern_categories = Some(multi_mic_descriptors);
        config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Cymbals {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_cymbals = props.group_prefix.as_deref() == Some("Cymbals")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Cymbals")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_cymbals {
            return Err(TemplateParseError::NotMatch("Cymbals".to_string()));
        }
        
        Ok(props)
    }
}
