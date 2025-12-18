use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;
use super::Snare;

use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;

impl Group for Snare {
    fn group_name(&self) -> &str {
        "Snare"
    }

    fn group_config(&self) -> GroupConfig {
        let mut config = GroupConfig {
            name: "Snare".to_string(),
            prefix: "Snare".to_string(),
            patterns: vec!["snare".to_string(), "snr".to_string(), "sn".to_string()],
            negative_patterns: vec![],
            parent_track: None,
            destination_track: None,
            insert_mode: Some(InsertMode::Increment),
            increment_start: Some(1),
            only_number_when_multiple: Some(true),
            create_if_missing: Some(true),
            ..Default::default()
        };
        
        let mut component_patterns = HashMap::new();
        component_patterns.insert("arrangement".to_string(), vec!["Crack".to_string(), "Body".to_string(), "Shell".to_string(), "Snares".to_string(), "Rim".to_string(), "Trig".to_string()]);
        config.component_patterns = Some(component_patterns);
        
        let mut multi_mic_descriptors = HashMap::new();
        multi_mic_descriptors.insert("Top".to_string(), PatternCategory { patterns: vec!["Up".to_string(), "Batter".to_string(), "Attack".to_string()], required: false });
        multi_mic_descriptors.insert("Bottom".to_string(), PatternCategory { patterns: vec!["Down".to_string(), "Snares".to_string(), "Resonant".to_string(), "Side".to_string()], required: false });
        multi_mic_descriptors.insert("Trig".to_string(), PatternCategory { patterns: vec!["trigger".to_string(), "Trigger".to_string(), "Sample".to_string(), "Replacement".to_string()], required: false });
        
        config.pattern_categories = Some(multi_mic_descriptors);
        config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Snare {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_snare = props.group_prefix.as_deref() == Some("Snare")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Snare")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_snare {
            return Err(TemplateParseError::NotMatch("Snare".to_string()));
        }
        
        Ok(props)
    }
}
