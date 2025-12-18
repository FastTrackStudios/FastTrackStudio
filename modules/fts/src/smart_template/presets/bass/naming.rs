//! Bass naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;
use super::Bass;

use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;

impl Group for Bass {
    fn group_name(&self) -> &str {
        "Bass"
    }

    fn group_config(&self) -> GroupConfig {
        let mut config = GroupConfig {
            name: "Bass".to_string(),
            prefix: "Bass".to_string(),
            patterns: vec!["bass".to_string()],
            negative_patterns: vec!["bassdrum".to_string(), "bd".to_string()],
            parent_track: None,
            destination_track: None,
            insert_mode: Some(InsertMode::Increment),
            increment_start: Some(1),
            only_number_when_multiple: Some(true),
            create_if_missing: Some(true),
            priority: Some(10),
            ..Default::default()
        };
        
        let mut pattern_categories = HashMap::new();
        pattern_categories.insert("Bass Guitar".to_string(), PatternCategory { patterns: vec!["bass guitar".to_string(), "bassguitar".to_string(), "bg".to_string(), "electric bass".to_string()], required: false });
        pattern_categories.insert("Synth Bass".to_string(), PatternCategory { patterns: vec!["synth bass".to_string(), "synthbass".to_string(), "sb".to_string(), "bass synth".to_string()], required: false });
        pattern_categories.insert("DI".to_string(), PatternCategory { patterns: vec!["di".to_string(), "DI".to_string(), "direct".to_string(), "Direct".to_string()], required: false });
        pattern_categories.insert("Amp".to_string(), PatternCategory { patterns: vec!["amp".to_string(), "Amp".to_string(), "amplifier".to_string(), "Amplifier".to_string()], required: false });
        
        config.pattern_categories = Some(pattern_categories);
        config
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Bass {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_bass = props.group_prefix.as_deref() == Some("Bass")
            || props.sub_type.as_ref()
                .map(|st| {
                    st.iter().any(|s| {
                        s.eq_ignore_ascii_case("Bass")
                            || s.eq_ignore_ascii_case("Bass Guitar")
                            || s.eq_ignore_ascii_case("Synth Bass")
                            || s.eq_ignore_ascii_case("BG")
                            || s.eq_ignore_ascii_case("SB")
                    })
                })
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                        || config.pattern_categories.as_ref()
                            .map(|cats| {
                                cats.iter().any(|(_, cat)| {
                                    cat.patterns.iter()
                                        .any(|p| name_lower.contains(&p.to_lowercase()))
                                })
                            })
                            .unwrap_or(false)
                })
                .unwrap_or(false);
        
        if !is_bass {
            return Err(TemplateParseError::NotMatch("Bass".to_string()));
        }
        
        if let Some(original) = &props.original_name {
            let name_lower = original.to_string().to_lowercase();
            let matches_negative = config.negative_patterns.iter()
                .any(|p| {
                    let neg_lower = p.to_lowercase();
                    name_lower == neg_lower
                        || name_lower.starts_with(&format!("{} ", neg_lower))
                        || name_lower.ends_with(&format!(" {}", neg_lower))
                        || name_lower.contains(&format!(" {} ", neg_lower))
                });
            
            if matches_negative {
                return Err(TemplateParseError::NegativeMatch("Bass".to_string()));
            }
        }
        
        Ok(props)
    }
}
