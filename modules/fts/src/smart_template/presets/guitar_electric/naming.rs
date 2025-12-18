use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use super::GuitarElectric;

use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;

impl Group for GuitarElectric {
    fn group_name(&self) -> &str {
        "Guitar Electric"
    }

    fn group_config(&self) -> GroupConfig {
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

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for GuitarElectric {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_guitar = props.group_prefix.as_deref() == Some("GTR")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Guitar") || s.eq_ignore_ascii_case("EG")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_guitar {
            return Err(TemplateParseError::NotMatch("Guitar Electric".to_string()));
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
                return Err(TemplateParseError::NegativeMatch("Guitar Electric".to_string()));
            }
        }
        
        Ok(props)
    }
}
