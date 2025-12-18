use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;
use super::Vocals;

impl Group for Vocals {
    fn group_name(&self) -> &str {
        "Vocals"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Vocals".to_string(),
            prefix: "V".to_string(),
            patterns: vec!["vocal".to_string(), "vox".to_string(), "lead".to_string()],
            negative_patterns: vec!["bgv".to_string(), "backing".to_string()],
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

impl Parser for Vocals {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_vocals = props.group_prefix.as_deref() == Some("V")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Vocal") || s.eq_ignore_ascii_case("Vox")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_vocals {
            return Err(TemplateParseError::NotMatch("Vocals".to_string()));
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
                return Err(TemplateParseError::NegativeMatch("Vocals".to_string()));
            }
        }
        
        Ok(props)
    }
}
