use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;
use super::BGVs;

impl Group for BGVs {
    fn group_name(&self) -> &str {
        "BGVs"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "BGVs".to_string(),
            prefix: "BGV".to_string(),
            patterns: vec!["bgv".to_string(), "backing".to_string(), "bck".to_string()],
            negative_patterns: vec!["lead".to_string()],
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

impl Parser for BGVs {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_bgvs = props.group_prefix.as_deref() == Some("BGVs")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("BGV") || s.eq_ignore_ascii_case("Backing")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_bgvs {
            return Err(TemplateParseError::NotMatch("BGVs".to_string()));
        }
        
        Ok(props)
    }
}
