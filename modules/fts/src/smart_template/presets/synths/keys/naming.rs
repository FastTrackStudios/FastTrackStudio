use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::GroupConfig;
use super::Keys;

use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;

impl Group for Keys {
    fn group_name(&self) -> &str {
        "Keys"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Keys".to_string(),
            prefix: "KY".to_string(),
            patterns: vec!["keys".to_string(), "synth keys".to_string()],
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Keys {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_keys = props.group_prefix.as_deref() == Some("Keys")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Keys")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_keys {
            return Err(TemplateParseError::NotMatch("Keys".to_string()));
        }
        
        Ok(props)
    }
}
