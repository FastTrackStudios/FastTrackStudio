use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::GroupConfig;
use super::Piano;

use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;

impl Group for Piano {
    fn group_name(&self) -> &str {
        "Piano"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Piano".to_string(),
            prefix: "P".to_string(),
            patterns: vec!["piano".to_string(), "grand".to_string(), "upright".to_string()],
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Piano {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_piano = props.group_prefix.as_deref() == Some("Piano")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Piano")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_piano {
            return Err(TemplateParseError::NotMatch("Piano".to_string()));
        }
        
        Ok(props)
    }
}
