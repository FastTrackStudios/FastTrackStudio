use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::GroupConfig;
use super::Lead;

use crate::smart_template::core::traits::{Group, Parser,  TemplateSource};
use crate::smart_template::core::errors::{TemplateParseError };
use daw::tracks::{Track };

impl Group for Lead {
    fn group_name(&self) -> &str {
        "Lead"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Lead".to_string(),
            prefix: "LD".to_string(),
            patterns: vec!["lead".to_string(), "synth lead".to_string()],
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Lead {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_lead = props.group_prefix.as_deref() == Some("Lead")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Lead")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_lead {
            return Err(TemplateParseError::NotMatch("Lead".to_string()));
        }
        
        Ok(props)
    }
}
