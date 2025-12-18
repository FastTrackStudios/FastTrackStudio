use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;
use super::Synths;

impl Group for Synths {
    fn group_name(&self) -> &str {
        "Synths"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Synths".to_string(),
            prefix: "SYN".to_string(),
            patterns: vec!["synth".to_string(), "pad".to_string(), "lead".to_string(), "arp".to_string()],
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Synths {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        Ok(parser.parse_item_properties(name))
    }
}
