use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::traits::{Group, Parser,  TemplateSource};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::errors::{TemplateParseError };
use daw::tracks::{Track };
use super::ElectricKeys;

impl Group for ElectricKeys {
    fn group_name(&self) -> &str {
        "Electric Keys"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Electric Keys".to_string(),
            prefix: "EK".to_string(),
            patterns: vec![
                "rhodes".to_string(), 
                "wurli".to_string(), 
                "wurlitzer".to_string(), 
                "electric keys".to_string(), 
                "e keys".to_string(),
                "digital".to_string(),
                "dx7".to_string(),
                "jd800".to_string(),
                "jd-800".to_string(),
                "ms-80".to_string(),
                "ms80".to_string(),
            ],
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for ElectricKeys {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        Ok(parser.parse_item_properties(name))
    }
}

