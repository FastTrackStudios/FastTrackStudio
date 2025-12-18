use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;
use super::Keys;

impl Group for Keys {
    fn group_name(&self) -> &str {
        "Keys"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Keys".to_string(),
            prefix: "Keys".to_string(),
            patterns: vec!["keys".to_string(), "keyboard".to_string(), "piano".to_string()],
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
        Ok(parser.parse_item_properties(name))
    }
}
