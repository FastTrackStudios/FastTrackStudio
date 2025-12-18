use crate::smart_template::core::models::group_config::GroupConfig;
use crate::smart_template::core::traits::{Group, Parser, Matcher, TemplateSource};
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::errors::{TemplateParseError, TemplateMatchError};
use daw::tracks::{Track, TrackName};
use super::Organ;

impl Group for Organ {
    fn group_name(&self) -> &str {
        "Organ"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Organ".to_string(),
            prefix: "ORG".to_string(),
            patterns: vec![
                "organ".to_string(), 
                "hammond".to_string(), 
                "b3".to_string(), 
                "vox continental".to_string(), 
                "farfisa".to_string(),
                "pipe".to_string(),
                "transistor".to_string(),
            ],
            ..Default::default()
        }
    }

    fn default_tracklist(&self) -> Vec<Track> {
        self.template().tracks
    }
}

impl Parser for Organ {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        Ok(parser.parse_item_properties(name))
    }
}


