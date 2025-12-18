use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode};
use super::Room;

use crate::smart_template::core::traits::{Group, Parser, TemplateSource};
use crate::smart_template::core::errors::TemplateParseError;
use daw::tracks::Track;

impl Group for Room {
    fn group_name(&self) -> &str {
        "Rooms"
    }

    fn group_config(&self) -> GroupConfig {
        GroupConfig {
            name: "Rooms".to_string(),
            prefix: "Room".to_string(),
            patterns: vec!["room".to_string(), "amb".to_string(), "ambient".to_string()],
            negative_patterns: vec![],
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

impl Parser for Room {
    type Output = ItemProperties;
    type Error = TemplateParseError;

    fn parse_item_properties(&self, name: &str) -> Result<Self::Output, Self::Error> {
        let parser = ItemPropertiesParser::new();
        let props = parser.parse_item_properties(name);
        
        let config = self.group_config();
        let is_room = props.group_prefix.as_deref() == Some("Room")
            || props.group_prefix.as_deref() == Some("Rooms")
            || props.sub_type.as_ref()
                .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Room") || s.eq_ignore_ascii_case("Rooms")))
                .unwrap_or(false)
            || props.original_name.as_ref()
                .map(|n| {
                    let name_lower = n.to_lowercase();
                    config.patterns.iter()
                        .any(|p| name_lower.contains(&p.to_lowercase()))
                })
                .unwrap_or(false);
        
        if !is_room {
            return Err(TemplateParseError::NotMatch("Room".to_string()));
        }
        
        Ok(props)
    }
}
