//! Room naming implementation

use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::features::naming::item_properties_parser::ItemPropertiesParser;
use crate::smart_template::core::models::group_config::{GroupConfig, InsertMode, PatternCategory};
use std::collections::HashMap;
use super::Room;

/// Get the default Room group configuration
pub fn default_room_config() -> GroupConfig {
    let mut config = GroupConfig {
        name: "Room".to_string(),
        prefix: "Room".to_string(),
        patterns: vec!["room".to_string(), "rooms".to_string()],
        negative_patterns: vec![],
        parent_track: None,
        destination_track: None,
        insert_mode: Some(InsertMode::Increment),
        increment_start: Some(1),
        only_number_when_multiple: Some(true),
        create_if_missing: Some(true),
        ..Default::default()
    };
    
    let mut pattern_categories = HashMap::new();
    pattern_categories.insert("Short".to_string(), PatternCategory { patterns: vec!["short".to_string(), "Short".to_string(), "close".to_string(), "Close".to_string()], required: false });
    pattern_categories.insert("Far".to_string(), PatternCategory { patterns: vec!["far".to_string(), "Far".to_string(), "distant".to_string(), "Distant".to_string()], required: false });
    pattern_categories.insert("Mono".to_string(), PatternCategory { patterns: vec!["mono".to_string(), "Mono".to_string(), "mon".to_string(), "Mon".to_string()], required: false });
    
    config.pattern_categories = Some(pattern_categories);
    config
}

/// Parse a track name into Room properties
pub fn parse_room(room: &Room, name: &str) -> Result<ItemProperties, RoomParseError> {
    let parser = ItemPropertiesParser::new();
    let props = parser.parse(name);
    
    let is_room = props.group_prefix.as_deref() == Some("Room")
        || props.sub_type.as_ref()
            .map(|st| st.iter().any(|s| s.eq_ignore_ascii_case("Room") || s.eq_ignore_ascii_case("Rooms")))
            .unwrap_or(false)
        || props.original_name.as_ref()
            .map(|n| {
                let name_lower = n.to_lowercase();
                room.config.patterns.iter()
                    .any(|p| name_lower.contains(&p.to_lowercase()))
                    || room.config.pattern_categories.as_ref()
                        .map(|cats| {
                            cats.iter().any(|(_, cat)| {
                                cat.patterns.iter()
                                    .any(|p| name_lower.contains(&p.to_lowercase()))
                            })
                        })
                        .unwrap_or(false)
            })
            .unwrap_or(false);
    
    if !is_room {
        return Err(RoomParseError::NotRoomTrack);
    }
    
    Ok(props)
}

#[derive(Debug, thiserror::Error)]
pub enum RoomParseError {
    #[error("Track name does not match room patterns")]
    NotRoomTrack,
    #[error("Parse error: {0}")]
    Other(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_room_parse() {
        let room = Room::new();
        assert!(room.parse("Rooms Short").is_ok());
        assert!(room.parse("Kick").is_err());
    }
}
