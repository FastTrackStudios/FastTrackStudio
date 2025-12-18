//! Room template implementation

use crate::smart_template::features::matching::matcher::MatchResult;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::create_track;
use crate::smart_template::features::naming::item_properties::ItemProperties;
use crate::smart_template::core::models::GroupMode;
use daw::tracks::TrackName;
use super::Room;

/// Generate the default Room track structure
pub fn generate_room_structure() -> Template {
    Template::builder("Room")
        .bus("Room")
            .track("Rooms").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("Rooms Far").modes(&[GroupMode::Full, GroupMode::Recording])
            .track("Rooms Mono").modes(&[GroupMode::Full, GroupMode::Recording])
        .end()
        .build()
}

/// Find best match for Room
pub fn find_best_match(room: &Room, track_name: &ItemProperties) -> Option<MatchResult> {
    let search_name = match track_name.multi_mic.as_ref().and_then(|m| m.first()) {
        Some(first) => format!("Rooms {}", first),
        None => "Rooms".to_string(),
    };
    
    crate::smart_template::features::matching::matcher::helpers::instrument_find_best_match(
        &room.template,
        track_name,
        &search_name,
    )
}

/// Find or create track for Room
pub fn find_or_create_track(room: &mut Room, track_name: &ItemProperties, base_name: Option<&str>) -> Result<(TrackName, bool), RoomMatchError> {
    if let Some(result) = find_best_match(room, track_name) {
        return Ok((result.track_name, result.use_takes));
    }
    
    let new_track_name = TrackName::from(base_name
        .map(|s| s.to_string())
        .unwrap_or_else(|| {
            match track_name.multi_mic.as_ref().and_then(|m| m.first()) {
                Some(first) => format!("Rooms {}", first),
                None => "Rooms".to_string(),
            }
        }));
    
    room.template.tracks.push(create_track(
        &new_track_name.0,
        None,
        Some("Room"),
        &[],
    ));
    
    Ok((new_track_name, false))
}

#[derive(Debug, thiserror::Error)]
pub enum RoomMatchError {
    #[error("Match error: {0}")]
    Other(String),
}
