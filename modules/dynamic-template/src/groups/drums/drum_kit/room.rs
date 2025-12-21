//! Room/ambient drums group definition

use crate::item_metadata::prelude::*;

/// Room/ambient drums group
pub struct Room;

impl From<Room> for ItemMetadataGroup {
    fn from(_val: Room) -> Self {
        // Define room positions as a metadata field (L, R, etc.)
        let room_position = ItemMetadataGroup::builder("MultiMic")
            .patterns(["L", "R", "Left", "Right", "Mono", "Stereo"])
            .build();

        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Rooms")
            .patterns([
                "room", "rooms", "amb", "ambient", "ambience", "mono", "stereo", "wide", "crush",
                "crushed", "verb", "reverb",
            ])
            .multi_mic(room_position)
            .build()
    }
}
