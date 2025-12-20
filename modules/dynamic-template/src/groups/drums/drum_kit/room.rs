//! Room/ambient drums group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Room/ambient drums group
pub struct Room;

impl From<Room> for Group<ItemMetadata> {
    fn from(_val: Room) -> Self {
        Group::builder("Room")
            .patterns(vec![
                "room", "rooms", "amb", "ambient", "ambience", "mono", "stereo", "wide", "crush",
                "crushed", "verb", "reverb",
            ])
            .build()
    }
}
