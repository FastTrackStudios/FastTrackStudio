//! Room/ambient drums group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Room/ambient drums group
pub struct Room;

impl Into<Group<ItemMetadata>> for Room {
    fn into(self) -> Group<ItemMetadata> {
        Group::builder("Room")
            .patterns(vec![
                "room", "rooms", "amb", "ambient", "ambience", "mono", "stereo", "wide", "crush",
                "crushed", "verb", "reverb",
            ])
            .build()
    }
}
