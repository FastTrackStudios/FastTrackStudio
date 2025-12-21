//! Room/ambient drums group definition

use crate::item_metadata::prelude::*;

/// Room/ambient drums group
pub struct Room;

impl From<Room> for ItemMetadataGroup {
    fn from(_val: Room) -> Self {
        use monarchy::FieldValueDescriptor;
        use crate::item_metadata::ItemMetadataField;

        // Define room positions using field_value_descriptors to ensure L comes before R
        // Order matters - items will be sorted by the order of descriptors
        let room_position_descriptors = vec![
            FieldValueDescriptor::builder("L")
                .patterns(["L", "Left", "left"])
                .build(),
            FieldValueDescriptor::builder("R")
                .patterns(["R", "Right", "right"])
                .build(),
            FieldValueDescriptor::builder("Mono")
                .patterns(["Mono", "mono"])
                .build(),
            FieldValueDescriptor::builder("Stereo")
                .patterns(["Stereo", "stereo"])
                .build(),
        ];

        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Rooms")
            .patterns([
                "room", "rooms", "amb", "ambient", "ambience", "mono", "stereo", "wide", "crush",
                "crushed", "verb", "reverb",
            ])
            .field_value_descriptors(ItemMetadataField::MultiMic, room_position_descriptors)
            .build()
    }
}
