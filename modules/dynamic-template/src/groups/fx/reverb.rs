//! Reverb effect group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Reverb effect group
pub struct Reverb;

impl From<Reverb> for Group<ItemMetadata> {
    fn from(_val: Reverb) -> Self {
        Group::builder("Reverb")
            .patterns(vec![
                "reverb",
                "verb",
                "room_",
                "hall_",
                "plate_",
                "spring_",
                "chamber_",
                "cathedral_",
                "ambience",
                "space_",
            ])
            .priority(-1000) // Very low priority - only match when nothing else matches
            .build()
    }
}
