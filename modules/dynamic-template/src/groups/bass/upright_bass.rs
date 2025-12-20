//! Upright bass group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Upright bass group
pub struct UprightBass;

impl From<UprightBass> for Group<ItemMetadata> {
    fn from(_val: UprightBass) -> Self {
        Group::builder("Upright Bass")
            .patterns(vec!["upright", "double_bass", "acoustic_bass"])
            .build()
    }
}
