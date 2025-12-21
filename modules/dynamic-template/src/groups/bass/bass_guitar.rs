//! Bass guitar group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Bass guitar group
pub struct BassGuitar;

impl From<BassGuitar> for Group<ItemMetadata> {
    fn from(_val: BassGuitar) -> Self {
        Group::builder("Guitar")
            .patterns(vec!["bass_guitar", "bassguitar", "bass guitar", "electric_bass", "electric bass"])
            .build()
    }
}
