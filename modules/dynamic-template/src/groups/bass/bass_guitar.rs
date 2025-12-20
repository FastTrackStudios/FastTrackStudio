//! Bass guitar group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Bass guitar group
pub struct BassGuitar;

impl From<BassGuitar> for Group<ItemMetadata> {
    fn from(_val: BassGuitar) -> Self {
        Group::builder("Bass Guitar")
            .patterns(vec!["bass_guitar", "bassguitar", "electric_bass"])
            .build()
    }
}
