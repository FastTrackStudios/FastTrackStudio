//! Snare drum group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Snare drum group
pub struct Snare;

impl From<Snare> for Group<ItemMetadata> {
    fn from(_val: Snare) -> Self {
        Group::builder("Snare")
            .patterns(vec!["snare", "snr", "sn"])
            .build()
    }
}
