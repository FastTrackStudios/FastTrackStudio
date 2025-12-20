//! Snare drum group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Snare drum group
pub struct Snare;

impl Into<Group<ItemMetadata>> for Snare {
    fn into(self) -> Group<ItemMetadata> {
        Group::builder("Snare")
            .patterns(vec!["snare", "snr", "sn"])
            .build()
    }
}
