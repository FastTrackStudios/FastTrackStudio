//! Synth pad group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Pad synth group
pub struct Pad;

impl From<Pad> for Group<ItemMetadata> {
    fn from(_val: Pad) -> Self {
        Group::builder("Pad")
            .patterns(vec!["pad", "ambient"])
            .build()
    }
}
