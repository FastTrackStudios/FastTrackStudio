//! Kick drum group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Kick drum group
pub struct Kick;

impl From<Kick> for Group<ItemMetadata> {
    fn from(_val: Kick) -> Self {
        Group::builder("Kick")
            .patterns(vec!["kick", "kik", "bd", "bassdrum", "bass_drum"])
            .build()
    }
}
