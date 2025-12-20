//! Kick drum group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Kick drum group
pub struct Kick;

impl Into<Group<ItemMetadata>> for Kick {
    fn into(self) -> Group<ItemMetadata> {
        Group::builder("Kick")
            .patterns(vec!["kick", "kik", "bd", "bassdrum", "bass_drum"])
            .build()
    }
}
