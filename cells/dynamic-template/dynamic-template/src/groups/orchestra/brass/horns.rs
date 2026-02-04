//! Horns group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Horns group
pub struct Horns;

impl From<Horns> for Group<ItemMetadata> {
    fn from(_val: Horns) -> Self {
        Group::builder("Horns")
            .patterns(vec!["horn", "horns", "french_horn", "frenchhorn"])
            .build()
    }
}
