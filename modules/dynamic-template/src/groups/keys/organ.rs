//! Organ group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Organ group
pub struct Organ;

impl From<Organ> for Group<ItemMetadata> {
    fn from(_val: Organ) -> Self {
        Group::builder("Organ")
            .patterns(vec![
                "organ",
                "hammond",
                "b3",
                "leslie",
                "church_organ",
                "pipe_organ",
            ])
            .build()
    }
}
