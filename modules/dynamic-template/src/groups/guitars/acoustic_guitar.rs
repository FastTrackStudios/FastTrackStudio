//! Acoustic guitar group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Acoustic guitar group
pub struct AcousticGuitar;

impl From<AcousticGuitar> for Group<ItemMetadata> {
    fn from(_val: AcousticGuitar) -> Self {
        Group::builder("Acoustic Guitar")
            .prefix("AG")
            .patterns(vec!["acoustic"])
            .build()
    }
}
