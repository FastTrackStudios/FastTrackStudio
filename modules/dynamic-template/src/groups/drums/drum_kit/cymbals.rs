//! Cymbals group definition

use crate::item_metadata::prelude::*;

/// Cymbals group
pub struct Cymbals;

impl From<Cymbals> for ItemMetadataGroup {
    fn from(_val: Cymbals) -> Self {
        // Define OH (overhead) positions as a nested group with L/R metadata
        let oh_multi_mic = ItemMetadataGroup::builder("MultiMic")
            .patterns(["L", "R", "Left", "Right"])
            .build();

        let oh_group = ItemMetadataGroup::builder("OH")
            .patterns(["oh", "overhead", "overheads"])
            .multi_mic(oh_multi_mic)
            .build();

        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Cymbals")
            .patterns([
                "cymbal",
                "cymbals",
                "oh",
                "overhead",
                "overheads",
                "hat",
                "hh",
                "hihat",
                "hi-hat",
                "ride",
                "crash",
                "china",
                "splash",
                "bell",
            ])
            .group(oh_group)
            .build()
    }
}
