//! Snare drum group definition

use crate::item_metadata::prelude::*;

/// Snare drum group
pub struct Snare;

impl From<Snare> for ItemMetadataGroup {
    fn from(_val: Snare) -> Self {
        // Define multi-mic positions as a Group
        let multi_mic = ItemMetadataGroup::builder("MultiMic")
            .patterns(["Top", "Bottom", "Side", "OH"])
            .build();

        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Snare")
            .patterns(["snare", "snr", "sn"])
            .multi_mic(multi_mic)
            .build()
    }
}
