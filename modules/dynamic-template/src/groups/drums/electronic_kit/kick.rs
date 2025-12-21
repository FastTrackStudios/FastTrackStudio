//! Electronic kick drum group definition

use crate::item_metadata::prelude::*;

/// Electronic kick drum group
pub struct Kick;

impl From<Kick> for ItemMetadataGroup {
    fn from(_val: Kick) -> Self {
        // Define multi-mic positions as a Group
        let multi_mic = ItemMetadataGroup::builder("MultiMic")
            .patterns(["808", "808-Kick" ])
            .build();


        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Kick")
            .patterns(["808"])
            .multi_mic(multi_mic)
            .build()
    }
}
