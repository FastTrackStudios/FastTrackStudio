//! Tom drum group definition

use crate::item_metadata::prelude::*;

/// Tom drum group
pub struct Tom;

impl From<Tom> for ItemMetadataGroup {
    fn from(_val: Tom) -> Self {
        // Define tom number/position as a metadata field using MultiMic
        // This will create nested structure: Toms -> 1, 2, 3, etc.
        // The display names will be "1", "2", "3" (we can format to "T1", "T2", "T3" later if needed)
        let tom_number = ItemMetadataGroup::builder("MultiMic")
            .patterns(["1", "2", "3", "4", "5", "t1", "t2", "t3", "t4", "t5", "ft", "floor"])
            .build();

        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Toms")
            .patterns(["tom", "t1", "t2", "t3", "t4", "t5", "ft", "floor"])
            .multi_mic(tom_number)
            .build()
    }
}
