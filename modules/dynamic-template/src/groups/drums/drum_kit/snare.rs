//! Snare drum group definition

use crate::item_metadata::prelude::*;

/// Snare drum group
pub struct Snare;

impl From<Snare> for ItemMetadataGroup {
    fn from(_val: Snare) -> Self {
        use monarchy::FieldValueDescriptor;
        use crate::item_metadata::ItemMetadataField;

        // Define multi-mic positions using field_value_descriptors to ensure Top comes before Bottom
        // Order matters - items will be sorted by the order of descriptors
        let multi_mic_descriptors = vec![
            FieldValueDescriptor::builder("Top")
                .patterns(["Top", "top"])
                .build(),
            FieldValueDescriptor::builder("Bottom")
                .patterns(["Bottom", "bottom", "Bot", "bot", "Btm", "btm"])
                .build(),
            FieldValueDescriptor::builder("Side")
                .patterns(["Side", "side"])
                .build(),
            FieldValueDescriptor::builder("OH")
                .patterns(["OH", "oh", "overhead"])
                .build(),
        ];

        // Use the convenience method - extension trait is in scope via prelude
        ItemMetadataGroup::builder("Snare")
            .patterns(["snare", "snr", "sn"])
            .field_value_descriptors(ItemMetadataField::MultiMic, multi_mic_descriptors)
            .build()
    }
}
