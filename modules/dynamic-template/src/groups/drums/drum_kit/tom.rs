//! Tom drum group definition

use crate::item_metadata::prelude::*;

/// Tom drum group
pub struct Tom;

impl From<Tom> for ItemMetadataGroup {
    fn from(_val: Tom) -> Self {
        use monarchy::FieldValueDescriptor;
        use crate::item_metadata::ItemMetadataField;
        
        // Define tom number/position as field value descriptors
        // Each value can have its own patterns, and the value name becomes the display name
        // So "T1", "T2", "T3" will be the track names
        let tom_number_descriptors = vec![
            FieldValueDescriptor::builder("T1")
                .patterns(["1", "t1"])
                .build(),
            FieldValueDescriptor::builder("T2")
                .patterns(["2", "t2"])
                .build(),
            FieldValueDescriptor::builder("T3")
                .patterns(["3", "t3"])
                .build(),
            FieldValueDescriptor::builder("T4")
                .patterns(["4", "t4"])
                .build(),
            FieldValueDescriptor::builder("T5")
                .patterns(["5", "t5"])
                .build(),
            FieldValueDescriptor::builder("FT")
                .patterns(["ft", "floor"])
                .build(),
        ];

        // Use field_value_descriptors for MultiMic to get "T1", "T2", "T3" as display names
        ItemMetadataGroup::builder("Toms")
            .patterns(["tom", "t1", "t2", "t3", "t4", "t5", "ft", "floor"])
            .field_value_descriptors(ItemMetadataField::MultiMic, tom_number_descriptors)
            .build()
    }
}
