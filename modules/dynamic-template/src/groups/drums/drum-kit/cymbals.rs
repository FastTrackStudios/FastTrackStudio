//! Cymbals group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Cymbals group
pub struct Cymbals;

impl Into<Group<ItemMetadata>> for Cymbals {
    fn into(self) -> Group<ItemMetadata> {
        Group::builder("Cymbals")
            .patterns(vec![
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
            .build()
    }
}
