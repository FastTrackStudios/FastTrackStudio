//! Cymbals group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Cymbals group
pub struct Cymbals;

impl From<Cymbals> for Group<ItemMetadata> {
    fn from(_val: Cymbals) -> Self {
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
