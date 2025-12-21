//! Dynamics effect group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Dynamics effect group
pub struct Dynamics;

impl From<Dynamics> for Group<ItemMetadata> {
    fn from(_val: Dynamics) -> Self {
        Group::builder("Dynamics")
            .patterns(vec![
                "compressor",
                "comp_",
                "limiter",
                "gate",
                "expander",
                "de-esser",
                "deesser",
                "transient",
                "maximizer",
                "leveler",
                "multiband",
                "opto",
                "vca_",
                "fet_",
                "tube_",
            ])
            .priority(-1000) // Very low priority - only match when nothing else matches
            .build()
    }
}
