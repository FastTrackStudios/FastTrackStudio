//! Guitar-related group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

pub mod acoustic_guitar;
pub mod electric_guitar;

pub use acoustic_guitar::AcousticGuitar;
pub use electric_guitar::ElectricGuitar;

/// Top-level guitars group containing all guitar types
pub struct Guitars;

impl From<Guitars> for Group<ItemMetadata> {
    fn from(_val: Guitars) -> Self {
        Group::builder("Guitars")
            .prefix("GTR")
            .patterns(vec!["guitar", "gtr"])
            // Negative patterns to avoid matching bass guitars
            .excludes(vec!["bass_guitar", "bassguitar", "bg"])
            // Make transparent so Electric GTR and Acoustic GTR appear at top level
            .transparent()
            .group(ElectricGuitar)
            .group(AcousticGuitar)
            .build()
    }
}
