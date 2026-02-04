//! Horn section definitions for pop/rock/jazz contexts
//!
//! This is separate from Orchestra brass - these are for horn sections
//! in modern music production (jazz, funk, soul, rock, etc.)

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

pub mod saxophone;

pub use saxophone::Saxophone;

/// Top-level horns group for pop/rock/jazz horn sections
pub struct Horns;

impl From<Horns> for Group<ItemMetadata> {
    fn from(_val: Horns) -> Self {
        Group::builder("Horns")
            .prefix("Horn")
            .patterns(vec!["horn", "horns", "brass section"])
            .group(Saxophone)
            .build()
    }
}
