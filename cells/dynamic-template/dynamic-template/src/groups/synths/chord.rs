//! Synth chord group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Chord group
pub struct Chord;

impl From<Chord> for Group<ItemMetadata> {
    fn from(_val: Chord) -> Self {
        Group::builder("Chord")
            .patterns(vec!["chord", "stab"])
            .build()
    }
}
