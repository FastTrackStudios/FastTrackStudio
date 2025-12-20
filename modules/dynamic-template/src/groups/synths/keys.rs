//! Synth keys group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Keys group (for synthesizer keys sounds)
pub struct Keys;

impl From<Keys> for Group<ItemMetadata> {
    fn from(_val: Keys) -> Self {
        Group::builder("Keys")
            .patterns(vec!["polysynth", "poly"])
            .build()
    }
}
