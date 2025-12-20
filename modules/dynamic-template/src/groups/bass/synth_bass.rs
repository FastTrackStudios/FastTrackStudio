//! Synth bass group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Synth bass group
pub struct SynthBass;

impl From<SynthBass> for Group<ItemMetadata> {
    fn from(_val: SynthBass) -> Self {
        Group::builder("Synth Bass")
            .patterns(vec!["synth_bass", "synthbass", "808", "sub_bass"])
            .build()
    }
}
