//! Synth bass group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Synth bass group
pub struct SynthBass;

impl From<SynthBass> for Group<ItemMetadata> {
    fn from(_val: SynthBass) -> Self {
        Group::builder("Synth")
            .patterns(vec!["synth_bass", "synthbass", "bass synth", "bass_synth", "sub_bass", "sub bass"])
            .exclude(vec!["808"]) // Exclude 808 to avoid matching electronic kick drums
            .build()
    }
}
