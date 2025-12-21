//! Pitch effect group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// Pitch effect group
pub struct Pitch;

impl From<Pitch> for Group<ItemMetadata> {
    fn from(_val: Pitch) -> Self {
        Group::builder("Pitch")
            .patterns(vec![
                "pitch",
                "pitchshift",
                "pitch_shift",
                "autotune",
                "auto_tune",
                "melodyne",
                "harmony_",
                "harmonizer",
                "octave",
                "octaver",
                "detune",
                "transpose",
                "vocoder",
                "talkbox",
            ])
            // Exclude single letters to avoid conflicts with channels
            .exclude(vec!["L", "C", "R", "l", "c", "r"])
            .build()
    }
}
