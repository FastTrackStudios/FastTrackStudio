//! Bass-related group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

pub mod bass_guitar;
pub mod synth_bass;
pub mod upright_bass;

pub use bass_guitar::BassGuitar;
pub use synth_bass::SynthBass;
pub use upright_bass::UprightBass;

/// Top-level bass group containing all bass types
pub struct Bass;

impl From<Bass> for Group<ItemMetadata> {
    fn from(_val: Bass) -> Self {
        Group::builder("Bass")
            .prefix("Bass")
            .patterns(vec!["bass"])
            // Negative patterns to avoid matching bass drums
            .excludes(vec!["bassdrum", "bass_drum", "bd", "kick"])
            .group(BassGuitar)
            .group(SynthBass)
            .group(UprightBass)
            .build()
    }
}
