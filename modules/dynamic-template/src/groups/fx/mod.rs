//! FX (effects) group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

pub mod delay;
pub mod distortion;
pub mod dynamics;
pub mod eq;
pub mod modulation;
pub mod pitch;
pub mod reverb;

pub use delay::Delay;
pub use distortion::Distortion;
pub use dynamics::Dynamics;
pub use eq::EQ;
pub use modulation::Modulation;
pub use pitch::Pitch;
pub use reverb::Reverb;

/// Top-level FX group containing all effect types
/// 
/// This group can also be nested within other groups to allow effects
/// to be attached to specific tracks (e.g., "Drum Verb", "Guitar Clean Verb")
pub struct FX;

impl From<FX> for Group<ItemMetadata> {
    fn from(_val: FX) -> Self {
        Group::builder("FX")
            .prefix("FX")
            .patterns(vec!["effect", "effects", "fx"])
            .group(Reverb)
            .group(Delay)
            .group(EQ)
            .group(Dynamics)
            .group(Modulation)
            .group(Distortion)
            .group(Pitch)
            .build()
    }
}

/// Creates an FX group that can be nested within other groups
/// 
/// This allows effects to be attached to specific tracks.
/// For example, "Drum Verb" will create a "Verb" child under "Drums".
pub fn fx_group() -> Group<ItemMetadata> {
    Group::builder("FX")
        .patterns(vec!["effect", "effects", "fx"])
        .group(Reverb)
        .group(Delay)
        .group(EQ)
        .group(Dynamics)
        .group(Modulation)
        .group(Distortion)
        .group(Pitch)
        .build()
}
