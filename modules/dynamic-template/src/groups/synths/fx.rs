//! Synth fx group definition

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

/// FX group (for synthesizer effects/sound design)
pub struct Fx;

impl From<Fx> for Group<ItemMetadata> {
    fn from(_val: Fx) -> Self {
        Group::builder("FX")
            .patterns(vec!["fx", "effect", "sweep", "riser", "impact"])
            .build()
    }
}
