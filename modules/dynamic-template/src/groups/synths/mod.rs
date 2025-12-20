//! Synthesizer group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

pub mod arp;
pub mod chord;
pub mod fx;
pub mod keys;
pub mod lead;
pub mod pad;

pub use arp::Arp;
pub use chord::Chord;
pub use fx::Fx;
pub use keys::Keys;
pub use lead::Lead;
pub use pad::Pad;

/// Top-level synths group containing all synthesizer types
pub struct Synths;

impl From<Synths> for Group<ItemMetadata> {
    fn from(_val: Synths) -> Self {
        Group::builder("Synths")
            .prefix("SY")
            .patterns(vec!["synth", "synthesizer"])
            .group(Lead)
            .group(Pad)
            .group(Arp)
            .group(Chord)
            .group(Keys)
            .group(Fx)
            .build()
    }
}
