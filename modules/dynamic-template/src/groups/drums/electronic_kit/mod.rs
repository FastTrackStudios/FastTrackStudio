//! Electronic drum kit group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

// Electronic kit specific drum types can be added here
// mod kick;
// mod snare;
// mod hat;
// mod pad;
// mod trigger;

// pub use kick::Kick;
// pub use snare::Snare;
// pub use hat::Hat;
// pub use pad::Pad;
// pub use trigger::Trigger;

/// Electronic drum kit container group
pub struct ElectronicKit;

impl From<ElectronicKit> for Group<ItemMetadata> {
    fn from(_val: ElectronicKit) -> Self {
        Group::builder("Electronic Drums")
            .patterns(vec![
                "electronic",
                "elec",
                "e-kit",
                "ekit",
                "808",
                "909",
                "drum machine",
                "machine",
                "sample",
                "sampled",
                "trigger",
                "midi drum",
                "vst drum",
            ])
            .block_prefix("D") // Avoid "D Electronic Drums" redundancy
            // Electronic kit specific groups can be added here
            // .group(Kick)
            // .group(Snare)
            // .group(Hat)
            // .group(Pad)
            // .group(Trigger)
            .build()
    }
}
