//! Drum kit group definitions

use crate::item_metadata::ItemMetadata;
use monarchy::Group;

mod cymbals;
mod kick;
mod room;
mod snare;
mod tom;

pub use cymbals::Cymbals;
pub use kick::Kick;
pub use room::Room;
pub use snare::Snare;
pub use tom::Tom;

/// Drum kit container group that includes all drum subgroups
pub struct DrumKit;

impl Into<Group<ItemMetadata>> for DrumKit {
    fn into(self) -> Group<ItemMetadata> {
        Group::builder("Drum Kit")
            .patterns(vec!["d_", "drum", "kit", "drums"])
            .block_prefix("D") // Avoid "D Drum Kit" redundancy
            .group(Kick)
            .group(Snare)
            .group(Tom)
            .group(Cymbals)
            .group(Room)
            .build()
    }
}
