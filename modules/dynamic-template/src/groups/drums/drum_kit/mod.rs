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

impl From<DrumKit> for Group<ItemMetadata> {
    fn from(_val: DrumKit) -> Self {
        Group::builder("Drum Kit")
            .patterns(vec!["d_", "drum", "kit", "drums"])
            .exclude(vec![
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
            .block_prefix("D") // Avoid "D Drum Kit" redundancy
            .group(Kick)
            .group(Snare)
            .group(Tom)
            .group(Cymbals)
            .group(Room)
            .build()
    }
}

// region: --- Tests

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{OrganizeIntoTracks, default_config};
    use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};

    type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

    #[test]
    fn full_drum_kit_integration_test() -> Result<()> {
        // -- Setup & Fixtures
        let items = vec![
            "Kick In",
            "Kick Out",
            "Snare Top",
            "Snare Bottom",
            "Tom 1",
            "Tom 2",
            "Tom 3",
            "Hi Hat",
            "Ride",
            "OH L",
            "OH R",
            "Rooms L",
            "Rooms R",
        ];
        let config = default_config();

        // -- Exec
        let tracks = items.organize_into_tracks(&config, None)?;

        // -- Check
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        let expected = TrackStructureBuilder::new()
            .folder("Drums")
            .folder("Kick")
            .track("In", "Kick In")
            .track("Out", "Kick Out")
            .end()
            .folder("Snare")
            .track("Top", "Snare Top")
            .track("Bottom", "Snare Bottom")
            .end()
            .folder("Toms")
            .track("T1", "Tom 1")
            .track("T2", "Tom 2")
            .track("T3", "Tom 3")
            .end()
            .folder("Cymbals")
            .track("Hi Hat", "Hi Hat")
            .track("Ride", "Ride")
            .folder("OH")
            .track("L", "OH L")
            .track("R", "OH R")
            .end()
            .end()
            .folder("Rooms")
            .track("L", "Rooms L")
            .track("R", "Rooms R")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected)?;

        Ok(())
    }
}

// endregion: --- Tests
