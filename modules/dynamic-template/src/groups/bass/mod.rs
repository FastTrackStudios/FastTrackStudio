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
            .exclude(vec!["bassdrum", "bass_drum", "bd", "kick"])
            .group(BassGuitar)
            .group(SynthBass)
            .group(UprightBass)
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{default_config, OrganizeIntoTracks};
    use daw::tracks::item::Item;
    use daw::tracks::{assert_tracks_equal, TrackStructureBuilder};

    #[test]
    fn full_bass_integration_test() {
        // Test all bass types
        let items = vec!["Bass Guitar", "Bass Synth", "Upright Bass"];

        // Organize into tracks using monarchy sort
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        // Display the track list
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Verify we got the expected structure:
        // Bass
        // -Guitar [Bass Guitar]
        // -Synth [Bass Synth]
        // -Upright Bass [Upright Bass]
        let expected = TrackStructureBuilder::new()
            .folder("Bass")
            .track("Guitar", "Bass Guitar")
            .track("Synth", "Bass Synth")
            .track("Upright Bass", "Upright Bass")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }
}
