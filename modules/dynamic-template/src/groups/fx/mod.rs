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
            .priority(-1000) // Very low priority - only match when nothing else matches
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
/// 
/// FX groups have very low priority so they only match when nothing else matches
/// (e.g., metadata fields like Performer, Section, Layers, Channels take precedence).
pub fn fx_group() -> Group<ItemMetadata> {
    Group::builder("FX")
        .patterns(vec!["effect", "effects", "fx"])
        .priority(-1000) // Very low priority - only match when nothing else matches
        .group(Reverb)
        .group(Delay)
        .group(EQ)
        .group(Dynamics)
        .group(Modulation)
        .group(Distortion)
        .group(Pitch)
        .build()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{default_config, OrganizeIntoTracks};
    use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};

    #[test]
    fn standalone_fx_goes_to_top_level() {
        // Example: Standalone FX items go to top-level FX group
        // Input: FX Reverb, FX Delay, FX EQ
        // Output: FX -> Reverb [FX Reverb], Delay [FX Delay], EQ [FX EQ]
        // Note: Using "FX" prefix to ensure they match top-level FX patterns, not nested FX
        let items = vec![
            "FX Reverb",
            "FX Delay",
            "FX EQ",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Standalone FX items go to top-level FX group
        let expected = TrackStructureBuilder::new()
            .folder("FX")
                .track("Reverb", "FX Reverb")
                .track("Delay", "FX Delay")
                .track("EQ", "FX EQ")
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_nested_under_drums() {
        // Example: FX nested under Drums
        // Input: Drum Verb, Drum Delay
        // Output: Drums -> FX -> Reverb [Drum Verb], Delay [Drum Delay]
        let items = vec![
            "Drum Verb",
            "Drum Delay",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // FX nested under Drums
        let expected = TrackStructureBuilder::new()
            .folder("Drums")
                .folder("FX")
                    .track("Reverb", "Drum Verb")
                    .track("Delay", "Drum Delay")
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_nested_under_drum_kit() {
        // Example: FX nested under Drum Kit
        // Input: Drum Kit Verb, Drum Kit Delay
        // Output: Drums -> Drum Kit -> FX -> Reverb [Drum Kit Verb], Delay [Drum Kit Delay]
        let items = vec![
            "Drum Kit Verb",
            "Drum Kit Delay",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // FX nested under Drum Kit
        // Note: "Drums" is collapsed when it's the only child
        let expected = TrackStructureBuilder::new()
            .folder("Drum Kit")
                .folder("FX")
                    .track("Reverb", "Drum Kit Verb")
                    .track("Delay", "Drum Kit Delay")
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_nested_under_guitar() {
        // Example: FX nested under Guitar
        // Input: Guitar Verb, Guitar Delay
        // Output: Guitars -> Electric Guitar -> FX -> Reverb [Guitar Verb], Delay [Guitar Delay]
        // Note: "Guitars" is transparent, so "Electric Guitar" appears at top level
        let items = vec![
            "Guitar Verb",
            "Guitar Delay",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // FX nested under Electric Guitar
        // Note: "Guitars" is transparent, so "Electric Guitar" appears at top level
        let expected = TrackStructureBuilder::new()
            .folder("Electric Guitar")
                .folder("FX")
                    .track("Reverb", "Guitar Verb")
                    .track("Delay", "Guitar Delay")
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_nested_under_guitar_arrangement() {
        // Example: FX nested under Guitar arrangement
        // Input: Guitar Clean Verb, Guitar Clean Delay
        // Output: Guitars -> Electric Guitar -> Clean -> FX -> Reverb [Guitar Clean Verb], Delay [Guitar Clean Delay]
        let items = vec![
            "Guitar Clean Verb",
            "Guitar Clean Delay",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // FX nested under Clean arrangement
        // Note: "Guitars" is transparent, so "Electric Guitar" appears at top level
        let expected = TrackStructureBuilder::new()
            .folder("Electric Guitar")
                .folder("Clean")
                    .folder("FX")
                        .track("Reverb", "Guitar Clean Verb")
                        .track("Delay", "Guitar Clean Delay")
                    .end()
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_nested_at_deepest_level() {
        // Example: FX nested at deepest level (after all metadata fields)
        // Input: Guitar Clean Main DBL L Verb, Guitar Clean Main DBL L Delay
        // Output: Guitars -> Electric Guitar -> Clean -> Main -> DBL -> L -> FX -> Reverb, Delay
        let items = vec![
            "Guitar Clean Main DBL L Verb",
            "Guitar Clean Main DBL L Delay",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // FX nested at deepest level (after Arrangement -> Layers -> Channel)
        // Note: "Guitars" is transparent, so "Electric Guitar" appears at top level
        // Note: Intermediate levels may collapse
        let expected = TrackStructureBuilder::new()
            .folder("Electric Guitar")
                .folder("Clean")
                    .folder("Main")
                        .folder("DBL")
                            .folder("L")
                                .folder("FX")
                                    .track("Reverb", "Guitar Clean Main DBL L Verb")
                                    .track("Delay", "Guitar Clean Main DBL L Delay")
                                .end()
                            .end()
                        .end()
                    .end()
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_nested_under_vocals() {
        // Example: FX nested under Lead Vocals
        // Input: Vocal Chorus Verb, Vocal Chorus Delay
        // Output: Vocals -> Lead Vocals -> Chorus -> FX -> Reverb [Vocal Chorus Verb], Delay [Vocal Chorus Delay]
        let items = vec![
            "Vocal Chorus Verb",
            "Vocal Chorus Delay",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // FX nested under Lead Vocals -> Chorus
        // Note: "Vocals" is transparent, so "Lead Vocals" appears at top level
        // Note: "Cody" is collapsed when it's the only performer
        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
                .folder("Chorus")
                    .folder("FX")
                        .track("Reverb", "Vocal Chorus Verb")
                        .track("Delay", "Vocal Chorus Delay")
                    .end()
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_nested_under_bass() {
        // Example: FX nested under Bass
        // Input: Bass Verb, Bass Delay
        // Output: Bass -> FX -> Reverb [Bass Verb], Delay [Bass Delay]
        let items = vec![
            "Bass Verb",
            "Bass Delay",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // FX nested under Bass
        let expected = TrackStructureBuilder::new()
            .folder("Bass")
                .folder("FX")
                    .track("Reverb", "Bass Verb")
                    .track("Delay", "Bass Delay")
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn multiple_fx_types() {
        // Example: Multiple FX types
        // Input: Verb, Delay, EQ, Dynamics, Modulation, Distortion, Pitch
        // Output: FX -> Reverb [Verb], Delay [Delay], EQ [EQ], Dynamics [Dynamics], Modulation [Modulation], Distortion [Distortion], Pitch [Pitch]
        let items = vec![
            "Verb",
            "Delay",
            "EQ",
            "Compressor",
            "Modulation",
            "Distortion",
            "Pitch",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Multiple FX types at top level
        let expected = TrackStructureBuilder::new()
            .folder("FX")
                .track("Reverb", "Verb")
                .track("Delay", "Delay")
                .track("EQ", "EQ")
                .track("Dynamics", "Compressor")
                .track("Modulation", "Modulation")
                .track("Distortion", "Distortion")
                .track("Pitch", "Pitch")
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_does_not_match_single_letters() {
        // Example: Single letters should NOT match FX (they're channels)
        // Input: Vocal Chorus Cody L, Vocal Chorus Cody C, Vocal Chorus Cody R
        // Output: Vocals -> Chorus -> Cody -> L [Vocal Chorus Cody L], C [Vocal Chorus Cody C], R [Vocal Chorus Cody R]
        // Note: L, C, R should match as Channels, NOT as FX effects
        let items = vec![
            "Vocal Chorus Cody L",
            "Vocal Chorus Cody C",
            "Vocal Chorus Cody R",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // L, C, R should match as Channels, not FX
        // Note: "Vocals" is transparent, "Lead Vocals" is collapsed, "Cody" is collapsed when it's the only performer
        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
                .track("L", "Vocal Chorus Cody L")
                .track("C", "Vocal Chorus Cody C")
                .track("R", "Vocal Chorus Cody R")
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn fx_does_not_match_sections() {
        // Example: Sections should NOT match FX (e.g., "Chorus" is a Section, not Modulation)
        // Input: Vocal Chorus Cody, Vocal Verse Cody
        // Output: Vocals -> Chorus [Vocal Chorus Cody], Verse [Vocal Verse Cody]
        // Note: "Chorus" should match as Section, NOT as Modulation effect
        let items = vec![
            "Vocal Chorus Cody",
            "Vocal Verse Cody",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // "Chorus" should match as Section, not as Modulation
        // Note: "Vocals" is transparent, "Lead Vocals" is collapsed, "Cody" is collapsed when it's the only performer
        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
                .track("Chorus", "Vocal Chorus Cody")
                .track("Verse", "Vocal Verse Cody")
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }
}
