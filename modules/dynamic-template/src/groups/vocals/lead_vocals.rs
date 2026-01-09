//! Lead vocals group definition

use crate::item_metadata::ItemMetadataField;
use crate::item_metadata::prelude::*;

/// Lead vocals group
///
/// Sorting priority: Performer → Section → Layers → Channels
pub struct LeadVocals;

impl From<LeadVocals> for ItemMetadataGroup {
    fn from(_val: LeadVocals) -> Self {
        // Configure lead vocals with field priority: Performer → Section → Layers → Channels
        // The order of these calls determines the priority order
        // Layers uses "Main" as default value so items without a layer are grouped alongside items with layers
        // Note: No prefix for Lead Vocals - parent Vocals has "V" prefix which is sufficient
        ItemMetadataGroup::builder("Lead")
            // Patterns for lead vocals - includes generic vocal patterns since Lead is the
            // default destination for vocal tracks that don't match BGVs patterns
            .patterns([
                "lead", "main", "solo", "ld", "ldv", "vox", "vocal", "voca", "voice",
            ])
            // Only match if parent (Vocals) also matches - prevents "JohnyLead" from matching
            // just because it contains "Lead" without any vocal-related patterns
            .requires_parent_match()
            .performer(ItemMetadataGroup::builder("Performer").build()) // Priority 1: Performer (uses global patterns)
            .section(ItemMetadataGroup::builder("Section").build()) // Priority 2: Section (uses global patterns)
            // Layers includes quad (quadruple tracking), stereo, mono, etc.
            .layers(
                ItemMetadataGroup::builder("Layers")
                    .patterns(["quad", "stereo", "mono", "double", "triple"])
                    .build(),
            ) // Priority 3: Layers
            .field_default_value(ItemMetadataField::Layers, "Main") // Default layer name for items without a layer
            .channel(
                ItemMetadataGroup::builder("Channel")
                    .patterns(["L", "C", "R", "Left", "Center", "Right"])
                    .build(),
            ) // Priority 4: Channel (order: L, C, R)
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{OrganizeIntoTracks, default_config};
    use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};

    #[test]
    fn single_track_no_grouping_needed() {
        // Example 1: Single track - no grouping needed
        // Input: Vocal Chorus Cody DBL L
        // Output: Lead Vocals: Vocal Chorus Cody DBL L
        // Vocals is transparent, so Lead Vocals becomes the top-level name
        let items = vec!["Vocal Chorus Cody DBL L"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Single track - all intermediate levels are collapsed
        // Vocals is NOT transparent, so Vocals name is kept
        let expected = TrackStructureBuilder::new()
            .track("Vocals", "Vocal Chorus Cody DBL L")
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn multiple_sections_grouped() {
        // Example 2: Multiple sections - grouped under performer
        // Input: Vocal Verse Cody, Vocal Chorus Cody
        // Output: Vocals -> Verse, Chorus
        let items = vec!["Vocal Verse Cody", "Vocal Chorus Cody"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Performer (Cody) is collapsed when it's the only one
        // Lead shares "vocal" pattern with Vocals, so collapses into Vocals
        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Chorus", "Vocal Chorus Cody")
            .track("Verse", "Vocal Verse Cody")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn multiple_performers_grouped() {
        // Example 3: Multiple performers - grouped under Vocals
        // Input: Vocal Chorus Cody, Vocal Chorus John
        // Output: Vocals -> Cody, John
        let items = vec!["Vocal Chorus Cody", "Vocal Chorus John"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Performers are grouped, Chorus and Main are collapsed
        // Lead shares "vocal" pattern with Vocals, so collapses into Vocals
        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Cody", "Vocal Chorus Cody")
            .track("John", "Vocal Chorus John")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn adding_layers() {
        // Example 4: Adding layers - Main and DBL
        // Input: Vocal Chorus Cody, Vocal Chorus Cody DBL
        // Output: Vocals -> Main, DBL
        let items = vec!["Vocal Chorus Cody", "Vocal Chorus Cody DBL"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Cody and Chorus are collapsed
        // Lead shares "vocal" pattern with Vocals, so collapses into Vocals
        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .track("Main", "Vocal Chorus Cody")
            .track("DBL", "Vocal Chorus Cody DBL")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn adding_channels() {
        // Example 5: Adding channels - L, C, R
        // Input: Vocal Chorus Cody L, Vocal Chorus Cody C, Vocal Chorus Cody R
        // Output: Vocals -> L, C, R
        let items = vec![
            "Vocal Chorus Cody L",
            "Vocal Chorus Cody C",
            "Vocal Chorus Cody R",
        ];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Cody, Chorus, and Main are collapsed
        // Lead shares "vocal" pattern with Vocals, so collapses into Vocals
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
    fn layers_and_channels_together() {
        // Example 6: Layers and channels together - layers take priority
        // Input: Vocal Chorus Cody Main L, Vocal Chorus Cody Main C, Vocal Chorus Cody Main R,
        //        Vocal Chorus Cody DBL L, Vocal Chorus Cody DBL C, Vocal Chorus Cody DBL R
        // Output: Vocals -> Main -> L, C, R and DBL -> L, C, R
        let items = vec![
            "Vocal Chorus Cody Main L",
            "Vocal Chorus Cody Main C",
            "Vocal Chorus Cody Main R",
            "Vocal Chorus Cody DBL L",
            "Vocal Chorus Cody DBL C",
            "Vocal Chorus Cody DBL R",
        ];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Cody and Chorus are collapsed
        // Lead shares "vocal" pattern with Vocals, so collapses into Vocals
        let expected = TrackStructureBuilder::new()
            .folder("Vocals")
            .folder("Main")
            .track("L", "Vocal Chorus Cody Main L")
            .track("C", "Vocal Chorus Cody Main C")
            .track("R", "Vocal Chorus Cody Main R")
            .end()
            .folder("DBL")
            .track("L", "Vocal Chorus Cody DBL L")
            .track("C", "Vocal Chorus Cody DBL C")
            .track("R", "Vocal Chorus Cody DBL R")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }
}
