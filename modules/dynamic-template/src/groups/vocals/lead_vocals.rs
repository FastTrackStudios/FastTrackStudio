//! Lead vocals group definition

use crate::item_metadata::prelude::*;
use crate::item_metadata::ItemMetadataField;

/// Lead vocals group
/// 
/// Sorting priority: Performer → Section → Layers → Channels
pub struct LeadVocals;

impl From<LeadVocals> for ItemMetadataGroup {
    fn from(_val: LeadVocals) -> Self {
        // Configure lead vocals with field priority: Performer → Section → Layers → Channels
        // The order of these calls determines the priority order
        // Layers uses "Main" as default value so items without a layer are grouped alongside items with layers
        ItemMetadataGroup::builder("Lead Vocals")
            .prefix("LV")
            .patterns(["lead", "main", "solo", "vocal"])
            .performer(ItemMetadataGroup::builder("Performer").build()) // Priority 1: Performer (uses global patterns)
            .section(ItemMetadataGroup::builder("Section").build()) // Priority 2: Section (uses global patterns)
            .layers(ItemMetadataGroup::builder("Layers").build()) // Priority 3: Layers (uses global patterns)
            .field_default_value(ItemMetadataField::Layers, "Main") // Default layer name for items without a layer
            .channel(ItemMetadataGroup::builder("Channel").patterns(["L", "C", "R", "Left", "Center", "Right"]).build()) // Priority 4: Channel (order: L, C, R)
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{default_config, OrganizeIntoTracks};
    use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};

    #[test]
    fn single_track_no_grouping_needed() {
        // Example 1: Single track - no grouping needed
        // Input: Vocal Chorus Cody DBL L
        // Output: Lead Vocals: Vocal Chorus Cody DBL L
        let items = vec!["Vocal Chorus Cody DBL L"];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Single track with no grouping - all intermediate levels are collapsed
        // Note: "Vocals" is transparent, so "Lead Vocals" appears at top level, but then collapses when it's the only child
        let expected = TrackStructureBuilder::new()
            .track("Vocals", "Vocal Chorus Cody DBL L")
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn multiple_sections_grouped() {
        // Example 2: Multiple sections - grouped under performer
        // Input: Vocal Verse Cody, Vocal Chorus Cody
        // Output: Lead Vocals -> Cody -> Verse, Chorus
        let items = vec![
            "Vocal Verse Cody",
            "Vocal Chorus Cody",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Performer first, then sections
        // Note: "Lead Vocals" is collapsed when it's the only child of "Vocals"
        // "Cody" is collapsed when it's the only performer
        // Sections are sorted by their order in the config (Verse, Chorus, Bridge, etc.)
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
        // Example 3: Multiple performers - grouped under Lead Vocals
        // Input: Vocal Chorus Cody, Vocal Chorus John
        // Output: Lead Vocals -> Cody -> Chorus, John -> Chorus
        let items = vec![
            "Vocal Chorus Cody",
            "Vocal Chorus John",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Performers are grouped first, then sections under each performer
        // Note: "Lead Vocals" is collapsed, and "Chorus" and "Main" are collapsed when they're the only child
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
        // Output: Lead Vocals -> Cody -> Chorus -> Main, DBL
        let items = vec![
            "Vocal Chorus Cody",
            "Vocal Chorus Cody DBL",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // The track without the layer keyword is assumed to be "Main" and goes on the folder track
        // Note: "Lead Vocals" is collapsed, "Cody" is collapsed when it's the only performer, and "Chorus" is collapsed when it's the only section
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
        // Output: Lead Vocals -> Cody -> Chorus -> L, C, R
        let items = vec![
            "Vocal Chorus Cody L",
            "Vocal Chorus Cody C",
            "Vocal Chorus Cody R",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Channels are organized under the section folder within each performer
        // Note: "Lead Vocals" is collapsed, "Cody" is collapsed when it's the only performer, and "Chorus" is collapsed when it's the only section
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
        // Output: Lead Vocals -> Cody -> Chorus -> Main -> L, C, R and DBL -> L, C, R
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
        
        // When both layers and channels are present, layers take priority
        // Note: "Lead Vocals" is collapsed, "Cody" is collapsed when it's the only performer, and "Chorus" is collapsed when it's the only section
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
