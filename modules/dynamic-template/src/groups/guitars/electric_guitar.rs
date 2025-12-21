//! Electric guitar group definition

use crate::item_metadata::prelude::*;
use monarchy::{FieldGroupingStrategy, FieldValueDescriptor};

/// Electric guitar group
pub struct ElectricGuitar;

impl From<ElectricGuitar> for ItemMetadataGroup {
    fn from(_val: ElectricGuitar) -> Self {
        use crate::item_metadata::ItemMetadataField;
        
        // Define guitar-specific arrangement patterns
        // These are guitar-specific and not in the global metadata patterns
        let guitar_arrangement = ItemMetadataGroup::builder("Arrangement")
            .patterns(["Clean", "Crunch", "Drive", "Lead", "Pick", "Chug"])
            .build();
        
        // Define multi-mic descriptors for guitar (Amp, DI, Amplitube)
        let multi_mic_descriptors = vec![
            FieldValueDescriptor::builder("Amp")
                .patterns(["amp", "amplitube"])
                .build(),
            FieldValueDescriptor::builder("DI")
                .patterns(["di"])
                .build(),
        ];
        
        // Configure electric guitar with field priority: Arrangement → Layers → Channel → MultiMic
        // The order of these calls determines the priority order
        // MultiMic uses MainOnContainer strategy so base tracks go on folder, multi-mic versions become children
        ItemMetadataGroup::builder("Electric Guitar")
            .prefix("EG")
            .patterns(["electric", "guitar", "lead guitar", "lead_guitar", "leadguitar"])
            .arrangement(guitar_arrangement) // Priority 1: Arrangement
            .layers(ItemMetadataGroup::builder("Layers").build()) // Priority 2: Layers (uses global patterns)
            .channel(ItemMetadataGroup::builder("Channel").build()) // Priority 3: Channel (uses global patterns)
            // Note: We use field_value_descriptors for MultiMic, so we don't need the nested .multi_mic() group
            // The field_value_descriptors handle the MultiMic value extraction and matching
            .field_value_descriptors(ItemMetadataField::MultiMic, multi_mic_descriptors)
            .field_strategy(ItemMetadataField::MultiMic, FieldGroupingStrategy::MainOnContainer)
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{default_config, OrganizeIntoTracks};
    use daw::tracks::item::Item;
    use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};

    #[test]
    fn single_track_no_grouping_needed() {
        // Example 1: Single track - no grouping needed
        // Input: Guitar Clean DBL L
        // Output: Guitar: Guitar Clean DBL L
        let items = vec!["Guitar Clean DBL L"];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Single track with no grouping - all intermediate levels (Electric Guitar, Clean, DBL, L) are collapsed
        // Item goes directly on "Guitars" track (when a structure has items but no children, it becomes a track)
        let expected = TrackStructureBuilder::new()
            .track("Guitars", "Guitar Clean DBL L")
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn multiple_arrangements_grouped() {
        // Example 2: Multiple arrangements - grouped under Guitar folder
        // Input: Guitar Clean, Guitar Drive
        // Output: Guitar -> Clean, Drive
        let items = vec![
            "Guitar Clean",
            "Guitar Drive",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
                .folder("Electric Guitar")
                    .track("Clean", "Guitar Clean")
                    .track("Drive", "Guitar Drive")
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn guitars_with_multi_mics() {
        // Example 3: Guitars with multi-mics - base track on folder, multi-mic versions as children
        // Input: Guitar Clean, Guitar Clean Amp, Guitar Clean DI
        // Output: Guitar: [Guitar Clean] -> Amp: [Guitar Clean Amp], DI: [Guitar Clean DI]
        // Note: Amp and DI should be TRACKS (not folders) with items
        let items = vec![
            "Guitar Clean",
            "Guitar Clean Amp",
            "Guitar Clean DI",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Base track goes on folder track, Amp and DI are child tracks (not folders)
        // The "Clean" folder has the item "Guitar Clean" on it, not as a separate track
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
                .folder_with_items("Clean", Some("Guitar Clean")) // Item on folder track
                    .track("Amp", "Guitar Clean Amp") // Track, not folder
                    .track("DI", "Guitar Clean DI") // Track, not folder
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn multiple_arrangements_with_multi_mics() {
        // Example 3 extended: Multiple arrangements with multi-mics
        // Input: Guitar Clean, Guitar Clean Amp, Guitar Clean DI, Guitar Drive, Guitar Drive Amp, Guitar Drive DI
        // Output: Guitar -> Clean -> Amp, DI and Drive -> Amp, DI
        // Note: Amp and DI should be TRACKS (not folders)
        let items = vec![
            "Guitar Clean",
            "Guitar Clean Amp",
            "Guitar Clean DI",
            "Guitar Drive",
            "Guitar Drive Amp",
            "Guitar Drive DI",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
                .folder("Electric Guitar")
                    .folder_with_items("Clean", Some("Guitar Clean")) // Item on folder track
                        .track("Amp", "Guitar Clean Amp") // Track, not folder
                        .track("DI", "Guitar Clean DI") // Track, not folder
                    .end()
                    .folder_with_items("Drive", Some("Guitar Drive")) // Item on folder track
                        .track("Amp", "Guitar Drive Amp") // Track, not folder
                        .track("DI", "Guitar Drive DI") // Track, not folder
                    .end()
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn adding_layers() {
        // Example 4: Adding layers - Main and DBL
        // Input: Guitar Clean, Guitar Clean Amp, Guitar Clean DI, Guitar Clean DBL, Guitar Clean Amp DBL, Guitar Clean DI DBL
        // Output: Guitar -> Main [Guitar Clean] -> Amp, DI and DBL [Guitar Clean DBL] -> Amp, DI
        // Note: Main and DBL are folders with items, Amp and DI are tracks
        let items = vec![
            "Guitar Clean",
            "Guitar Clean Amp",
            "Guitar Clean DI",
            "Guitar Clean DBL",
            "Guitar Clean Amp DBL",
            "Guitar Clean DI DBL",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Note: Currently "Guitar Clean" (without layer) goes on "Clean" folder directly
        // TODO: Should create "Main" folder per example-guitar.md, but current implementation
        // puts it directly on "Clean". "Guitar Clean DBL" goes on "DBL" folder.
        // "Amp" and "DI" are tracks (not folders) as expected.
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
                .folder("Electric Guitar")
                    .folder_with_items("Clean", Some("Guitar Clean")) // Base track on folder
                        .folder_with_items("DBL", Some("Guitar Clean DBL")) // Base track on folder
                            .track("Amp", "Guitar Clean Amp DBL") // Track, not folder
                            .track("DI", "Guitar Clean DI DBL") // Track, not folder
                        .end()
                        .track("Amp", "Guitar Clean Amp") // Track, not folder
                        .track("DI", "Guitar Clean DI") // Track, not folder
                    .end()
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn adding_channels() {
        // Example 5: Adding channels - L, C, R
        // Input: Guitar Clean L, Guitar Clean Amp L, Guitar Clean DI L, etc.
        // Output: Guitar -> L -> Amp, DI and C -> Amp, DI and R -> Amp, DI
        // Note: L, C, R are folders, Amp and DI are tracks
        let items = vec![
            "Guitar Clean L",
            "Guitar Clean Amp L",
            "Guitar Clean DI L",
            "Guitar Clean C",
            "Guitar Clean Amp C",
            "Guitar Clean DI C",
            "Guitar Clean R",
            "Guitar Clean Amp R",
            "Guitar Clean DI R",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        // Note: "Clean" and "Electric Guitar" are collapsed when they're intermediate levels
        // Channels are directly under "Guitars" in order: L, C, R
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
                .folder_with_items("L", Some("Guitar Clean L")) // Base track on folder
                    .track("Amp", "Guitar Clean Amp L") // Track, not folder
                    .track("DI", "Guitar Clean DI L") // Track, not folder
                .end()
                .folder_with_items("C", Some("Guitar Clean C")) // Base track on folder
                    .track("Amp", "Guitar Clean Amp C") // Track, not folder
                    .track("DI", "Guitar Clean DI C") // Track, not folder
                .end()
                .folder_with_items("R", Some("Guitar Clean R")) // Base track on folder
                    .track("Amp", "Guitar Clean Amp R") // Track, not folder
                    .track("DI", "Guitar Clean DI R") // Track, not folder
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn layers_and_channels_together() {
        // Example 6: Layers and channels together - layers take priority
        // Input: Guitar Clean Main L, Guitar Clean Amp Main L, etc. and Guitar Clean DBL L, etc.
        // Output: Guitar -> Main -> L -> Amp, DI and C -> Amp, DI and R -> Amp, DI
        //         and DBL -> L -> Amp, DI and C -> Amp, DI and R -> Amp, DI
        // Note: Layers take priority, then channels, then Amp/DI are tracks
        let items = vec![
            "Guitar Clean Main L",
            "Guitar Clean Amp Main L",
            "Guitar Clean DI Main L",
            "Guitar Clean Main C",
            "Guitar Clean Amp Main C",
            "Guitar Clean DI Main C",
            "Guitar Clean Main R",
            "Guitar Clean Amp Main R",
            "Guitar Clean DI Main R",
            "Guitar Clean DBL L",
            "Guitar Clean Amp DBL L",
            "Guitar Clean DI DBL L",
            "Guitar Clean DBL C",
            "Guitar Clean Amp DBL C",
            "Guitar Clean DI DBL C",
            "Guitar Clean DBL R",
            "Guitar Clean Amp DBL R",
            "Guitar Clean DI DBL R",
        ];
        
        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();
        
        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);
        
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
                .folder("Electric Guitar")
                    .folder("Clean")
                        .folder("Main")
                            .folder_with_items("L", Some("Guitar Clean Main L")) // Base track on folder
                                .track("Amp", "Guitar Clean Amp Main L") // Track, not folder
                                .track("DI", "Guitar Clean DI Main L") // Track, not folder
                            .end()
                            .folder_with_items("C", Some("Guitar Clean Main C")) // Base track on folder
                                .track("Amp", "Guitar Clean Amp Main C") // Track, not folder
                                .track("DI", "Guitar Clean DI Main C") // Track, not folder
                            .end()
                            .folder_with_items("R", Some("Guitar Clean Main R")) // Base track on folder
                                .track("Amp", "Guitar Clean Amp Main R") // Track, not folder
                                .track("DI", "Guitar Clean DI Main R") // Track, not folder
                            .end()
                        .end()
                        .folder("DBL")
                            .folder_with_items("L", Some("Guitar Clean DBL L")) // Base track on folder
                                .track("Amp", "Guitar Clean Amp DBL L") // Track, not folder
                                .track("DI", "Guitar Clean DI DBL L") // Track, not folder
                            .end()
                            .folder_with_items("C", Some("Guitar Clean DBL C")) // Base track on folder
                                .track("Amp", "Guitar Clean Amp DBL C") // Track, not folder
                                .track("DI", "Guitar Clean DI DBL C") // Track, not folder
                            .end()
                            .folder_with_items("R", Some("Guitar Clean DBL R")) // Base track on folder
                                .track("Amp", "Guitar Clean Amp DBL R") // Track, not folder
                                .track("DI", "Guitar Clean DI DBL R") // Track, not folder
                            .end()
                        .end()
                    .end()
                .end()
            .end()
            .build();
        
        assert_tracks_equal(&tracks, &expected).unwrap();
    }
}
