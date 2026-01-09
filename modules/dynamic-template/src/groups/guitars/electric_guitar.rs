//! Electric guitar group definition

use crate::item_metadata::prelude::*;
use monarchy::{FieldGroupingStrategy, FieldValueDescriptor};

/// Electric guitar group
pub struct ElectricGuitar;

impl From<ElectricGuitar> for ItemMetadataGroup {
    fn from(_val: ElectricGuitar) -> Self {
        use crate::item_metadata::ItemMetadataField;

        // Define guitar-specific arrangement patterns (playing styles / tones)
        let guitar_arrangement = ItemMetadataGroup::builder("Arrangement")
            .patterns([
                "Clean",
                "Crunch",
                "Drive",
                "Lead",
                "Pick",
                "Chug",
                "Rhythm",
                "Solo",
                "Phaser",
                "Pitch",
                "Wah",
                "Distortion",
                "Overdrive",
            ])
            .build();

        // Define guitar model/variant descriptors
        let variant_descriptors = vec![
            FieldValueDescriptor::builder("Strat")
                .patterns(["strat", "stratocaster"])
                .build(),
            FieldValueDescriptor::builder("Tele")
                .patterns(["tele", "telecaster"])
                .build(),
            FieldValueDescriptor::builder("Les Paul")
                .patterns(["les paul", "lespaul", "lp"])
                .build(),
            FieldValueDescriptor::builder("SG").patterns(["sg"]).build(),
            FieldValueDescriptor::builder("Gretsch")
                .patterns(["gretsch"])
                .build(),
            FieldValueDescriptor::builder("Jazzmaster")
                .patterns(["jazzmaster", "jm"])
                .build(),
            FieldValueDescriptor::builder("Jaguar")
                .patterns(["jaguar", "jag"])
                .build(),
            FieldValueDescriptor::builder("335")
                .patterns(["335", "es-335", "es335"])
                .build(),
            FieldValueDescriptor::builder("Rickenbacker")
                .patterns(["rickenbacker", "ric", "ricky"])
                .build(),
            FieldValueDescriptor::builder("PRS")
                .patterns(["prs", "paul reed smith"])
                .build(),
        ];

        // Define multi-mic descriptors for guitar (Amp, DI, Amplitube)
        let multi_mic_descriptors = vec![
            FieldValueDescriptor::builder("Amp")
                .patterns(["amp", "amplitube"])
                .build(),
            FieldValueDescriptor::builder("DI").patterns(["di"]).build(),
        ];

        // Configure electric guitar with field priority: Performer → Arrangement → Layers → Channel → MultiMic
        // The order of these calls determines the priority order
        // MultiMic uses MainOnContainer strategy so base tracks go on folder, multi-mic versions become children
        // Layers uses "Main" as default value so items without a layer are grouped alongside items with layers
        ItemMetadataGroup::builder("Electric")
            .prefix("E")
            .patterns([
                "electric",
                "guitar",
                "lead guitar",
                "lead_guitar",
                "leadguitar",
                // Common electric guitar models (also matched as variants)
                "strat",
                "stratocaster",
                "tele",
                "telecaster",
                "les paul",
                "lespaul",
                "sg",
                "gretsch",
                "jazzmaster",
                "jaguar",
                "firebird",
                "explorer",
                "flying v",
            ])
            .performer(ItemMetadataGroup::builder("Performer").build()) // Priority 1: Performer (uses global patterns)
            .field_value_descriptors(ItemMetadataField::Variant, variant_descriptors) // Priority 2: Variant/model
            .arrangement(guitar_arrangement) // Priority 3: Arrangement
            .layers(ItemMetadataGroup::builder("Layers").build()) // Priority 4: Layers (uses global patterns)
            .field_default_value(ItemMetadataField::Layers, "Main") // Default layer name for items without a layer
            .channel(
                ItemMetadataGroup::builder("Channel")
                    .patterns(["L", "C", "R", "Left", "Center", "Right"])
                    .build(),
            ) // Priority 5: Channel (order: L, C, R)
            // Note: We use field_value_descriptors for MultiMic, so we don't need the nested .multi_mic() group
            // The field_value_descriptors handle the MultiMic value extraction and matching
            .field_value_descriptors(ItemMetadataField::MultiMic, multi_mic_descriptors)
            .field_strategy(
                ItemMetadataField::MultiMic,
                FieldGroupingStrategy::MainOnContainer,
            )
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{OrganizeIntoTracks, default_config};
    use daw::tracks::item::Item;
    use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};

    #[test]
    fn single_track_no_grouping_needed() {
        // Example 1: Single track - no grouping needed
        // Input: Guitar Clean DBL L
        // Output: Guitars: Guitar Clean DBL L
        // Guitars has patterns, so the top-level name is preserved
        let items = vec!["Guitar Clean DBL L"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Single track with no grouping - all intermediate levels are collapsed
        // Guitars (with patterns) is kept as the name, Electric is collapsed into it
        let expected = TrackStructureBuilder::new()
            .track("Guitars", "Guitar Clean DBL L")
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn multiple_arrangements_grouped() {
        // Example 2: Multiple arrangements - grouped under Guitars folder
        // Input: Guitar Clean, Guitar Drive
        // Output: Guitars -> Clean, Drive
        // Guitars (with patterns) is preserved, Electric is collapsed into it
        let items = vec!["Guitar Clean", "Guitar Drive"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Note: Order is Clean then Drive (matches config order: ["Clean", "Crunch", "Drive", ...])
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .track("Clean", "Guitar Clean")
            .track("Drive", "Guitar Drive")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn guitars_with_multi_mics() {
        // Example 3: Guitars with multi-mics - base track on folder, multi-mic versions as children
        // Input: Guitar Clean, Guitar Clean Amp, Guitar Clean DI
        // Output: Electric: [Guitar Clean] -> Amp, DI
        // Single arrangement (Clean) collapses to Electric, base item on folder, multi-mics as children
        let items = vec!["Guitar Clean", "Guitar Clean Amp", "Guitar Clean DI"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Guitars (with patterns) is preserved, Electric and Clean are collapsed
        // All items expand to children (Amp, DI, Electric Clean)
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .track("Amp", "Guitar Clean Amp")
            .track("DI", "Guitar Clean DI")
            .track("Electric Clean", "Guitar Clean")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn multiple_arrangements_with_multi_mics() {
        // Example 3 extended: Multiple arrangements with multi-mics
        // Input: Guitar Clean, Guitar Clean Amp, Guitar Clean DI, Guitar Drive, Guitar Drive Amp, Guitar Drive DI
        // Output: Electric -> Clean -> Amp, DI, Electric Clean and Drive -> Amp, DI, Electric Drive
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

        // Guitars (with patterns) is preserved, Electric is collapsed
        // Each arrangement has items expanded to children
        // "Clean" and "Drive" are stripped since they're parent folder names
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Clean")
            .track("Amp", "Guitar Clean Amp")
            .track("DI", "Guitar Clean DI")
            .track("Electric", "Guitar Clean")
            .end()
            .folder("Drive")
            .track("Amp", "Guitar Drive Amp")
            .track("DI", "Guitar Drive DI")
            .track("Electric", "Guitar Drive")
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

        // Guitars (with patterns) is preserved, Electric and Clean are collapsed
        // Main and DBL are layers - items expand to individual children
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Main")
            .track("Amp", "Guitar Clean Amp")
            .track("DI", "Guitar Clean DI")
            .track("Electric Clean", "Guitar Clean")
            .end()
            .folder("DBL")
            .track("Amp", "Guitar Clean Amp DBL")
            .track("DI", "Guitar Clean DI DBL")
            .track("Electric Clean DBL", "Guitar Clean DBL")
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

        // Guitars (with patterns) is preserved, Electric, Clean, Main are collapsed
        // Channels (L, C, R) are directly under Guitars, items expand to children
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("L")
            .track("Amp", "Guitar Clean Amp L")
            .track("DI", "Guitar Clean DI L")
            .track("Electric Clean L", "Guitar Clean L")
            .end()
            .folder("C")
            .track("Amp", "Guitar Clean Amp C")
            .track("DI", "Guitar Clean DI C")
            .track("Electric Clean C", "Guitar Clean C")
            .end()
            .folder("R")
            .track("Amp", "Guitar Clean Amp R")
            .track("DI", "Guitar Clean DI R")
            .track("Electric Clean R", "Guitar Clean R")
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

        // Guitars (with patterns) is preserved, Electric and Clean are collapsed
        // Main and DBL are layers, each with channels L, C, R, items expand to children
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Main")
            .folder("L")
            .track("Amp", "Guitar Clean Amp Main L")
            .track("DI", "Guitar Clean DI Main L")
            .track("Electric Clean Main L", "Guitar Clean Main L")
            .end()
            .folder("C")
            .track("Amp", "Guitar Clean Amp Main C")
            .track("DI", "Guitar Clean DI Main C")
            .track("Electric Clean Main C", "Guitar Clean Main C")
            .end()
            .folder("R")
            .track("Amp", "Guitar Clean Amp Main R")
            .track("DI", "Guitar Clean DI Main R")
            .track("Electric Clean Main R", "Guitar Clean Main R")
            .end()
            .end()
            .folder("DBL")
            .folder("L")
            .track("Amp", "Guitar Clean Amp DBL L")
            .track("DI", "Guitar Clean DI DBL L")
            .track("Electric Clean DBL L", "Guitar Clean DBL L")
            .end()
            .folder("C")
            .track("Amp", "Guitar Clean Amp DBL C")
            .track("DI", "Guitar Clean DI DBL C")
            .track("Electric Clean DBL C", "Guitar Clean DBL C")
            .end()
            .folder("R")
            .track("Amp", "Guitar Clean Amp DBL R")
            .track("DI", "Guitar Clean DI DBL R")
            .track("Electric Clean DBL R", "Guitar Clean DBL R")
            .end()
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn arrangements_and_layers_without_multi_mics() {
        // Test: Multiple arrangements with layers, but no multi-mics
        // Input: Guitar Clean, Guitar Clean DBL, Guitar Drive, Guitar Drive DBL
        // Output: Guitar -> Clean -> Main, DBL and Drive -> Main, DBL
        let items = vec![
            "Guitar Clean",
            "Guitar Clean DBL",
            "Guitar Drive",
            "Guitar Drive DBL",
        ];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Guitars (with patterns) is preserved, Electric is collapsed into it
        // Arrangements (Clean, Drive) are under Guitars
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Clean")
            .track("Main", "Guitar Clean")
            .track("DBL", "Guitar Clean DBL")
            .end()
            .folder("Drive")
            .track("Main", "Guitar Drive")
            .track("DBL", "Guitar Drive DBL")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn arrangements_and_channels_without_multi_mics() {
        // Test: Multiple arrangements with channels, but no multi-mics
        // Input: Guitar Clean L, Guitar Clean C, Guitar Clean R, Guitar Drive L, Guitar Drive C, Guitar Drive R
        // Output: Guitar -> Clean -> L, C, R and Drive -> L, C, R
        let items = vec![
            "Guitar Clean L",
            "Guitar Clean C",
            "Guitar Clean R",
            "Guitar Drive L",
            "Guitar Drive C",
            "Guitar Drive R",
        ];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Guitars (with patterns) is preserved, Electric is collapsed into it
        // Arrangements (Clean, Drive) are under Guitars, each with channels L, C, R
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .folder("Clean")
            .track("L", "Guitar Clean L")
            .track("C", "Guitar Clean C")
            .track("R", "Guitar Clean R")
            .end()
            .folder("Drive")
            .track("L", "Guitar Drive L")
            .track("C", "Guitar Drive C")
            .track("R", "Guitar Drive R")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn just_layers_without_multi_mics_or_channels() {
        // Test: Just layers, no multi-mics, no channels
        // Input: Guitar Clean, Guitar Clean DBL
        // Output: Guitar -> Main, DBL
        let items = vec!["Guitar Clean", "Guitar Clean DBL"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Guitars (with patterns) is preserved, Electric and Clean are collapsed into it
        // Layers (Main, DBL) are directly under Guitars
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .track("Main", "Guitar Clean")
            .track("DBL", "Guitar Clean DBL")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn just_channels_without_multi_mics_or_layers() {
        // Test: Just channels, no multi-mics, no layers
        // Input: Guitar Clean L, Guitar Clean C, Guitar Clean R
        // Output: Guitar -> L, C, R
        let items = vec!["Guitar Clean L", "Guitar Clean C", "Guitar Clean R"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Guitars (with patterns) is preserved, Electric, Clean, Main are collapsed into it
        // Channels (L, C, R) are directly under Guitars
        let expected = TrackStructureBuilder::new()
            .folder("Guitars")
            .track("L", "Guitar Clean L")
            .track("C", "Guitar Clean C")
            .track("R", "Guitar Clean R")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }
}
