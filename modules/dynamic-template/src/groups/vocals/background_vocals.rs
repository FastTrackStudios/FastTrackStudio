//! Background vocals (BGVs) group definition

use crate::item_metadata::prelude::*;
use crate::item_metadata::ItemMetadataField;

/// Background vocals group (BGVs)
///
/// Sorting priority: Performer → Section → Arrangement (harmonies) → Layers → Channels
pub struct BackgroundVocals;

impl From<BackgroundVocals> for ItemMetadataGroup {
    fn from(_val: BackgroundVocals) -> Self {
        // Define harmony arrangements for BGVs
        // These are the Arrangement metadata field values specific to BGVs
        let harmony_arrangement = ItemMetadataGroup::builder("Arrangement")
            .patterns([
                // Voice Parts
                "Soprano",
                "soprano",
                "sop",
                "s",
                "Alto",
                "alto",
                "a",
                "Tenor",
                "tenor",
                "t",
                "Baritone",
                "baritone",
                "bar",
                "bari",
                "b",
                "Bass",
                "bass",
                "low",
                // Harmony Descriptors
                "High",
                "high",
                "high harmony",
                "high harm",
                "upper",
                "Low",
                "low",
                "low harmony",
                "low harm",
                "lower",
                "Mid",
                "mid",
                "middle",
                "mid harmony",
                "mid harm",
                "Drone",
                "drone",
                "drone harmony",
                "sustained",
                // Additional Common Harmonies
                "Harmony 1",
                "harmony 1",
                "harm 1",
                "h1",
                "harmony1",
                "harm1",
                "Harmony 2",
                "harmony 2",
                "harm 2",
                "h2",
                "harmony2",
                "harm2",
                "Harmony 3",
                "harmony 3",
                "harm 3",
                "h3",
                "harmony3",
                "harm3",
                "Oohs",
                "ooh",
                "oohs",
                "ooh harmony",
                "Aahs",
                "aah",
                "aahs",
                "aah harmony",
                "Ad Libs",
                "ad lib",
                "adlib",
                "ad libs",
                "adlibs",
            ])
            .build();

        // Configure BGVs with field priority: Performer → Section → Arrangement → Layers → Channels
        // The order of these calls determines the priority order
        // Layers uses "Main" as default value so items without a layer are grouped alongside items with layers
        // Note: BGVs does NOT use requires_parent_match because "bgv", "background", etc.
        // are already specific enough patterns that uniquely identify background vocals
        ItemMetadataGroup::builder("BGVs")
            .prefix("BGV")
            .patterns(["bgv", "background", "backing", "harmony", "choir"])
            .performer(ItemMetadataGroup::builder("Performer").build()) // Priority 1: Performer (uses global patterns)
            .section(ItemMetadataGroup::builder("Section").build()) // Priority 2: Section (uses global patterns)
            .arrangement(harmony_arrangement) // Priority 3: Arrangement (harmony-specific patterns)
            .layers(ItemMetadataGroup::builder("Layers").build()) // Priority 4: Layers (uses global patterns)
            .field_default_value(ItemMetadataField::Layers, "Main") // Default layer name for items without a layer
            .channel(
                ItemMetadataGroup::builder("Channel")
                    .patterns(["L", "C", "R", "Left", "Center", "Right"])
                    .build(),
            ) // Priority 5: Channel (order: L, C, R)
            .build()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{default_config, OrganizeIntoTracks};
    use daw::tracks::{assert_tracks_equal, TrackStructureBuilder};

    #[test]
    fn bgvs_with_harmony_arrangements() {
        // Example: BGVs with harmony arrangements
        // Input: BGV Chorus Cody Soprano, BGV Chorus Cody Alto, BGV Chorus JT High, BGV Chorus JT Low
        // Output: BGVs -> Cody -> Chorus -> Soprano, Alto and JT -> Chorus -> High, Low
        let items = vec![
            "BGV Chorus Cody Soprano",
            "BGV Chorus Cody Alto",
            "BGV Chorus JT High",
            "BGV Chorus JT Low",
        ];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Vocals -> BGVs collapses to BGVs
        // Multiple performers (Cody, JT), Chorus is collapsed per performer
        let expected = TrackStructureBuilder::new()
            .folder("BGVs")
            .folder("Cody")
            .track("Soprano", "BGV Chorus Cody Soprano")
            .track("Alto", "BGV Chorus Cody Alto")
            .end()
            .folder("JT")
            .track("low", "BGV Chorus JT Low")
            .track("High", "BGV Chorus JT High")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn bgvs_with_voice_parts() {
        // Example: BGVs with voice parts (Soprano, Alto, Tenor, Bass)
        // Input: BGV Chorus Cody Soprano, BGV Chorus Cody Alto, BGV Chorus Cody Tenor, BGV Chorus Cody Bass
        // Output: BGVs -> Cody -> Chorus -> Soprano, Alto, Tenor, Bass
        let items = vec![
            "BGV Chorus Cody Soprano",
            "BGV Chorus Cody Alto",
            "BGV Chorus Cody Tenor",
            "BGV Chorus Cody Bass",
        ];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Vocals -> BGVs collapses to BGVs
        // Cody and Chorus are collapsed
        // NOTE: "Bass" is a special case - it matches both the voice part pattern AND
        // might get a "Main" subfolder due to Layers default value
        let expected = TrackStructureBuilder::new()
            .folder("BGVs")
            .track("Soprano", "BGV Chorus Cody Soprano")
            .track("Alto", "BGV Chorus Cody Alto")
            .track("Tenor", "BGV Chorus Cody Tenor")
            .folder("Bass")
            .track("Main", "BGV Chorus Cody Bass")
            .end()
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn bgvs_with_harmony_descriptors() {
        // Example: BGVs with harmony descriptors (High, Low, Mid)
        // Input: BGV Chorus Cody High, BGV Chorus Cody Low, BGV Chorus Cody Mid
        // Output: BGVs -> Cody -> Chorus -> High, Low, Mid
        let items = vec![
            "BGV Chorus Cody High",
            "BGV Chorus Cody Low",
            "BGV Chorus Cody Mid",
        ];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Vocals -> BGVs collapses to BGVs
        // Cody and Chorus are collapsed
        let expected = TrackStructureBuilder::new()
            .folder("BGVs")
            .track("low", "BGV Chorus Cody Low")
            .track("High", "BGV Chorus Cody High")
            .track("Mid", "BGV Chorus Cody Mid")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn bgvs_with_numbered_harmonies() {
        // Example: BGVs with numbered harmonies (Harmony 1, Harmony 2, Harmony 3)
        // Input: BGV Chorus Cody Harmony 1, BGV Chorus Cody Harmony 2, BGV Chorus Cody Harmony 3
        // Output: BGVs -> Cody -> Chorus -> Harmony 1, Harmony 2, Harmony 3
        let items = vec![
            "BGV Chorus Cody Harmony 1",
            "BGV Chorus Cody Harmony 2",
            "BGV Chorus Cody Harmony 3",
        ];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Vocals -> BGVs collapses to BGVs
        // Cody and Chorus are collapsed
        let expected = TrackStructureBuilder::new()
            .folder("BGVs")
            .track("Harmony 1", "BGV Chorus Cody Harmony 1")
            .track("Harmony 2", "BGV Chorus Cody Harmony 2")
            .track("Harmony 3", "BGV Chorus Cody Harmony 3")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }

    #[test]
    fn bgvs_without_harmony_arrangements() {
        // Example: BGVs without explicit harmony arrangements
        // Input: BGV Chorus Cody, BGV Chorus JT, BGV Chorus Bri
        // Output: BGVs -> Cody -> Chorus, JT -> Chorus, Bri -> Chorus
        let items = vec!["BGV Chorus Cody", "BGV Chorus JT", "BGV Chorus Bri"];

        let config = default_config();
        let tracks = items.organize_into_tracks(&config, None).unwrap();

        println!("\nTrack list:");
        daw::tracks::display_tracklist(&tracks);

        // Vocals -> BGVs collapses to BGVs
        // Chorus and Main are collapsed under each performer
        let expected = TrackStructureBuilder::new()
            .folder("BGVs")
            .track("Bri", "BGV Chorus Bri")
            .track("Cody", "BGV Chorus Cody")
            .track("JT", "BGV Chorus JT")
            .end()
            .build();

        assert_tracks_equal(&tracks, &expected).unwrap();
    }
}
