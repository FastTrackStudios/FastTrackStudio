//! Default metadata field patterns for extracting metadata from input strings
//! 
//! These patterns are used in a metadata-only group that extracts metadata fields
//! like Section, MultiMic, Arrangement, etc. across all groups without creating
//! structure nodes in the hierarchy.

use crate::item_metadata::prelude::*;

/// Creates a default metadata field patterns group
/// 
/// This is a metadata-only group that extracts metadata fields like Section, MultiMic,
/// Arrangement, etc. from input strings. It doesn't create a structure node in the
/// hierarchy but provides metadata extraction across all groups.
pub fn default_metadata_field_patterns() -> Group<ItemMetadata> {
    // Define each field's patterns separately for clarity
    let section = Group::builder("Section")
        .patterns(["Intro", "Verse", "Chorus", "Bridge", "Instrumental", "Outro"])
        .build();

    let arrangement = Group::builder("Arrangement")
        .patterns(["Down", "Big", "Build"])
        .build();

    let layers = Group::builder("Layers")
        .patterns(["DBL", "TPL", "Double", "Triple", "Main", "OCT", "1", "2", "3", "4", "5"])
        .build();

    let channel = Group::builder("Channel")
        .patterns(["L", "C", "R", "Left", "Center", "Right"])
        .build();

    let playlist = Group::builder("Playlist")
        .patterns([".1", ".2", ".3", ".4", ".5"])
        .build();

    let multi_mic = Group::builder("MultiMic")
        .patterns(["Top", "Bottom", "In", "Out", "DI", "Amp", "Amplitube"])
        .build();

    let performer = Group::builder("Performer")
        .patterns(["Cody", "John", "JT", "Bri"])
        .build();

    let rec_tag = Group::builder("RecTag")
        .patterns(["PASS 1", "PASS 2", "PASS 3", "PASS 4", "PASS-1", "PASS-2", "PASS-3", "PASS-4"])
        .build();

    // Combine all field patterns into a single metadata-only group
    Group::builder("MetadataFields")
        .metadata_only()
        .section(section)
        .arrangement(arrangement)
        .layers(layers)
        .channel(channel)
        .playlist(playlist)
        .multi_mic(multi_mic)
        .performer(performer)
        .rec_tag(rec_tag)
        .build()
}

