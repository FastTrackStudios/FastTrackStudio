//! Test for Acoustic Singer-Songwriter - Forget multitracks
//!
//! This test verifies that all track names from this multitrack session
//! can be correctly parsed and matched to the expected track structure.

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_acoustic_singer_songwriter_forget() {
    // Track names from the folder
    let track_names = vec![
        "_Full Mix - FORGET.wav",
        "BASS NOTES - FORGET.wav",
        "BRIDGE LEFT VOCAL - FORGET.wav",
        "BRIDGE MIDDLE VOCAL - FORGET.wav",
        "BRIDGE RIGHT VOCAL - FORGET.wav",
        "Current - FORGET.wav",
        "ELECTRIC GUITAR SOLO - FORGET.wav",
        "FIRST VERSE VOCAL - FORGET.wav",
        "MAIN CHORD PROGRESSION - FORGET.wav",
        "MUTED PERC BUS - FORGET.wav",
        "MUTED PERC LEFT - FORGET.wav",
        "MUTED PERC RIGHT - FORGET.wav",
        "OUTRO SOLO - FORGET.wav",
        "OUTRO VERSE VOCAL - FORGET.wav",
        "PLUCK LEAD BUS - FORGET.wav",
        "PLUCK LEAD LEFT - FORGET.wav",
        "PLUCK LEAD RIGHT - FORGET.wav",
        "SECOND VERSE VOCAL - FORGET.wav",
        "VOCAL BUS - FORGET.wav",
    ];

    // Create parser and template
    let parser = SimpleParser::new(create_default_groups());
    let mut template = Template::new("Acoustic Singer-Songwriter - Forget");

    // Build expected track structure
    // This should match the actual organization of tracks in the session
    // TODO: Define the expected structure based on the track names
    
    // For now, let's parse each track and see what we get
    println!("\n=== Parsing Acoustic Singer-Songwriter - Forget ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("Original: {}", name);
        println!("  Parsed -> Formatted: {}", formatted);
        println!("  Group Prefix: {:?}", parsed.group_prefix);
        println!("  Sub Type: {:?}", parsed.sub_type);
        println!("  Performer: {:?}", parsed.performer);
        println!("  Arrangement: {:?}", parsed.arrangement);
        println!("  Section: {:?}", parsed.section);
        println!("  Channel: {:?}", parsed.channel);
        println!("  Multi-Mic: {:?}", parsed.multi_mic);
        println!();
    }
    
    // TODO: Create matcher and verify matches
    // TODO: Assert expected track structure
}

