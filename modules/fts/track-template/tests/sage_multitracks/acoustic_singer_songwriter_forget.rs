//! Test for Acoustic Singer-Songwriter - Forget multitracks
//!
//! This test verifies that all track names from this multitrack session
//! can be correctly parsed.
//!
//! TODO: Add expected track structure and matching verification

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

    // Create parser
    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Acoustic Singer-Songwriter - Forget ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

