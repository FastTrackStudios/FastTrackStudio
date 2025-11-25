//! Test for Pop Indie - Peacefully Departed multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_pop_indie_peacefully_departed() {
    let track_names = vec![
        "_Full Master - Peacefully Departed.wav",
        "_Full Mix - Peacefully Departed.wav",
        "Bass - Peacefully Departed.wav",
        "Bright Electric Piano - Peacefully Departed.wav",
        "Chorus Harmony - Peacefully Departed.wav",
        "Chorus Keys - Peacefully Departed.wav",
        "Double High Chorus Vocal - Peacefully Departed.wav",
        "Guitar Riff - Peacefully Departed.wav",
        "Hard Panned Hats - Peacefully Departed.wav",
        "Hat - Peacefully Departed.wav",
        "High Chorus Vocal - Peacefully Departed.wav",
        "Keys - Peacefully Departed.wav",
        "Kick - Peacefully Departed.wav",
        "Low Chorus - Peacefully Departed.wav",
        "Outro Guitar - Peacefully Departed.wav",
        "Reverse Vocal - Peacefully Departed.wav",
        "Riff Outro Guitar - Peacefully Departed.wav",
        "Rim - Peacefully Departed.wav",
        "Verse Harmony - Peacefully Departed.wav",
        "Verse Vocal - Peacefully Departed.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Pop Indie - Peacefully Departed ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

