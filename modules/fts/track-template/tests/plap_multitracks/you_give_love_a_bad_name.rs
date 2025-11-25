//! Test for You Give Love A Bad Name PLAP Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_you_give_love_a_bad_name() {
    let track_names = vec![
        "095 Pop Tamb.L.wav",
        "095 Pop Tamb.R.wav",
        "Acoustic.Right.wav",
        "Acoustic.wav",
        "Bass.wav",
        "Drums.PNT.L.wav",
        "Drums.PNT.R.wav",
        "Fiddle PNT.L.wav",
        "Fiddle PNT.R.wav",
        "Guitar Slide.wav",
        "Guitar Solo.wav",
        "Mando.wav",
        "Vocal.Harmony.One.wav",
        "Vocal.Harmony.Two.wav",
        "Vocal.Tune.Lead.wav",
        "You Give Love A Bad Name.PRINT.L.wav",
        "You Give Love A Bad Name.PRINT.R.wav",
        "banjo.One.wav",
        "banjo.Solo.wav",
        "banjo.Two.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing You Give Love A Bad Name ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

