//! Test for Iron Maiden - The Trooper Stems
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_iron_maiden_trooper() {
    let track_names = vec![
        "08-Kick-TheTrooper.wav",
        "09-Snare-TheTrooper.wav",
        "10-OH-TheTrooper.wav",
        "12-Bass DI-TheTrooper.wav",
        "13-Bass Amp-TheTrooper.wav",
        "15-Rhy Gtr L-TheTrooper.wav",
        "16-Rhy Gtr R-TheTrooper.wav",
        "17-Solo 1-TheTrooper.wav",
        "18-Solo 2-TheTrooper.wav",
        "19-Solo 3-TheTrooper.wav",
        "21-Vocal 1-TheTrooper.wav",
        "22-Vocal 2-TheTrooper.wav",
        "23-Vocal 3-TheTrooper.wav",
        "Trooper-mix1.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Iron Maiden - The Trooper ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

