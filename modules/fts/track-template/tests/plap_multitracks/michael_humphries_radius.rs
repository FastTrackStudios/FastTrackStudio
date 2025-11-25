//! Test for Michael Humphries - Radius
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_michael_humphries_radius() {
    let track_names = vec![
        "01.Kick_01.wav",
        "02.SN Top_01.wav",
        "03.SN Bot_01.wav",
        "04.Rack.Tom_01.wav",
        "05.Flr.Tom_01.wav",
        "06.Hi Hat_01.wav",
        "07.OH_01.wav",
        "08.Room_01.wav",
        "09.Bass DI_01.wav",
        "10.Gtr Amp_01.wav",
        "11.Nord Piano_01.wav",
        "12.Radius Mix_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Michael Humphries - Radius ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

