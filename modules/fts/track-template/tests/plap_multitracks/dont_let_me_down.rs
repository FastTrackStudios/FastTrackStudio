//! Test for Don't Let Me Down Aspen Pittman AP1B Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_dont_let_me_down() {
    let track_names = vec![
        "01.Kick_01.wav",
        "02.Snare_01.wav",
        "03.OH_01.wav",
        "04.Bass Di_01.wav",
        "05.Bass Amp_01.wav",
        "06.Gtr DI_01.wav",
        "07.Gtr Amp_01.wav",
        "08.Keys_01.wav",
        "09.Vi.Vocal_01.wav",
        "10.Caitlin Vocal_01.wav",
        "11.Don't Let Me Down Joe Carrell Mix_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Don't Let Me Down ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

