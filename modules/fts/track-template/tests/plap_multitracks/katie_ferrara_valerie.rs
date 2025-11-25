//! Test for Katie Ferrara - Valerie 103BPM Antelope Discrete 4 Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_katie_ferrara_valerie() {
    let track_names = vec![
        "01.Kick_01-01.wav",
        "02.Snare_01-01.wav",
        "03.OH_01-01.wav",
        "04.Bass DI_01-01.wav",
        "05.Bass Amp_01-01.wav",
        "06.Acoustic_01-01.wav",
        "07.Gtr DI_01-01.wav",
        "08.Gtr Amp_01-01.wav",
        "09.Gtr DI.Riff_01-01.wav",
        "10.Gtr Amp.Riff_01-01.wav",
        "11.Gtr DI.Solo_01-01.wav",
        "12.Gtr Amp.Solo_01-01.wav",
        "13.Vocal_01-01.wav",
        "14.KD.BV_01-01.wav",
        "15.KD.BV Dbl_01-01.wav",
        "16.H3000.One_01-01.wav",
        "17.H3000.Two_01-01.wav",
        "18.H3000.Three_01-01.wav",
        "19.Vocal.Eko.Plate_01-01.wav",
        "20.Vocal.Magic_01-01.wav",
        "21.Valerie Mix_01-01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Katie Ferrara - Valerie ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

