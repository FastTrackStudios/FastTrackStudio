//! Test for Ain't No Sunshine - Roswell Mini K47x and Mini K67x Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_aint_no_sunshine() {
    let track_names = vec![
        "01.Djembe Bottom 47X_01.wav",
        "02.Djembe Top 67X_01.wav",
        "03.Bass DI.01_02.wav",
        "04.Bass Amp 47X_01.wav",
        "05.Gtr DI.01_02.wav",
        "06.Gtr Amp.67X_01.wav",
        "07.Gtr Warren Amp.01_02.wav",
        "08.Gtr Solo DI.01_02.wav",
        "09.Gtr Solo Amp.67X_01.wav",
        "10.Acoustic 47X_01.wav",
        "11.Vocal.47X_01.wav",
        "12.Ain't No Sunshine Mix_Roswell Mini K47x Mini K67x_02.wav",
        "Djembe Top Quantized.wav",
        "HiHat.wav",
        "Kick.wav",
        "Overhead.wav",
        "Sidestick.wav",
        "Snare.wav",
        "STRINGS HIGH.wav",
        "STRINGS MAIN.wav",
        "Tom  3.wav",
        "Tom  4.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Ain't No Sunshine ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

