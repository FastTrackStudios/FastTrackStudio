//! Test for Covering The Beatles Girl Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_covering_the_beatles_girl() {
    let track_names = vec![
        "AC GTR 1_01.wav",
        "Acoustic Guitar_TAPE .wav",
        "BASS.03_01.wav",
        "Bass_TAPE .wav",
        "Chad1_01.wav",
        "Chad2_01.wav",
        "Drums_TAPE_2.wav",
        "Framus end track Perry_01.wav",
        "GIRL_MDN.wav",
        "Girl Sam J160 1_01.wav",
        "Girl Sam J160 2_01.wav",
        "Lou1_01.wav",
        "Lou2_01.wav",
        "Marc VOX 2.wav",
        "Marc VOX.wav",
        "OverHead DRUMS_01.wav",
        "Snare DRUMS _01.R.wav",
        "Tit1_01.wav",
        "Tit2_01.wav",
        "Tit3_01.wav",
        "boomK__BottleOver_02.wav",
        "boomK__FloorOmni414_02.wav",
        "boomK__KickRe20_02.wav",
        "kitA_KickRe20_02.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Covering The Beatles Girl ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

