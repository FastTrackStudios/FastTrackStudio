//! Test for Steve Maggiora - It's Too Late 110BPM
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_steve_maggiora_its_too_late() {
    let track_names = vec![
        "01.Kick_02.wav",
        "02.SN TP_02.wav",
        "03.OH.01_02 L.wav",
        "04.OH.01_02 R.wav",
        "05.Bass DI_02.wav",
        "06.Bass_02.wav",
        "07.Acoustic_02.wav",
        "08.Guitar DI Rhythm_01.wav",
        "09.Guitar Rhythm_01.wav",
        "10.Piano_02.wav",
        "11.Gtr Di Solo Michael_02.wav",
        "12.Gtr Amp Solo Michael_02.wav",
        "13.Guitar DI Solo Warren_02.wav",
        "14.Guitar Amp Solo Warren_01.wav",
        "15.Vocal_02.wav",
        "16.H3000.One_02.wav",
        "17.H3000.Two_02.wav",
        "18.H3000.Three_02.wav",
        "19.Vocal.Eko.Plate_02.wav",
        "20.Vocal.Magic_02.wav",
        "21.It's Too Late Mix_02.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Steve Maggiora - It's Too Late ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

