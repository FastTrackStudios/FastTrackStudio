//! Test for Heart Of Gold Multitracks Audioscape 24Bit 48K
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_heart_of_gold() {
    let track_names = vec![
        "01.Kick OptoComp_01.wav",
        "02.Snare No Compression_01.wav",
        "03.OH L 260VU_01.wav",
        "04.OH R 260VU_01.wav",
        "05.Bass DI OptoComp_01.wav",
        "06.Bass Amp 44A. 260VU_01.wav",
        "07.Bass Amp 421 260VU_01.wav",
        "08.Acoustic OptoComp_01.wav",
        "09.Acoustic Dbl OptoComp_01.wav",
        "10.Acoustic Harmonics OptoComp_01.wav",
        "11.Acoustic Outro Strum OptoComp_01.wav",
        "12.Mando OptoComp_01.wav",
        "13.Steel Guitar OptoComp_01.wav",
        "14.Piano 260VU_01.wav",
        "15.Piano Lead 260VU_01.wav",
        "16.Vocal OptoComp_01.wav",
        "17.Vocal.DBL OptoComp_01.wav",
        "18.Vocal.Triple OptoComp_01.wav",
        "19.Vocal.HARMONY OptoComp_01.wav",
        "20.Vocal.HARMONY Dbl OptoComp_01.wav",
        "21.Heart Of Gold Mix_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Heart Of Gold ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

