//! Test for Marc Martel - Don't Stop Me Now (Cover) Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_marc_martel_dont_stop_me_now() {
    let track_names = vec![
        "01.Kick In_01.wav",
        "02.Kick Out_01.wav",
        "03.Kick Sample_01.wav",
        "04.Snare Top_01.wav",
        "05.Snare Bottom_01.wav",
        "06.Snare Sample_01.wav",
        "07.Snare Sample Two_01.wav",
        "08.Tom1_01.wav",
        "09.Tom2_01.wav",
        "10.HighHat_01.wav",
        "11.OH_01.wav",
        "12.Rooms_01.wav",
        "13.Percussion_01.wav",
        "14.Bass DI_01.wav",
        "15.Piano_01.wav",
        "16.Lead Guitar Amplitube Left_01.wav",
        "17.Lead Guitar Amplitube Right_01.wav",
        "18.Lead Guitar Clean DI Left_01.wav",
        "19.Lead Guitar Clean DI Right_01.wav",
        "20.Vocal_01.wav",
        "21.H3000.One_01.wav",
        "22.H3000.Two_01.wav",
        "23.H3000.Three_01.wav",
        "24.Vocal.Eko.Plate_01.wav",
        "25.Vocal.Magic_01.wav",
        "26.BGV1_01.wav",
        "27.BGV2_01.wav",
        "28.BGV3_01.wav",
        "29.BGV4_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Marc Martel - Don't Stop Me Now ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

