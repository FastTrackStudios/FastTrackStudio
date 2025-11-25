//! Test for Gloria Anderson - Households Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_gloria_anderson_households() {
    let track_names = vec![
        "01.Households ACO JC MIXX_01.wav",
        "02.Kick LIM_01.wav",
        "03.Kick Out LIM_01.wav",
        "04.Sn T_01.wav",
        "05.Sn B_01.wav",
        "06.Hat_01.wav",
        "07.Tom 1 EDT_01.wav",
        "08.Tom 2 EDT_01.wav",
        "09.OH_01.wav",
        "10.Room_01.wav",
        "11.Bass Body_01.wav",
        "12.Bass Neck_01.wav",
        "13.AG 1_01.wav",
        "14.Piano_01.wav",
        "15.Steel_01.wav",
        "16.Vocal_01.wav",
        "Choir.wav",
        "Households_TimHoek_NashGtrL.wav",
        "Households_TimHoek_NashGtrR.wav",
        "Households_TimHoek_Strings.wav",
        "Strings (R).wav",
        "Vocal harmony (D).wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Gloria Anderson - Households ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

