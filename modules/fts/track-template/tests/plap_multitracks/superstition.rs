//! Test for Superstition Multitracks Zen Quadro 24Bit 48K
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_superstition() {
    let track_names = vec![
        "001 Soprano sax_01.wav",
        "002 Alto sax_01.wav",
        "003 Alto (uni tenor)_01.wav",
        "004Tenor sax_01.wav",
        "005 Bari sax_01.wav",
        "006 Kick_01.wav",
        "007 Snare_01.wav",
        "008 Overhead Hats_01.wav",
        "009 Overhead Ride_01.wav",
        "010 Lead Vocal_01.wav",
        "011 Horn Vocal 1_01.wav",
        "012 Horn Vocal 2_01.wav",
        "013 Horn Vocal 3_01.wav",
        "014 Horn Vocal_01.wav",
        "015 Bass DI_01.wav",
        "016 Bass Amp Sim_01.wav",
        "017 Elec Gui 1 DI_01.wav",
        "018 Elec Gui 1 Amp Sim_01.wav",
        "019 Elec Gui 2 DI_01.wav",
        "020 Elec Gui 2 Amp Sim_01.wav",
        "021 Elec Gui 3 DI_01.wav",
        "022 Elec Gui 4 Amp Sim_01.wav",
        "023 Elec Gui 4 DI_01.wav",
        "024 Elec Gui 3 Amp Sim B_01.wav",
        "Superstition Mix 1_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Superstition ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

