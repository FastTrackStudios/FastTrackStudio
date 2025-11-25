//! Test for Mars_SWS
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_mars_sws() {
    let track_names = vec![
        "BGV1A.wav",
        "BGV1B.wav",
        "BGV1C.wav",
        "BGV1D.wav",
        "BGV1_SUM.wav",
        "BGV2A.wav",
        "BGV2B.wav",
        "BGV2C.wav",
        "BGV2D.wav",
        "BGV2_SUM.wav",
        "BGV3A.wav",
        "BGV3B.wav",
        "BGV3C.wav",
        "BGV3D.wav",
        "BGV3_SUM.wav",
        "BGV4A.wav",
        "BGV4B.wav",
        "BGV4C.wav",
        "BGV4D.wav",
        "BGV4_SUM.wav",
        "Bass Synth.L.wav",
        "Bass Synth.R.wav",
        "Bass Synth.wav",
        "Crystalizer_Print.wav",
        "Crystalizer_Print_1.wav",
        "Kim VOX.wav",
        "Kim VOX_1.wav",
        "Kim VOX_2.wav",
        "Kim VOX_SUM.wav",
        "Mars Kim vx.wav",
        "Mars_PLAP.wav",
        "Piano L.wav",
        "Piano R.wav",
        "Piano Room Mono.wav",
        "Piano_Rough_76bpm.wav",
        "Piano_SUM.wav",
        "Reverse Piano.wav",
        "Steve VOX.wav",
        "Steve VOX_SUM.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Mars_SWS ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

