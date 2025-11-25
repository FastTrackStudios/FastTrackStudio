//! Test for Day Tripper Cover - Warm Audio Wa-19 Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_day_tripper_cover() {
    let track_names = vec![
        "Steve BGV1A.06_01.wav",
        "Steve BGV1B.06_01.wav",
        "Steve BGV1C.06_01.wav",
        "Steve BGV1D.06_01.wav",
        "Steve BGV1_SUM.06_01.wav",
        "Steve BGV2A.06_01.wav",
        "Steve BGV2B.06_01.wav",
        "Steve BGV2C.06_01.wav",
        "Steve BGV2D.06_01.wav",
        "Steve BGV2_SUM.06_01.wav",
        "Steve Lead VOX Take 1.06_01.wav",
        "Steve Lead VOX Take 2.06_01.wav",
        "bass hofner.Duplicate _01.wav",
        "bass hofner_01.wav",
        "click.Edit_01.wav",
        "fernando strat_02.wav",
        "gretsch fernando_02.wav",
        "kick_01.wav",
        "oh_01.wav",
        "snare_01.wav",
        "swell_01.wav",
        "tambourine_01.wav",
        "warren strat_02.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Day Tripper Cover ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

