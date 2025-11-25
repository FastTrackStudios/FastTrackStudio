//! Test for Katie Ferrara and Steve Maggiora - How Deep Is Your Love Neve 5025
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_how_deep_is_your_love() {
    let track_names = vec![
        "Acoustic SILK OFF.02_03-02.wav",
        "Bass SILK OFF_02.wav",
        "HH SILK OFF_02.wav",
        "Katie Vocal SILK OFF.02_01.wav",
        "Keys Silk OFF_01.wav",
        "Kick SILK OFF_05.wav",
        "Ride SILK OFF_02.wav",
        "Snare SILK OFF_01.wav",
        "Steve Vocal SILK OFF.03_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing How Deep Is Your Love ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

