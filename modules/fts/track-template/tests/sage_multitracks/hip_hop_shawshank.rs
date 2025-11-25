//! Test for Hip Hop - Shawshank multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_hip_hop_shawshank() {
    let track_names = vec![
        "_Full Master - Shawshank.wav",
        "Adlib Vox - Shawshank.wav",
        "Bridge Vox - Shawshank.wav",
        "Hook Vox - Shawshank.wav",
        "Intro - Shawshank.wav",
        "Verse 1 Vox - Shawshank.wav",
        "Verse 2 Vox - Shawshank.wav",
        "Verse Stem 1 - Shawshank.mp3",
        "Verse Stem 2 - Shawshank.mp3",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Hip Hop - Shawshank ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

