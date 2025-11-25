//! Test for Hip Hop - Shawshank multitracks

use track_template::{Template, Track, TrackMatcher};
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
}

