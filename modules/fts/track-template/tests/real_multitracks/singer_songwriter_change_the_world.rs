//! Test for Singer-Songwriter - Change the World multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_singer_songwriter_change_the_world() {
    let track_names = vec![
        "_Demo Mix 1 - Change The World.mp3",
        "_Demo Mix 2 (Strings Start Later) - Change The World.mp3",
        "Bass - Change The World.wav",
        "BGV 1 - Change The World.wav",
        "BGV 1 Dbl - Change The World.wav",
        "BGV 2 - Change The World.wav",
        "BGV 2 Dbl - Change The World.wav",
        "BGV 3 - Change The World.wav",
        "BGV 3 Dbl - Change The World.wav",
        "Drums - Change The World.wav",
        "Guitar - Change The World.wav",
        "Lead Vocal - Change The World.wav",
        "Piano - Change The World.wav",
        "Strings - Change The World.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Singer-Songwriter - Change the World ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

