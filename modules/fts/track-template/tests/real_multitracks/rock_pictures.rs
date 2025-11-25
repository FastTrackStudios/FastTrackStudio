//! Test for Rock - Pictures multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_rock_pictures() {
    let track_names = vec![
        "808 Bassline - Pictures.wav",
        "_Demo Mix - Pictures.wav",
        "Background Texture - Pictures.wav",
        "Current- Pictures.wav",
        "Electric Lead Guitar - Pictures.wav",
        "Electric Outro Solo - Pictures.wav",
        "Guitar Chords - Pictures.wav",
        "Guitar Pluck Left - Pictures.wav",
        "Guitar Pluck Right - Pictures.wav",
        "Guitar Plucks Mixed Bus - Pictures.wav",
        "HiHats- Pictures.wav",
        "Hook Vocal Bus - Pictures.wav",
        "Hook Vocal Left - Pictures.wav",
        "Hook Vocal Middle- Pictures.wav",
        "Hook Vocal Right- Pictures.wav",
        "Kick - Pictures.wav",
        "Master- Pictures.wav",
        "Outro Riser- Pictures.wav",
        "Rythym Guitar Left- Pictures.wav",
        "Rythym Guitar Right- Pictures.wav",
        "Rythym Guitars Mixed Bus- Pictures.wav",
        "Snare 1- Pictures.wav",
        "Snare 2- Pictures.wav",
        "Snare Bus- Pictures.wav",
        "Spacey Mallets Bus- Pictures.wav",
        "Spacey Mallets Left- Pictures.wav",
        "Spacey Mallets Right- Pictures.wav",
        "Verse Vocal Bus.wav",
        "Verse Vocal Left- Pictures.wav",
        "Verse Vocal Middle- Pictures.wav",
        "Verse Vocal Right- Pictures.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Rock - Pictures ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

