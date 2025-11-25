//! Test for Soft Rock - Pressure multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_soft_rock_pressure() {
    let track_names = vec![
        "12th Fret Acoustic Guitar.wav",
        "12th Fret Acoustic Guitar Double.wav",
        "_Full Master - Pressure.wav",
        "_Full Mix - Pressure.wav",
        "Acoustic Guitar.wav",
        "Acoustic Guitar Double.wav",
        "Auxiliary Click Percussion.wav",
        "Auxiliary Kick Percussion.wav",
        "Auxiliary Overhead Left.wav",
        "Auxiliary Overhead Right.wav",
        "Auxiliary Snare Roll.wav",
        "Auxiliary Snare Roll Bottom.wav",
        "Bass Guitar.wav",
        "Drum Overhead Left.wav",
        "Drum Overhead Right.wav",
        "Electric Guitar.wav",
        "Kick.wav",
        "Kick Click.wav",
        "Kick Sampled.wav",
        "Lead Vocal Comp.wav",
        "Snare Rim Shots.wav",
        "Tom Roll.wav",
        "Vocal Double 1.wav",
        "Vocal Double 2.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Soft Rock - Pressure ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

