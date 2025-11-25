//! Test for Singer-Songwriter - Nothing Remains multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_singer_songwriter_nothing_remains() {
    let track_names = vec![
        "_Full Master - Nothing Remains.wav",
        "_Full Mix - Nothing Remains.wav",
        "BGV.wav",
        "BGV Outro 1.wav",
        "BGV Outro 2.wav",
        "BGV Outro 3.wav",
        "Clap Sample.wav",
        "Classical Guitar Loop Reamp.wav",
        "Drum Loop Tape.wav",
        "Electric Guitar Loop.wav",
        "Electric Guitar Loop 2.wav",
        "Korg Melody.wav",
        "Lead Vocal Comp.wav",
        "Pad.wav",
        "Synth.wav",
        "Vocal Double.wav",
        "Vocal Harmony.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Singer-Songwriter - Nothing Remains ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

