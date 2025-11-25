//! Test for Rock - Borderline multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_rock_borderline() {
    let track_names = vec![
        "808.wav",
        "_Full Master - Borderline.wav",
        "_Full Mix - Borderline.wav",
        "Acoustic Guitar.wav",
        "Acoustic Guitar Double.wav",
        "Bass Guitar.wav",
        "Bass Synth.wav",
        "BGV 1.wav",
        "BGV 2.wav",
        "BGV 3.wav",
        "Chorus Harmony.wav",
        "Chorus Harmony Double.wav",
        "Clap.wav",
        "Cymbal.wav",
        "Electric Guitar.wav",
        "Electric Guitar Double.wav",
        "Kick.wav",
        "Lead Vocal Comp.wav",
        "Lead Vocal Double.wav",
        "Lead Vocal Harmony.wav",
        "Melody Pluck.wav",
        "Organ 1.wav",
        "Organ 2.wav",
        "Organ 3.wav",
        "Snare.wav",
        "Snare Double.wav",
        "Swell Pad.wav",
        "Whistle 1.wav",
        "Whistle 2.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Rock - Borderline ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

