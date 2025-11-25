//! Test for Hyper Pop Metal - LSD multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_hyper_pop_metal_lsd() {
    let track_names = vec![
        "_Full Master - LSD.wav",
        "_Full Mix - LSD.wav",
        "Bass & Hyper bass (With original FX) - LSD.wav",
        "Bass - LSD.wav",
        "Bass Guitar - LSD.wav",
        "Crash 1 - LSD.wav",
        "Cymbal Loop - LSD.wav",
        "Cymbals - LSD.wav",
        "Guitar 1 - LSD.wav",
        "Guitar 2 - LSD.wav",
        "Guitar 3 - LSD.wav",
        "Guitar 4 - LSD.wav",
        "Hi-Hat - LSD.wav",
        "Hyper Bass - LSD.wav",
        "Hyper Kick - LSD.wav",
        "LSD Synth - LSD.wav",
        "Metal Trap Kick - LSD.wav",
        "Perc - LSD.wav",
        "Snare 1 - LSD.wav",
        "Snare 2 - LSD.wav",
        "Trap Kick - LSD.wav",
        "Trap Snare 1 - LSD.wav",
        "Trap Snare 2 - LSD.wav",
        "White Noise - LSD.wav",
        "Zappity - LSD.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Hyper Pop Metal - LSD ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

