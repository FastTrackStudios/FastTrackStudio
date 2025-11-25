//! Test for Electronic - Why multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_electronic_why() {
    let track_names = vec![
        "_Full Mix - WHY.wav",
        "BREATH FX - WHY.wav",
        "CLAP - WHY.wav",
        "DIVA PLUCKS - WHY.wav",
        "FX CYMBAL - WHY.wav",
        "FX RISER - WHY.wav",
        "HARMONY - WHY.wav",
        "HIHAT CLOSED - WHY.wav",
        "KICK - WHY.wav",
        "LEAD LAYER - WHY.wav",
        "LEAD LAYER 2 - WHY.wav",
        "LEAD PAD - WHY.wav",
        "LEAD PAD FX - WHY.wav",
        "MAIN LEAD - WHY.wav",
        "MID BASS - WHY.wav",
        "PAD - WHY.wav",
        "PERC STAB - WHY.wav",
        "REVERSED CLAP FX - WHY.wav",
        "REVERSED FX - WHY.wav",
        "REVERSED FX 2 - WHY.wav",
        "REVERSED WHITE NOISE FX - WHY.wav",
        "RIDE HH - WHY.wav",
        "RISER FX - WHY.wav",
        "SHAKER 16TH - WHY.wav",
        "SHAKER HH - WHY.wav",
        "SNAP - WHY.wav",
        "SNARE ROLL - WHY.wav",
        "SUB BASS - WHY.wav",
        "TEXTURE - WHY.wav",
        "TEXTURE FX - WHY.wav",
        "VOCAL FX - WHY.wav",
        "VOCALS - WHY.wav",
        "VOCALS FX - WHY.wav",
        "VOCALS HARMONY - WHY.wav",
        "WHITE NOISE FX - WHY.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Electronic - Why ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

