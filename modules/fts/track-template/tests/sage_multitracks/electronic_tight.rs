//! Test for Electronic - Tight multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_electronic_tight() {
    let track_names = vec![
        "_Full Mix - TIGHT.wav",
        "ANNOYING RESONANCE - TIGHT.wav",
        "ATMO BACKGROUND - TIGHT.wav",
        "ATMO STABS - TIGHT.wav",
        "ATMO SYNTH - TIGHT.wav",
        "BACKGROUND MELODY - TIGHT.wav",
        "BASS - TIGHT.wav",
        "BREAK MELODY MAIN - TIGHT.wav",
        "CLAP 1 - TIGHT.wav",
        "CLAP 2 - TIGHT.wav",
        "CLAP FX - TIGHT.wav",
        "HIHAT - TIGHT.wav",
        "HIHAT LOOP - TIGHT.wav",
        "KICK - TIGHT.wav",
        "LEAD - TIGHT.wav",
        "LEAD LAYER - TIGHT.wav",
        "LEAD LAYER 2 - TIGHT.wav",
        "MALE VOCAL - TIGHT.wav",
        "MALE VOCAL FX REPEAT - TIGHT.wav",
        "MALE VOCAL PAN L - TIGHT.wav",
        "MALE VOCAL PAN R - TIGHT.wav",
        "NOISE FX - TIGHT.wav",
        "PAD - TIGHT.wav",
        "PERC LOOP - TIGHT.wav",
        "PERC LOOP 2 - TIGHT.wav",
        "PLUCKS - TIGHT.wav",
        "WHITE NOISE SWEEP - TIGHT.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Electronic - Tight ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

