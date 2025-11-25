//! Test for Hyper Pop Industrial - Death Wish multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_hyper_pop_industrial_death_wish() {
    let track_names = vec![
        "_Full Master - Death Wish.wav",
        "_Full Mix - Death Wish.wav",
        "Acoustic Cymbals - Death Wish.wav",
        "Acoustic Snare - Death Wish.wav",
        "Bass - Death Wish.wav",
        "Bass Guitar - Death Wish.wav",
        "Blade FX - Death Wish.wav",
        "Chimes - Death Wish.wav",
        "Cymbals 1 - Death Wish.wav",
        "Dist Pluck High (produces noise) - Death Wish.wav",
        "Dist Pluck Low (produces noise) - Death Wish.wav",
        "Growl Bass - Death Wish.wav",
        "Guitar 1 (26% left in mix) - Death Wish.wav",
        "Guitar 2 MID - Death Wish.wav",
        "Guitar 3 (28% right in mix) - Death Wish.wav",
        "Gunshot FX - Death Wish.wav",
        "Hi-Hat - Death Wish.wav",
        "Kick - Death Wish.wav",
        "Live Cymbals - Death Wish.wav",
        "Live Kick - Death Wish.wav",
        "Live Snare - Death Wish.wav",
        "Low Main Guitar - Death Wish.wav",
        "Noise FX - Death Wish.wav",
        "Ring FX - Death Wish.wav",
        "Riser - Death Wish.wav",
        "Saw Synth - Death Wish.wav",
        "Snare 1 - Death Wish.wav",
        "Snare 2 - Death Wish.wav",
        "Snare 3 (Clap) - Death Wish.wav",
        "Weird Snare 1 - Death Wish.wav",
        "Weird Snare 2 - Death Wish.wav",
        "White Noise - Death Wish.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Hyper Pop Industrial - Death Wish ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

