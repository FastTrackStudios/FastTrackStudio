//! Test for Pop Country - Lonesome Cowboy multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_pop_country_lonesome_cowboy() {
    let track_names = vec![
        "808 - Lonesome Cowboy.wav",
        "_Full Mix - Lonesome Cowboy.wav",
        "ALL GUITAR BUS - Lonesome Cowboy.wav",
        "ALL VOCAL BUS - Lonesome Cowboy.wav",
        "BACKING LICK - Lonesome Cowboy.wav",
        "CRASH CYMBAL - Lonesome Cowboy.wav",
        "Current - Lonesome Cowboy.wav",
        "DISTORTED CHORDS - Lonesome Cowboy.wav",
        "HIHAT - Lonesome Cowboy.wav",
        "HOOK BACKING VOCAL - Lonesome Cowboy.wav",
        "INTRO VOCAL LEFT - Lonesome Cowboy.wav",
        "INTRO VOCAL RIGHT - Lonesome Cowboy.wav",
        "KICK - Lonesome Cowboy.wav",
        "LEAD PLUCK LEFT - Lonesome Cowboy.wav",
        "LEAD PLUCK RIGHT - Lonesome Cowboy.wav",
        "MAIN VOCAL - Lonesome Cowboy.wav",
        "OUTRO VOCAL - Lonesome Cowboy.wav",
        "PERC - Lonesome Cowboy.wav",
        "SHAKER - Lonesome Cowboy.wav",
        "SNARE - Lonesome Cowboy.wav",
        "SOFT CHORD LEFT - Lonesome Cowboy.wav",
        "SOFT CHORD RIGHT - Lonesome Cowboy.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Pop Country - Lonesome Cowboy ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

