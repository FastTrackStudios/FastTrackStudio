//! Test for Pop - Menotu multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_pop_menotu() {
    let track_names = vec![
        "808 - MENOTU.wav",
        "_Full Mix - MENOTU.wav",
        "ACOUSTIC GUITAR - MENOTU.wav",
        "Current - MENOTU.wav",
        "HIHAT - MENOTU.wav",
        "HOOK BACKING VOCAL - MENOTU.wav",
        "HOOK VOCAL - MENOTU.wav",
        "INTRO-OUTRO VOCALS - MENOTU.wav",
        "KICK - MENOTU.wav",
        "LOFI PIANO BUS - MENOTU.wav",
        "LOFI PIANO LEFT - MENOTU.wav",
        "LOFI PIANO RIGHT - MENOTU.wav",
        "ORGANIC BASSLINE - MENOTU.wav",
        "RHODES - MENOTU.wav",
        "SNARE - MENOTU.wav",
        "VERSE-BRIDGE VOCAL - MENOTU.wav",
        "VOCAL BUS - MENOTU.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Pop - Menotu ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

