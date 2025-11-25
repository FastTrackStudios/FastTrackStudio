//! Test for Pop - Fight multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_pop_fight() {
    let track_names = vec![
        "808 - FIGHT.wav",
        "_Full Mix - FIGHT.wav",
        "BONGO PERC - FIGHT.wav",
        "BOTTLE PERC - FIGHT.wav",
        "CHANGE JINGLE BUS - FIGHT.wav",
        "CHANGE JINGLE LEFT - FIGHT.wav",
        "CHANGE JINGLE RIGHT - FIGHT.wav",
        "Current - FIGHT.wav",
        "GUITAR SOLO - FIGHT.wav",
        "HIHAT BUS - FIGHT.wav",
        "HIHAT LEFT - FIGHT.wav",
        "HIHAT RIGHT - FIGHT.wav",
        "HOOK BACKING VOCAL - FIGHT.wav",
        "HOOK MAIN VOCAL - FIGHT.wav",
        "HOOK VOCAL BUS - FIGHT.wav",
        "KICK - FIGHT.wav",
        "PERC - FIGHT.wav",
        "RADAR PERCUSSION - FIGHT.wav",
        "RHODES - FIGHT.wav",
        "SNARE - FIGHT.wav",
        "SYNTH SAX - FIGHT.wav",
        "VERSE VOCAL - FIGHT.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Pop - Fight ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

