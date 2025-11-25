//! Test for Rock - Sunrise multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_rock_sunrise() {
    let track_names = vec![
        "_Full Mix - Sunrise.wav",
        "Acoustic 1 - Sunrise.wav",
        "Acoustic 2 - Sunrise.wav",
        "Ad Libs - Sunrise.wav",
        "Bass Low Split - Sunrise.wav",
        "Bass Treble Split - Sunrise.wav",
        "BG Vox - Sunrise.wav",
        "Birds - Sunrise.wav",
        "Choir 1 - Sunrise.wav",
        "Choir 2 - Sunrise.wav",
        "Choir 3 - Sunrise.wav",
        "Claps - Sunrise.wav",
        "Closed Hat - Sunrise.wav",
        "EG 1 - Sunrise.wav",
        "EG 2 - Sunrise.wav",
        "Flute - Sunrise.wav",
        "Harp Arp - Sunrise.wav",
        "Impact Crash - Sunrise.wav",
        "Kick - Sunrise.wav",
        "Lead Guitar - Sunrise.wav",
        "Lead Vox - Sunrise.wav",
        "Lead Vox Double - Sunrise.wav",
        "Lofi Kick - Sunrise.wav",
        "Open Hat - Sunrise.wav",
        "Percs - Sunrise.wav",
        "Piano Melody - Sunrise.wav",
        "Reverse Piano - Sunrise.wav",
        "Risers - Sunrise.wav",
        "Shaker - Sunrise.wav",
        "Snaps - Sunrise.wav",
        "Snare 1 - Sunrise.wav",
        "Snare 2 - Sunrise.wav",
        "String Pad - Sunrise.wav",
        "Verse Vox - Sunrise.wav",
        "Vocal Riser 1 - Sunrise.wav",
        "Vocal Riser 2 - Sunrise.wav",
        "Wind - Sunrise.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Rock - Sunrise ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
}

