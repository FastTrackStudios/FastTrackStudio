//! Test for Heavy Rock - To the Sun multitracks

use track_template::{Template, Track, TrackMatcher};
use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_heavy_rock_to_the_sun() {
    let track_names = vec![
        "_Full Mix - To the Sun.wav",
        "Bass - To the Sun.wav",
        "Bridge gtr left amp 2 - To the Sun.wav",
        "Bridge gtr left crescendo amp 1 - To the Sun.wav",
        "Bridge gtr melody amp 2 - To the Sun.wav",
        "Bridge gtr right amp 1 - To the Sun.wav",
        "Bridge gtr right crescendo amp 2 - To the Sun.wav",
        "Hats - to the Sun.wav",
        "Intro chords lead amp 1 - To the Sun.wav",
        "Intro chords lead amp 2 - To the Sun.wav",
        "Intro melody bridge left amp 1 - To the Sun.wav",
        "Intro melody bridge right amp 2 - To the Sun.wav",
        "Kick - To the sun.wav",
        "Kick sub - To the Sun.wav",
        "Ovr Head - To the Sun.wav",
        "Ride - To the Sun.wav",
        "Room close - To the Sun.wav",
        "Room far - To the Sun.wav",
        "Room mono - To the Sun.wav",
        "Rytm gtr left amp 1 - To the Sun.wav",
        "Rytm gtr left amp 2 - To the Sun.wav",
        "Rytm gtr right amp 1 - To the Sun.wav",
        "Rytm gtr right amp 2 - To the Sun.wav",
        "Snare btm - To the Sun.wav",
        "Snare top - To the Sun.wav",
        "Tom 1 - To the Sun.wav",
        "Tom 2 - To the Sun.wav",
        "Tom 3 - To the Sun.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Heavy Rock - To the Sun ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
        println!("  Group: {:?}, Sub: {:?}, Arrangement: {:?}, Channel: {:?}, Multi-Mic: {:?}",
            parsed.group_prefix, parsed.sub_type, parsed.arrangement, parsed.channel, parsed.multi_mic);
    }
}

