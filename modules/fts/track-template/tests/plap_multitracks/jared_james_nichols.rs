//! Test for Jared James Nichols Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_jared_james_nichols() {
    let track_names = vec![
        "01 Kick In .wav",
        "02 Kick Out .wav",
        "03 Sub Kick .wav",
        "04 Snare Top .wav",
        "05 Snare Top.dup2 .wav",
        "06 Snare Bot .wav",
        "07 Hat .wav",
        "08 Rack .wav",
        "09 Floor .wav",
        "10 OH Hat .wav",
        "11 OH Ride .wav",
        "12 OH Mono .wav",
        "13 Crotch .wav",
        "14 Close RM Hat .wav",
        "15 Close RM Ride .wav",
        "16 Mid RM Hat .wav",
        "17 Mid RM Ride .wav",
        "18 Far RM Hat .wav",
        "19 Far RM Ride .wav",
        "20 Mono .wav",
        "21 Mono U47 .wav",
        "22 Bass DI .wav",
        "23 Bass Mic .wav",
        "24 Gtr Bus .wav",
        "25 Gtr Bus.dup1 .wav",
        "26 SoloGtr Bus.d .wav",
        "27 SoloGtr RM Left .wav",
        "28 SoloGtr RM Right .wav",
        "29 Talk Box .wav",
        "30 Jared .wav",
        "31 Jared call back .wav",
        "32 Jared Harm .wav",
        "33 Jared Harm.dup1 .wav",
        "Man In the Box Print 20220502 v2 .wav",
        "Smart Tempo Multitrack Set 1.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Jared James Nichols ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

