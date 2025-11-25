//! Test for Electronic - Dreams multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_electronic_dreams() {
    let track_names = vec![
        "_Full Mix - DREAMS.wav",
        "BASS LOW - DREAMS.wav",
        "BASS MID - DREAMS.wav",
        "CLAP 1 - DREAMS.wav",
        "CLAP 2 - DREAMS.wav",
        "CLINK FX - DREAMS.wav",
        "CRASH FX - DREAMS.wav",
        "CRASH FX 2 - DREAMS.wav",
        "CRASH FX 3 - DREAMS.wav",
        "CRASH FX 4 - DREAMS.wav",
        "DRUM FILL - DREAMS.wav",
        "FX ATMO - DREAMS.wav",
        "HIHAT C - DREAMS.wav",
        "HIHAT L - DREAMS.wav",
        "HIHAT R - DREAMS.wav",
        "KICK - DREAMS.wav",
        "MAIN LEAD - DREAMS.wav",
        "MAIN LEAD BREAKDOWN - DREAMS.wav",
        "NOISE HIHATS 16TH - DREAMS.wav",
        "PLUCKS - DREAMS.wav",
        "PSYCHEDELIC ATMO STABS - DREAMS.wav",
        "RIDE - DREAMS.wav",
        "SHAKER HIHAT - DREAMS.wav",
        "SHAKERS - DREAMS.wav",
        "SINGLE HAT HIT - DREAMS.wav",
        "TAMBOURINE LOOP - DREAMS.wav",
        "TOMS - DREAMS.wav",
        "VOCAL FX FREEZE - DREAMS.wav",
        "VOCALS - DREAMS.wav",
        "VOCALS FX - DREAMS.wav",
        "VOCAL TEXTURE FX - DREAMS.wav",
        "WHITE NOISE SWEEP - DREAMS.wav",
        "WHITE NOISE SWEEP FX - DREAMS.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Electronic - Dreams ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

