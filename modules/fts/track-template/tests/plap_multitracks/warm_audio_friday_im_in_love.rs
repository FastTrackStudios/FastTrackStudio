//! Test for Warm Audio 2MPX - Friday I'm In Love Cover Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_warm_audio_friday_im_in_love() {
    let track_names = vec![
        "01.Kick_01.wav",
        "02.Snare_01.wav",
        "03.Ohd_01 L.wav",
        "04.Ohd_01 R.wav",
        "05.Tambo_01.wav",
        "06.Bass DI_01.wav",
        "07.Bass Amp_01.wav",
        "08.Acoustic_01.wav",
        "09.Acoustic.dbl_01.wav",
        "10.Acoustic OD_01.wav",
        "11.Acoustic OD Dbl_01.wav",
        "12.Gtr 1 DI_01.wav",
        "13.Gtr 1 Amp_01.wav",
        "14.Gtr 2 DI_01.wav",
        "15.Gtr 2 Amp_01.wav",
        "16.Gtr 2 DI Overlap_01.wav",
        "17.Gtr 2 Amp Overlap_01.wav",
        "18.Gtr Solo DI_01.wav",
        "19.Gtr Solo Amp_01.wav",
        "20.Gtr Root Note DI_01.wav",
        "21.Gtr Root Note Amp_01.wav",
        "23.Pad_01.wav",
        "24.Vocal_01.wav",
        "25.Vocal Overlap_01.wav",
        "26. Vocal Dbl_01.wav",
        "27.HARM_01.wav",
        "28.Harm Overlap_01.wav",
        "29.Friday I'm In Love Mix_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Warm Audio - Friday I'm In Love ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

