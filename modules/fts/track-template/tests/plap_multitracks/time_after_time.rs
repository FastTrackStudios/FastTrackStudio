//! Test for Time After Time Presonus Quantum HD 8 126BPM
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_time_after_time() {
    let track_names = vec![
        "01.LV BECCA.TimeAfterTime.126bpm_01.wav",
        "02.BV LUCA.TimeAfterTime.126bpm_01.wav",
        "03.Acoustic Guitar.Nick.TimeAfterTime.126bpm_01.wav",
        "04.Electric.Gtr.Left.Warren.TimeAfterTime.126bpm_01.wav",
        "05.Electric.Gtr.Right.Warren.TimeAfterTime.126bpm_01.wav",
        "06.BASS AMP.Warren.TimeAfterTime.126bpm_01.wav",
        "07.BASS DI.Warren.TimeAfterTime.126bpm_01.wav",
        "08.Kick.TimeAfterTime.126bpm_01.wav",
        "09.Snare.TimeAfterTime.126bpm_01.wav",
        "10.Kit.Mono.Room.TimeAfterTime.126bpm_01.wav",
        "11.Overhead Hat.TimeAfterTime.126bpm_01.wav",
        "12.Overhead Ride.TimeAfterTime.126bpm_01.wav",
        "13.Room.Left.TimeAfterTime.126bpm_01.wav",
        "14.Room.Right.TimeAfterTime.126bpm_01.wav",
        "15.SHAKER.Left_01.wav",
        "16.SHAKER.Right_01.wav",
        "17.Click.Print_01.wav",
        "18.Time After Time Presonus HD8 Mix_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Time After Time ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

