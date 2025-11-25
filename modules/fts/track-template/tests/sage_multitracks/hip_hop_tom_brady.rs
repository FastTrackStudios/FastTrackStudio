//! Test for Hip Hop - Tom Brady multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_hip_hop_tom_brady() {
    let track_names = vec![
        "808 - Tom Brady.wav",
        "_Full Master - Tom Brady.wav",
        "Beat 1 - Tom Brady.wav",
        "Carnival - Tom Brady.wav",
        "Clap - Tom Brady.wav",
        "Hat Loop 4 - Tom Brady.wav",
        "Kick - Tom Brady.wav",
        "Melody 2 - Tom Brady.wav",
        "Perc - Tom Brady.wav",
        "Snare - Tom Brady.wav",
        "TAG - Tom Brady.wav",
        "Vox 1 adlib 2_1 - Tom Brady.wav",
        "Vox 1 adlib 3_1 - Tom Brady.wav",
        "Vox 1 ADLIB 4_1 - Tom Brady.wav",
        "Vox 1 CHORUS 1_1 - Tom Brady.wav",
        "Vox 1 CHORUS 2_1 - Tom Brady.wav",
        "Vox 1 chorus ADLIBS_1 - Tom Brady.wav",
        "Vox 1 CHORUS GRIT_1 - Tom Brady.wav",
        "Vox 1 MAIN - Tom Brady.wav",
        "Vox 1 MAIN 2_1 - Tom Brady.wav",
        "Vox 1 MAIN_2 - Tom Brady.wav",
        "Vox 2 ADLIB 1_1 - Tom Brady.wav",
        "Vox 2 ADLIB 2_1 - Tom Brady.wav",
        "Vox 2 Chorus 1_1 - Tom Brady.wav",
        "Vox 2 Chorus 2_1 - Tom Brady.wav",
        "Vox 2 MAIN_1 - Tom Brady.wav",
        "Yessss - Tom Brady.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Hip Hop - Tom Brady ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

