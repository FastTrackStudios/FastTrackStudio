//! Test for The Ocean Cover Multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_the_ocean_cover() {
    let track_names = vec![
        "01.Kick_01.wav",
        "02.SNR_01.wav",
        "03. OHL_01.wav",
        "04. OHR_01.wav",
        "05. Room_01 L.wav",
        "06. Room_01 R.wav",
        "07. Bass DI_01.wav",
        "08. Bass ReAmp_01.wav",
        "09. Gtr L Main Riff.A1_01.wav",
        "10. Gtr L Main Riff.A2_01.wav",
        "11. Gtr_L.A1_01.wav",
        "12. Gtr_L.A2_01.wav",
        "13. Gtr_R.A1_01.wav",
        "14. Gtr_R.A2_01.wav",
        "15. lead_1.A1_01.wav",
        "16. lead_1.A2_01.wav",
        "17. lead_2.A1_01.wav",
        "18. lead_2.A2_01.wav",
        "19. lead_Vox_01.wav",
        "20. LA LA Voc 1_01.wav",
        "21. LA LA Voc 2_01.wav",
        "22. Vocal Adlib_01.wav",
        "23. Outro Vocal 1_01.wav",
        "24. Outro Vocal 2_01.wav",
        "25. Outro Vocal 3_01.wav",
        "26. Outro Vocal 4_01.wav",
        "27. Outro Vocal 5_01.wav",
        "28. The Ocean_01.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing The Ocean Cover ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

