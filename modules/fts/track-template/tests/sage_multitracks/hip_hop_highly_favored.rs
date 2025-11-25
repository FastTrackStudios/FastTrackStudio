//! Test for Hip Hop - Highly Favored multitracks
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_hip_hop_highly_favored() {
    let track_names = vec![
        "808_bip_2 - Highly Favored.wav",
        "808_bip_3 - Highly Favored.wav",
        "_Full Master - Highly Favored.wav",
        "_ Full Mix - Highly Favored.wav",
        "Adlibs_bip - Highly Favored.wav",
        "Beat filter_bip - Highly Favored.wav",
        "Bip - Highly Favored.wav",
        "Choir 1_bip - Highly Favored.wav",
        "Choir 2_bip - Highly Favored.wav",
        "Choir 3_bip - Highly Favored.wav",
        "Choir 4_bip - Highly Favored.wav",
        "Choir 5_bip - Highly Favored.wav",
        "Choir 6_bip - Highly Favored.wav",
        "Clap_bip - Highly Favored.wav",
        "Drums outro_1_bip - Highly Favored.wav",
        "Drum stop_1_bip - Highly Favored.wav",
        "FX (RISER)-24b_bip - Highly Favored.wav",
        "HF JT FEAT STEM 1_bip - Highly Favored.wav",
        "HF JT FEAT STEM 2_bip - Highly Favored.wav",
        "HF JT FEAT STEM 3_bip - Highly Favored.wav",
        "HF JT FEAT STEM 4_bip - Highly Favored.wav",
        "Hihat_bip - Highly Favored.wav",
        "Hook 2_bip - Highly Favored.wav",
        "Hook_bip - Highly Favored.wav",
        "Hook stack 2_bip - Highly Favored.wav",
        "Hook stack_bip - Highly Favored.wav",
        "Kick_bip_2 - Highly Favored.wav",
        "Kick_bip_3 - Highly Favored.wav",
        "Open hat_bip - Highly Favored.wav",
        "Outro_bip - Highly Favored.wav",
        "Perc_bip - Highly Favored.wav",
        "Perc_bip_1 - Highly Favored.wav",
        "Snare_bip - Highly Favored.wav",
        "Supa hot fire - Ohhh_bip - Highly Favored.wav",
        "Symbolyc One - HAT LOOP - Highly Favored.wav",
        "Verse 1.1_bip - Highly Favored.wav",
        "Verse 1.2_bip - Highly Favored.wav",
        "Verse 1.3_bip - Highly Favored.wav",
        "Verse 1.3 stack_bip - Highly Favored.wav",
        "Verse 2.1_bip - Highly Favored.wav",
        "Verse 2.2_bip - Highly Favored.wav",
        "Verse 2.3_bip - Highly Favored.wav",
        "Verse 2.3 low_bip - Highly Favored.wav",
        "Vox chop_bip_2 - Highly Favored.wav",
        "Vox chop_bip_3 - Highly Favored.wav",
        "Vox chop low_bip_2 -  Highly Favored.wav",
        "Vox chop low_bip_3 - Highly Favored.wav",
        "Vox chop outro_bip - Highly Favored.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Hip Hop - Highly Favored ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

