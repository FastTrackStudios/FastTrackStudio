//! Test for Radiohead - Paranoid Android Cover
//!
//! TODO: Add expected track structure and matching verification

use naming_convention::{SimpleParser, create_default_groups, format_track_name_default};

#[test]
fn test_radiohead_paranoid_android() {
    let track_names = vec![
        "1 Kick In.05_04.wav",
        "10 Rack 10-St.01.R.05_03.wav",
        "11  Rack 12.01.R.05_03.wav",
        "12 Floor.01.R.05_03.wav",
        "13 Knee Mic.01.R.05_03.wav",
        "14 Mono Ovh.01.R.05_03.wav",
        "15 Room HH.05_03.wav",
        "19 Cabasa_03.wav",
        "20 Clave_03.wav",
        "21 Guiro Shaker_03.wav",
        "22 Guiro_03.wav",
        "23 Vibraslap_03.wav",
        "24 SHAKER_03.wav",
        "25 _Bass.01_03.wav",
        "26 Acc Guitar_03.wav",
        "28 EdCrunch_03.wav",
        "29 JohnyCrunch1_03.wav",
        "3 Kick Out.05_04.wav",
        "30 EdCrunch2_03.wav",
        "31 JohnyCrunch2_03.wav",
        "33 EdPitch_03.wav",
        "34 JohnyLead_03.wav",
        "35 JohnyPhaser1_03.wav",
        "36 JohnyPhaser1.dup1_03.wav",
        "39 JohnyPhaser2_03.wav",
        "40 CK MAP]_03.wav",
        "42 intro count.1_03.wav",
        "43 Robot Voice_03.wav",
        "44 Bells.2_03.wav",
        "45 DX7 .2_03.wav",
        "46 FX1.2_03.wav",
        "47 FX2.2_03.wav",
        "48 Mellotron.2_03.wav",
        "49 Organ Chords.2_03.wav",
        "5 Snare.05_03.wav",
        "50 Organ Notes.2_03.wav",
        "51 Organ Slide.2_03.wav",
        "52 Piano_03.wav",
        "53 rhodes_david bennett_03.wav",
        "54 prophet synth_david bennett_03.wav",
        "56 Lead Voc_03.wav",
        "57 Lead Voc Dbl_03.wav",
        "58 Lead Voc Dbl.dup1_03.wav",
        "59 Vocal 3_03.wav",
        "6 Snare.dup1.05_03.wav",
        "60 lead vox quad_03.wav",
        "61 Bridge vocal extra_03.wav",
        "62 Outro vocal 1_03.wav",
        "63 Outro vocal 2_03.wav",
        "64 Outro vocal 3_03.wav",
        "65 extra vocal2_03.wav",
        "66 extra vocal3_03.wav",
        "67 Voca Middle Bridge1_03.wav",
        "68 Voca Middle Bridge2_03.wav",
        "69 Voca Middle Bridge3_03.wav",
        "70 Voca Middle Bridge4_03.wav",
        "71 Voca Middle Bridge5_03.wav",
        "8 Snare Btm.05_03.wav",
        "9 SNR VERB.05_03.wav",
        "Paranoid_Android_Cover_PLP_JH_MIX_1_Master.wav",
    ];

    let parser = SimpleParser::new(create_default_groups());
    
    println!("\n=== Parsing Radiohead - Paranoid Android ===\n");
    
    for name in &track_names {
        let parsed = parser.parse(name);
        let formatted = format_track_name_default(&parsed);
        println!("{} -> {}", name, formatted);
    }
    
    // TODO: Build expected track structure
    // TODO: Create matcher and verify matches
    assert!(!track_names.is_empty(), "Should have track names to test");
}

