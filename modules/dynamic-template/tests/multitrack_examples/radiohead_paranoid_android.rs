use dynamic_template::*;
use daw::tracks::{TrackStructureBuilder, assert_tracks_equal, Track};
use monarchy::{monarchy_sort, move_unsorted_to_group, reapply_collapse, expand_items_to_children};

/// Count all items in a flat track list
fn count_items(tracks: &[Track]) -> usize {
    tracks.iter().map(|t| t.items.len()).sum()
}

#[test]
fn radiohead_paranoid_android() {
    // Track list from "Radiohead - Paranoid Android"
    let items = vec![
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
    
    // Step 1: Initial sort using monarchy
    let config = default_config();
    let mut structure = monarchy_sort(items, config.clone()).unwrap();
    
    println!("\n=== STEP 1: Initial monarchy_sort ===");
    println!("{}", structure);
    
    // Step 2: Move Ed/Johny guitar tracks from Unsorted to Electric Guitar
    // In a real app, this would be triggered by user input: "These are electric guitar tracks"
    println!("\n=== STEP 2: Moving guitar tracks from Unsorted to Electric Guitar ===");
    let guitar_sorted = move_unsorted_to_group(
        &mut structure,
        &config,
        &["Unsorted"],
        "Electric",
        &["Guitars"],
    ).unwrap();
    println!("Sorted {} guitar items", guitar_sorted);
    
    // Note: FX1/FX2 now automatically go to SFX group (no manual step needed)
    
    // Step 3: Re-apply collapse to clean up hierarchy
    reapply_collapse(&mut structure, &config);
    
    // Step 4: Expand items to individual child tracks
    // This ensures each item becomes its own track (one item per track rule)
    expand_items_to_children(&mut structure);
    
    println!("\n=== STEP 4: Final structure after expanding items to tracks ===");
    println!("{}", structure);
    
    // Convert to tracks for display
    let tracks = structure.clone().to_tracks();
    
    println!("\n=== Final Track list ===");
    daw::tracks::display_tracklist(&tracks);
    
    // Expected structure based on hierarchy conventions
    let expected = TrackStructureBuilder::new()
        // Drums
        .folder("Drums")
            .folder("Kick")
                .track("In", "1 Kick In.05_04.wav")
                .track("Out", "3 Kick Out.05_04.wav")
            .end()
            .folder("Snare")
                .folder("Sum")
                    .track("Top", "5 Snare.05_03.wav")
                    .track("Top 2", "6 Snare.dup1.05_03.wav")
                    .track("Bottom", "8 Snare Btm.05_03.wav")
                .end()
                .track("Verb", "9 SNR VERB.05_03.wav")
            .end()
            .folder("Toms")
                .track("Rack 10", "10 Rack 10-St.01.R.05_03.wav")
                .track("Rack 12", "11  Rack 12.01.R.05_03.wav")
                .track("Floor", "12 Floor.01.R.05_03.wav")
            .end()
            .folder("Cymbals")
                .track("OH Mono", "14 Mono Ovh.01.R.05_03.wav")
                .track("HH", "15 Room HH.05_03.wav")
            .end()
            .folder("Rooms")
                .track("Knee Mic", "13 Knee Mic.01.R.05_03.wav")
            .end()
        .end()
        // Percussion
        .folder("Percussion")
            .track("Cabasa", "19 Cabasa_03.wav")
            .track("Clave", "20 Clave_03.wav")
            .track("Guiro Shaker", "21 Guiro Shaker_03.wav")
            .track("Guiro", "22 Guiro_03.wav")
            .track("Vibraslap", "23 Vibraslap_03.wav")
            .track("Shaker", "24 SHAKER_03.wav")
        .end()
        // Bass (single item, no subfolder)
        .track("Bass", "25 _Bass.01_03.wav")
        // Guitars
        .folder("Guitars")
            .track("Acoustic", "26 Acc Guitar_03.wav")
            .folder("Electric")
                .folder("Ed")
                    .track("Crunch", "28 EdCrunch_03.wav")
                    .track("Crunch 2", "30 EdCrunch2_03.wav")
                    .track("Pitch", "33 EdPitch_03.wav")
                .end()
                .folder("Johny")
                    .track("Crunch 1", "29 JohnyCrunch1_03.wav")
                    .track("Crunch 2", "31 JohnyCrunch2_03.wav")
                    .track("Lead", "34 JohnyLead_03.wav")
                    .track("Phaser 1", "35 JohnyPhaser1_03.wav")
                    .track("Phaser 1 2", "36 JohnyPhaser1.dup1_03.wav")
                    .track("Phaser 2", "39 JohnyPhaser2_03.wav")
                .end()
            .end()
        .end()
        // Keys
        .folder("Keys")
            .track("Piano", "52 Piano_03.wav")
            .folder("Electric")
                .track("Rhodes", "53 rhodes_david bennett_03.wav")
                .track("DX7", "45 DX7 .2_03.wav")
                .track("Mellotron", "48 Mellotron.2_03.wav")
            .end()
            .folder("Organ")
                .track("Chords", "49 Organ Chords.2_03.wav")
                .track("Notes", "50 Organ Notes.2_03.wav")
                .track("Slide", "51 Organ Slide.2_03.wav")
            .end()
        .end()
        // Synths
        .folder("Synths")
            .track("Prophet", "54 prophet synth_david bennett_03.wav")
            .track("Bells", "44 Bells.2_03.wav")
        .end()
        // Vocals
        .folder("Vocals")
            .folder("Main")
                .folder("Lead")
                    .track("Main", "56 Lead Voc_03.wav")
                    .track("Dbl", "57 Lead Voc Dbl_03.wav")
                    .track("Dbl 2", "58 Lead Voc Dbl.dup1_03.wav")
                .end()
                .track("Vocal 3", "59 Vocal 3_03.wav")
                .track("Vocal Quad", "60 lead vox quad_03.wav")
                .track("Extra 2", "65 extra vocal2_03.wav")
                .track("Extra 3", "66 extra vocal3_03.wav")
            .end()
            .folder("Bridge")
                .track("Main", "61 Bridge vocal extra_03.wav")
                .track("1", "67 Voca Middle Bridge1_03.wav")
                .track("2", "68 Voca Middle Bridge2_03.wav")
                .track("3", "69 Voca Middle Bridge3_03.wav")
                .track("4", "70 Voca Middle Bridge4_03.wav")
                .track("5", "71 Voca Middle Bridge5_03.wav")
            .end()
            .folder("Outro")
                .track("1", "62 Outro vocal 1_03.wav")
                .track("2", "63 Outro vocal 2_03.wav")
                .track("3", "64 Outro vocal 3_03.wav")
            .end()
        .end()
        // SFX
        .folder("SFX")
            .track("FX1", "46 FX1.2_03.wav")
            .track("FX2", "47 FX2.2_03.wav")
            .track("Robot Voice", "43 Robot Voice_03.wav")
            .track("Intro Count", "42 intro count.1_03.wav")
        .end()
        // Unmatched
        .folder("Unmatched")
            .track("CK MAP", "40 CK MAP]_03.wav")
            .track("Mix Master", "Paranoid_Android_Cover_PLP_JH_MIX_1_Master.wav")
        .end()
        .build();
    
    // Full structure assertion
    assert_tracks_equal(&tracks, &expected).unwrap();
    
    // ============================================================================
    // EXPECTED TRACK HIERARCHY (documented version)
    // ============================================================================
    //
    // Drums/
    //   ├─ Kick/
    //   │   ├─ In             ← 1 Kick In.05_04.wav
    //   │   └─ Out            ← 3 Kick Out.05_04.wav
    //   ├─ Snare/
    //   │   ├─ Sum/                           (tagged collection for mic positions)
    //   │   │   ├─ Top        ← 5 Snare.05_03.wav (default = Top when Bottom exists)
    //   │   │   ├─ Top 2      ← 6 Snare.dup1.05_03.wav
    //   │   │   └─ Bottom     ← 8 Snare Btm.05_03.wav
    //   │   └─ Verb           ← 9 SNR VERB.05_03.wav (FX print, sibling to Sum)
    //   ├─ Toms/
    //   │   ├─ Rack 10        ← 10 Rack 10-St.01.R.05_03.wav (10" rack tom)
    //   │   ├─ Rack 12        ← 11 Rack 12.01.R.05_03.wav (12" rack tom)
    //   │   └─ Floor          ← 12 Floor.01.R.05_03.wav
    //   ├─ Cymbals/
    //   │   ├─ OH Mono        ← 14 Mono Ovh.01.R.05_03.wav (OVH → Cymbals)
    //   │   └─ HH             ← 15 Room HH.05_03.wav (HH → Cymbals, despite "Room" prefix)
    //   └─ Rooms/
    //       └─ Knee Mic       ← 13 Knee Mic.01.R.05_03.wav
    //
    // Percussion/
    //   ├─ Cabasa             ← 19 Cabasa_03.wav
    //   ├─ Clave              ← 20 Clave_03.wav
    //   ├─ Guiro Shaker       ← 21 Guiro Shaker_03.wav
    //   ├─ Guiro              ← 22 Guiro_03.wav
    //   ├─ Vibraslap          ← 23 Vibraslap_03.wav
    //   └─ Shaker             ← 24 SHAKER_03.wav
    //
    // Bass                     ← 25 _Bass.01_03.wav (single item, no subfolder needed)
    //
    // Guitars/
    //   ├─ Acoustic/
    //   │   └─ Main           ← 26 Acc Guitar_03.wav
    //   └─ Electric/
    //       ├─ Ed/
    //       │   ├─ Crunch     ← 28 EdCrunch_03.wav
    //       │   ├─ Crunch 2   ← 30 EdCrunch2_03.wav
    //       │   └─ Pitch      ← 33 EdPitch_03.wav
    //       └─ Johny/
    //           ├─ Crunch 1   ← 29 JohnyCrunch1_03.wav
    //           ├─ Crunch 2   ← 31 JohnyCrunch2_03.wav
    //           ├─ Lead       ← 34 JohnyLead_03.wav
    //           ├─ Phaser 1   ← 35 JohnyPhaser1_03.wav
    //           ├─ Phaser 1 2 ← 36 JohnyPhaser1.dup1_03.wav (dup = layer 2)
    //           └─ Phaser 2   ← 39 JohnyPhaser2_03.wav
    //
    // Keys/
    //   ├─ Piano              ← 52 Piano_03.wav
    //   ├─ Electric/
    //   │   ├─ Rhodes         ← 53 rhodes_david bennett_03.wav
    //   │   ├─ DX7            ← 45 DX7 .2_03.wav
    //   │   └─ Mellotron      ← 48 Mellotron.2_03.wav
    //   └─ Organ/
    //       ├─ Chords         ← 49 Organ Chords.2_03.wav
    //       ├─ Notes          ← 50 Organ Notes.2_03.wav
    //       └─ Slide          ← 51 Organ Slide.2_03.wav
    //
    // Synths/
    //   ├─ Prophet            ← 54 prophet synth_david bennett_03.wav
    //   └─ Bells              ← 44 Bells.2_03.wav
    //
    // Vocals/
    //   ├─ Main/                              (SECTION - main/verse parts)
    //   │   ├─ Lead/                          (arrangement - "Lead Voc" tracks)
    //   │   │   ├─ Main       ← 56 Lead Voc_03.wav
    //   │   │   ├─ Dbl        ← 57 Lead Voc Dbl_03.wav
    //   │   │   └─ Dbl 2      ← 58 Lead Voc Dbl.dup1_03.wav
    //   │   ├─ Vocal 3        ← 59 Vocal 3_03.wav
    //   │   ├─ Vocal Quad     ← 60 lead vox quad_03.wav
    //   │   ├─ Extra 2        ← 65 extra vocal2_03.wav
    //   │   └─ Extra 3        ← 66 extra vocal3_03.wav
    //   ├─ Bridge/                            (SECTION)
    //   │   ├─ Main           ← 61 Bridge vocal extra_03.wav
    //   │   ├─ 1              ← 67 Voca Middle Bridge1_03.wav
    //   │   ├─ 2              ← 68 Voca Middle Bridge2_03.wav
    //   │   ├─ 3              ← 69 Voca Middle Bridge3_03.wav
    //   │   ├─ 4              ← 70 Voca Middle Bridge4_03.wav
    //   │   └─ 5              ← 71 Voca Middle Bridge5_03.wav
    //   └─ Outro/                             (SECTION)
    //       ├─ 1              ← 62 Outro vocal 1_03.wav
    //       ├─ 2              ← 63 Outro vocal 2_03.wav
    //       └─ 3              ← 64 Outro vocal 3_03.wav
    //
    // SFX/
    //   ├─ FX1                ← 46 FX1.2_03.wav
    //   ├─ FX2                ← 47 FX2.2_03.wav
    //   ├─ Robot Voice        ← 43 Robot Voice_03.wav
    //   └─ Intro Count        ← 42 intro count.1_03.wav
    //
    // Unmatched/
    //   ├─ CK MAP             ← 40 CK MAP]_03.wav (likely click/map track)
    //   └─ Mix Master         ← Paranoid_Android_Cover_PLP_JH_MIX_1_Master.wav
    //
    // ============================================================================
    // NOTES:
    // - Track numbers (1-71) are original session order, not sorting priority
    // - "Ed" and "Johny" are guitarists (Ed O'Brien, Jonny Greenwood)
    // - ".dup1" suffix indicates duplicate/layer 2 track
    // - ".2_03" and ".05_03" suffixes are likely Pro Tools region identifiers
    // - Snare default is "Top" (not "Main") when Bottom mic exists
    // - Snare mic positions go in Sum folder, FX prints (Verb) are siblings to Sum
    // - OVH/Overheads and HH/Hi-Hat → Cymbals bus (not Rooms)
    // - Rack tom numbers indicate size (10", 12")
    // - DX7 is a Yamaha FM synthesizer → Electric Keys
    // - Mellotron is a tape-replay keyboard → Electric Keys
    // - Prophet is a Sequential Circuits synthesizer
    // - Vocals organized by Section first (Main, Bridge, Outro)
    // - Within Main section: "Lead" is an arrangement with layers (Main, Dbl, Dbl 2)
    // - Other vocals in Main section (Vocal 3, Quad, Extra) are separate parts
    // - PLP = Play Along Print, JH = initials (mixer?)
    // ============================================================================
}
