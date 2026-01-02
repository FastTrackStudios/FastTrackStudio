use dynamic_template::*;

#[test]
fn mars_sws() {
    // Track list from "Mars - SWS"
    let items = vec![
        "Bass Synth.L.wav",
        "Bass Synth.R.wav",
        "Bass Synth.wav",
        "Bass Synth.wav",
        "Bass Synth.wav",
        "BGV1_SUM.wav",
        "BGV1_SUM.wav",
        "BGV1A.wav",
        "BGV1A.wav",
        "BGV1A.wav",
        "BGV1A.wav",
        "BGV1B.wav",
        "BGV1B.wav",
        "BGV1B.wav",
        "BGV1B.wav",
        "BGV1C.wav",
        "BGV1C.wav",
        "BGV1C.wav",
        "BGV1C.wav",
        "BGV1D.wav",
        "BGV1D.wav",
        "BGV1D.wav",
        "BGV1D.wav",
        "BGV2_SUM.wav",
        "BGV2_SUM.wav",
        "BGV2A.wav",
        "BGV2A.wav",
        "BGV2A.wav",
        "BGV2A.wav",
        "BGV2B.wav",
        "BGV2B.wav",
        "BGV2B.wav",
        "BGV2B.wav",
        "BGV2C.wav",
        "BGV2C.wav",
        "BGV2C.wav",
        "BGV2C.wav",
        "BGV2D.wav",
        "BGV2D.wav",
        "BGV2D.wav",
        "BGV2D.wav",
        "BGV3_SUM.wav",
        "BGV3_SUM.wav",
        "BGV3A.wav",
        "BGV3A.wav",
        "BGV3A.wav",
        "BGV3A.wav",
        "BGV3B.wav",
        "BGV3B.wav",
        "BGV3B.wav",
        "BGV3B.wav",
        "BGV3C.wav",
        "BGV3C.wav",
        "BGV3C.wav",
        "BGV3C.wav",
        "BGV3D.wav",
        "BGV3D.wav",
        "BGV3D.wav",
        "BGV3D.wav",
        "BGV4_SUM.wav",
        "BGV4_SUM.wav",
        "BGV4A.wav",
        "BGV4A.wav",
        "BGV4A.wav",
        "BGV4A.wav",
        "BGV4B.wav",
        "BGV4B.wav",
        "BGV4B.wav",
        "BGV4B.wav",
        "BGV4C.wav",
        "BGV4C.wav",
        "BGV4C.wav",
        "BGV4C.wav",
        "BGV4D.wav",
        "BGV4D.wav",
        "BGV4D.wav",
        "BGV4D.wav",
        "Crystalizer_Print_1.wav",
        "Crystalizer_Print.wav",
        "Crystalizer_Print.wav",
        "Kim VOX_1.wav",
        "Kim VOX_2.wav",
        "Kim VOX_2.wav",
        "Kim VOX_SUM.wav",
        "Kim VOX_SUM.wav",
        "Kim VOX.wav",
        "Kim VOX.wav",
        "Kim VOX.wav",
        "Mars Kim vx.wav",
        "Mars_PLAP.wav",
        "Piano L.wav",
        "Piano L.wav",
        "Piano L.wav",
        "Piano L.wav",
        "Piano R.wav",
        "Piano R.wav",
        "Piano R.wav",
        "Piano R.wav",
        "Piano Room Mono.wav",
        "Piano Room Mono.wav",
        "Piano Room Mono.wav",
        "Piano Room Mono.wav",
        "Piano_Rough_76bpm.wav",
        "Piano_SUM.wav",
        "Piano_SUM.wav",
        "Reverse Piano.wav",
        "Reverse Piano.wav",
        "Reverse Piano.wav",
        "Reverse Piano.wav",
        "Steve VOX_SUM.wav",
        "Steve VOX_SUM.wav",
        "Steve VOX.wav",
        "Steve VOX.wav",
        "Steve VOX.wav",
        "Steve VOX.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
    
    // ============================================================================
    // SUGGESTED TRACK HIERARCHY
    // ============================================================================
    // 
    // Bass/
    //   └─ Synth/
    //       ├─ L              ← Bass Synth.L.wav
    //       ├─ R              ← Bass Synth.R.wav  
    //       └─ Main           ← Bass Synth.wav (x3 duplicates)
    //
    // Keys/
    //   └─ Piano/
    //       ├─ L              ← Piano L.wav (x4 duplicates)
    //       ├─ R              ← Piano R.wav (x4 duplicates)
    //       ├─ Room Mono      ← Piano Room Mono.wav (x4 duplicates)
    //       ├─ Rough          ← Piano_Rough_76bpm.wav
    //       ├─ SUM            ← Piano_SUM.wav (x2 duplicates)
    //       └─ Reverse        ← Reverse Piano.wav (x4 duplicates)
    //
    // Vocals/
    //   ├─ Lead Vocals/
    //   │   ├─ Kim/
    //   │   │   ├─ 1          ← Kim VOX_1.wav
    //   │   │   ├─ 2          ← Kim VOX_2.wav (x2 duplicates)
    //   │   │   ├─ SUM        ← Kim VOX_SUM.wav (x2 duplicates)
    //   │   │   └─ Main       ← Kim VOX.wav (x3 duplicates)
    //   │   ├─ Mars Kim       ← Mars Kim vx.wav
    //   │   └─ Steve/
    //   │       ├─ SUM        ← Steve VOX_SUM.wav (x2 duplicates)
    //   │       └─ Main       ← Steve VOX.wav (x4 duplicates)
    //   │
    //   └─ BGVs/
    //       ├─ BGV1/
    //       │   ├─ A          ← BGV1A.wav (x4 duplicates)
    //       │   ├─ B          ← BGV1B.wav (x4 duplicates)
    //       │   ├─ C          ← BGV1C.wav (x4 duplicates)
    //       │   ├─ D          ← BGV1D.wav (x4 duplicates)
    //       │   └─ SUM        ← BGV1_SUM.wav (x2 duplicates)
    //       ├─ BGV2/
    //       │   ├─ A          ← BGV2A.wav (x4 duplicates)
    //       │   ├─ B          ← BGV2B.wav (x4 duplicates)
    //       │   ├─ C          ← BGV2C.wav (x4 duplicates)
    //       │   ├─ D          ← BGV2D.wav (x4 duplicates)
    //       │   └─ SUM        ← BGV2_SUM.wav (x2 duplicates)
    //       ├─ BGV3/
    //       │   ├─ A          ← BGV3A.wav (x4 duplicates)
    //       │   ├─ B          ← BGV3B.wav (x4 duplicates)
    //       │   ├─ C          ← BGV3C.wav (x4 duplicates)
    //       │   ├─ D          ← BGV3D.wav (x4 duplicates)
    //       │   └─ SUM        ← BGV3_SUM.wav (x2 duplicates)
    //       └─ BGV4/
    //           ├─ A          ← BGV4A.wav (x4 duplicates)
    //           ├─ B          ← BGV4B.wav (x4 duplicates)
    //           ├─ C          ← BGV4C.wav (x4 duplicates)
    //           ├─ D          ← BGV4D.wav (x4 duplicates)
    //           └─ SUM        ← BGV4_SUM.wav (x2 duplicates)
    //
    // Effects/ (or unmatched)
    //   └─ Crystalizer/
    //       ├─ Print          ← Crystalizer_Print.wav (x2 duplicates)
    //       └─ Print 1        ← Crystalizer_Print_1.wav
    //
    // Unmatched/
    //   └─ Mars_PLAP          ← Mars_PLAP.wav (likely a print/mix reference)
    //
    // ============================================================================
    // NOTES:
    // - BGV1-4 appear to be 4 harmony parts (BGV1, BGV2, BGV3, BGV4)
    // - A/B/C/D appear to be layers or takes within each harmony part
    // - SUM tracks are likely submix/bus prints
    // - The duplicate counts suggest multiple takes or playlist items
    // - "Kim" and "Steve" are performers (likely Kim and Steve Maggiora based on other tracks)
    // - "Mars Kim vx" may be a specific vocal arrangement/section
    // - Crystalizer is an effect (Soundtoys plugin)
    // - PLAP likely means "Play Along Print" or similar reference mix
    // ============================================================================
}
