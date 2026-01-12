use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn led_zeppelin_the_ocean() -> Result<()> {
    // -- Setup & Fixtures
    // Led Zeppelin - The Ocean: 28-track classic rock session with multi-tracked guitars and layered vocals
    let items = vec![
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
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Expected structure:
    // Drums/
    //   ├─ Kick               ← 01.Kick_01.wav
    //   ├─ Snare              ← 02.SNR_01.wav
    //   └─ Rooms/
    //       ├─ L              ← 05. Room_01 L.wav
    //       └─ R              ← 06. Room_01 R.wav
    // Bass/
    //   ├─ Bass               ← 07. Bass DI_01.wav (DI)
    //   └─ Amp                ← 08. Bass ReAmp_01.wav (ReAmp)
    // Guitars/
    //   ├─ 09. L Main Riff.A1 ← 09. Gtr L Main Riff.A1_01.wav
    //   ├─ 10. L Main Riff.A2 ← 10. Gtr L Main Riff.A2_01.wav
    //   ├─ 11. Gtr_L.A1       ← 11. Gtr_L.A1_01.wav
    //   ├─ 12. Gtr_L.A2       ← 12. Gtr_L.A2_01.wav
    //   ├─ 13. Gtr_R.A1       ← 13. Gtr_R.A1_01.wav
    //   └─ 14. Gtr_R.A2       ← 14. Gtr_R.A2_01.wav
    // Vocals/
    //   ├─ Lead/
    //   │   ├─ Outro/
    //   │   │   ├─ Outro 1    ← 23. Outro Vocal 1_01.wav
    //   │   │   ├─ Outro 2    ← 24. Outro Vocal 2_01.wav
    //   │   │   ├─ Outro 3    ← 25. Outro Vocal 3_01.wav
    //   │   │   ├─ Outro 4    ← 26. Outro Vocal 4_01.wav
    //   │   │   └─ Outro 5    ← 27. Outro Vocal 5_01.wav
    //   │   └─ Lead/
    //   │       ├─ Lead 1     ← 19. lead_Vox_01.wav
    //   │       └─ Lead 2     ← 22. Vocal Adlib_01.wav
    //   ├─ 20. LA LA Voc 1    ← 20. LA LA Voc 1_01.wav
    //   └─ 21. LA LA Voc 2    ← 21. LA LA Voc 2_01.wav
    // Unsorted/
    //   ├─ 03. OHL            ← 03. OHL_01.wav (overhead L - not recognized)
    //   ├─ 04. OHR            ← 04. OHR_01.wav (overhead R - not recognized)
    //   ├─ 15. Lead_1.A1      ← 15. lead_1.A1_01.wav (guitar lead - ambiguous)
    //   ├─ 16. Lead_1.A2      ← 16. lead_1.A2_01.wav
    //   ├─ 17. Lead_2.A1      ← 17. lead_2.A1_01.wav
    //   ├─ 18. Lead_2.A2      ← 18. lead_2.A2_01.wav
    //   └─ 28. The Ocean      ← 28. The Ocean_01.wav (mix reference)
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .track("Kick", "01.Kick_01.wav")
            .track("Snare", "02.SNR_01.wav")
            .folder("Rooms")
                .track("L", "05. Room_01 L.wav")
                .track("R", "06. Room_01 R.wav")
            .end()
        .end()
        .folder("Bass")
            .track("Bass", "07. Bass DI_01.wav")
            .track("Amp", "08. Bass ReAmp_01.wav")
        .end()
        .folder("Guitars")
            .track("09. L Main Riff.A1", "09. Gtr L Main Riff.A1_01.wav")
            .track("10. L Main Riff.A2", "10. Gtr L Main Riff.A2_01.wav")
            .track("11. Gtr_L.A1", "11. Gtr_L.A1_01.wav")
            .track("12. Gtr_L.A2", "12. Gtr_L.A2_01.wav")
            .track("13. Gtr_R.A1", "13. Gtr_R.A1_01.wav")
            .track("14. Gtr_R.A2", "14. Gtr_R.A2_01.wav")
        .end()
        .folder("Vocals")
            .folder("Lead")
                .folder("Outro")
                    .track("Outro 1", "23. Outro Vocal 1_01.wav")
                    .track("Outro 2", "24. Outro Vocal 2_01.wav")
                    .track("Outro 3", "25. Outro Vocal 3_01.wav")
                    .track("Outro 4", "26. Outro Vocal 4_01.wav")
                    .track("Outro 5", "27. Outro Vocal 5_01.wav")
                .end()
                .folder("Lead")
                    .track("Lead 1", "19. lead_Vox_01.wav")
                    .track("Lead 2", "22. Vocal Adlib_01.wav")
                .end()
            .end()
            .track("20. LA LA Voc 1", "20. LA LA Voc 1_01.wav")
            .track("21. LA LA Voc 2", "21. LA LA Voc 2_01.wav")
        .end()
        .folder("Unsorted")
            .track("03. OHL", "03. OHL_01.wav")
            .track("04. OHR", "04. OHR_01.wav")
            .track("15. Lead_1.A1", "15. lead_1.A1_01.wav")
            .track("16. Lead_1.A2", "16. lead_1.A2_01.wav")
            .track("17. Lead_2.A1", "17. lead_2.A1_01.wav")
            .track("18. Lead_2.A2", "18. lead_2.A2_01.wav")
            .track("28. The Ocean", "28. The Ocean_01.wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
