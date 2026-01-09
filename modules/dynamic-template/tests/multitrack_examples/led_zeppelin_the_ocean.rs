use dynamic_template::*;

#[test]
fn led_zeppelin_the_ocean() {
    // Track list from "Led Zeppelin - The Ocean"
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

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // TODO: Add expected structure once provided
}
