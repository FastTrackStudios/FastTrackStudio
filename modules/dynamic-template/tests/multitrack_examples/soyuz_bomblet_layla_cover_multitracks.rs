use dynamic_template::*;

#[test]
fn soyuz_bomblet_layla_cover_multitracks() {
    // Track list from "Soyuz Bomblet Layla Cover Multitracks"
    let items = vec![
        "01. Drums_01.wav",
        "02.Bass DI_01.wav",
        "03.Bass Amp_01.wav",
        "04.Rhythm Gtr DI_02.wav",
        "05.Rhythm Gtr Amp_02.wav",
        "06.Rhythm Gtr DI Dbl_02.wav",
        "07.Rhythm Gtr Amp Dbl_02.wav",
        "08.Lead Harm Gtr DI_02.wav",
        "09.Lead Harm Gtr Amp_02.wav",
        "10.Lead Harm Gtr DI Dbl_02.wav",
        "11.Lead Harm Gtr Amp Dbl_02.wav",
        "12. Lead Gtr DI_02.wav",
        "13. Lead Gtr Amp_02.wav",
        "14. Lead Gtr DI Dbl_02.wav",
        "15. Lead Gtr Amp Dbl_02.wav",
        "16. Solo Gtr Amp_02.wav",
        "16. Solo Gtr DI_02.wav",
        "17. Outro Gtr DI_01.wav",
        "18. Outro Gtr Amp_01.wav",
        "19. Outro Gtr DI Dbl_01.wav",
        "20. Outro Gtr Amp Dbl_01.wav",
        "21. Acoustic_01.wav",
        "22. Acoustic Dbl_01.wav",
        "23.Acoustic Lead Line_01.wav",
        "24.Acoustic Lead Line Dbl_01.wav",
        "25.Acoustic Outro_01.wav",
        "26. Piano_01.wav",
        "27. Vocal_02.wav",
        "28.BG A Harm 1_02.wav",
        "29.BG A Harm 2_02.wav",
        "30.BG A Harm 3_02.wav",
        "31.BG A Harm 4_02.wav",
        "32.BG B Harm 1_02.wav",
        "33.BG B Harm 2_02.wav",
        "34.BG B Harm 3_02.wav",
        "35.BG B Harm 4_02.wav",
        "36.BG C Harm 1_02.wav",
        "37.BG C Harm 2_02.wav",
        "38.BG C Harm 3_02.wav",
        "39.BG C Harm 4_02.wav",
        "40.BG D Harm 1_02.wav",
        "41.BG D Harm 2_02.wav",
        "42.BG D Harm 3_02.wav",
        "43.BG D Harm 4_02.wav",
        "44.BG D Harm 5_02.wav",
        "45.BG D Harm 6_02.wav",
        "46.BG D Harm 7_02.wav",
        "47.BG D Harm 8_02.wav",
        "48.BG E Harm 1_02.wav",
        "49.BG E Harm 2_02.wav",
        "50.BG E Harm 3_02.wav",
        "51.BG E Harm 4_02.wav",
        "52.Soyuz Bomblet Layla Cover Mix_01.L.wav",
        "52.Soyuz Bomblet Layla Cover Mix_01.R.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
