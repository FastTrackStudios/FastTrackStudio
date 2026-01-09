use dynamic_template::*;

#[test]
fn laura_clapp_davidson_hey_hey_sister() {
    // Track list from "Laura Clapp Davidson - Hey Hey Sister"
    let items = vec![
        "01.KICK_01.wav",
        "02.SNARE_01.wav",
        "03.HH_01.wav",
        "04.OHS_01.wav",
        "05.ROOM_01.wav",
        "06.Hey_Sister_Bass DI_01.wav",
        "07.Hey_Sister_Bass_Tone_X_01.wav",
        "08.Hey_Sister_Acoustic_L_01.wav",
        "09.Hey_Sister_Acoustic_R_01.wav",
        "10.Hey_Sister_Gtr_1_L_01.wav",
        "11.Hey_Sister_Gtr_2_R_01.wav",
        "12.Hey_Sister_Licks_01.wav",
        "13.Hey_Sister_Solo_01.wav",
        "14.Laura Lead Vocal_01.wav",
        "15.Laura Verse Harmony_01.wav",
        "16.Laura Chorus Double_01.wav",
        "17.Laura Chorus Harmony 1_01.wav",
        "18.Laura Chorus Harmony 2_01.wav",
        "19.Laura Hey Hey High 1_01.wav",
        "20.Laura Hey Hey High 2_01.wav",
        "21.Laura Hey Hey Highest 1_01.wav",
        "22.Laura Hey Hey Highest 2_01.wav",
        "23.Laura Hey Hey Low 1_01.wav",
        "24.Laura Hey Hey Low 2_01.wav",
        "25.Laura Hey Hey Mid 1_01.wav",
        "26.Laura Hey Hey Mid 2_01.wav",
        "27. Hey Hey Sister_01.wav",
    ];

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // TODO: Add expected structure once provided
}
