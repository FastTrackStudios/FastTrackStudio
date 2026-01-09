use dynamic_template::*;

#[test]
fn tears_for_fears_shout() {
    // Track list from "Tears For Fears - Shout"
    let items = vec![
        "01.Kick.In.One_01-02.wav",
        "02.Kick.Room_01-02.wav",
        "03.Snare Top_01-02.wav",
        "04.Snare Room_01-02.wav",
        "05.Hi Hat Close_01-02.wav",
        "06.Hi Hat Room_01-02.wav",
        "07.Tea Time Over_01-02.wav",
        "08.Tea Time Logo_01-02.wav",
        "09.Toms etc Overhead_01-02.wav",
        "10.Toms etc Room_01-02.wav",
        "11.Kick (Mono)_01-02.wav",
        "12.Snare_01-02.wav",
        "13.Bass_01-02.wav",
        "14.Guitar.DI.Heavy.Rakes_01-02.wav",
        "15.Guitar.Amp.Heavy.Rakes_01-02.wav",
        "16.Guitar.Amp.Room.Heavy.Rakes_01-02.wav",
        "17.Guitar.DI.Two_01-02.wav",
        "18.Guitar.Amp.Two_01-02.wav",
        "19.Guitar.Amp.Room.Two_01-02.wav",
        "20.Guitar.DI_01-02.wav",
        "21.Guitar.Amp_01-02.wav",
        "22.Breathy Synth 1 - Casio CTK-601 Briteness_01-02.wav",
        "23.Breathy Synth 2 - Casio CTK-601 Briteness_1.1_01-02.wav",
        "24.NORD B3_1.1_01-02.wav",
        "25.Weird Synth Patch - CASIO Charang_1.1_01-02.wav",
        "26.Weird Synth Patch - CASIO Timpani_1.1_01-02.wav",
        "27.Weird Synth Patch - FA06 1_1.1_01-02.wav",
        "28.Weird Synth Patch - FA06 2_1.1_01-02.wav",
        "29.Weird Synth Patch - FA06 3_1.1_01-02.wav",
        "30.Weird Synth Patch - FA06 4_1.1_01-02.wav",
        "31.Weird Synth Patch - FA06 Dog Bark_1.1_01-02.wav",
        "32.Weird Synth Patch - FA06 Synth_SUM_1.1_01-02.wav",
        "33.Keys_01-02.wav",
        "34.SHOUT VOX_CHORUS AD LIB 1 L_01-02.R.wav",
        "35.SHOUT VOX_CHORUS AD LIB 1 R_01-02.R.wav",
        "36.SHOUT VOX_CHORUS AD LIB 2_01-02.R.wav",
        "37.SHOUT VOX_CHORUS AD LIB 3_01-02.R.wav",
        "38.SHOUT VOX_CHORUS AD LIB 4_01-02.R.wav",
        "39.SHOUT VOX_CHORUS DBL CENTRE_01-02.R.wav",
        "40.SHOUT VOX_CHORUS DBL L_01-02.R.wav",
        "41.SHOUT VOX_CHORUS DBL R_01-02.R.wav",
        "42.SHOUT VOX_CHORUS LEAD_01-02.R.wav",
        "43.SHOUT VOX_CHORUS POWERFUL END_01-02.R.wav",
        "44.SHOUT VOX_VERSE_01-02.R.wav",
        "45.SHOUT VOX_CHORUS SHOUT L_01-02.R.wav",
        "46.SHOUT VOX_CHORUS SHOUT R_01-02.R.wav",
        "47.Shout_Neumann BIMM_4824_mst_01-02.wav",
    ];

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // TODO: Add expected structure once provided
}
