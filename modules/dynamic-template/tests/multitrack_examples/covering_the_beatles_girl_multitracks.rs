use dynamic_template::*;

#[test]
fn covering_the_beatles_girl_multitracks() {
    // Track list from "Covering The Beatles Girl Multitracks"
    let items = vec![
        "AC GTR 1_01.wav",
        "Acoustic Guitar_TAPE .wav",
        "Bass_TAPE .wav",
        "BASS.03_01.wav",
        "boomK__BottleOver_02.wav",
        "boomK__FloorOmni414_02.wav",
        "boomK__KickRe20_02.wav",
        "Chad1_01.wav",
        "Chad2_01.wav",
        "Drums_TAPE_2.wav",
        "Framus end track Perry_01.wav",
        "Girl Sam J160 1_01.wav",
        "Girl Sam J160 2_01.wav",
        "GIRL_MDN.wav",
        "kitA_KickRe20_02.wav",
        "Lou1_01.wav",
        "Lou2_01.wav",
        "Marc VOX 2.wav",
        "Marc VOX.wav",
        "OverHead DRUMS_01.wav",
        "Snare DRUMS _01.R.wav",
        "Tit1_01.wav",
        "Tit2_01.wav",
        "Tit3_01.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
