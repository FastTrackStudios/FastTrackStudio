use dynamic_template::*;

#[test]
fn roswell_aint_no_sunshine() {
    // Track list from "Roswell - Ain't No Sunshine"
    let items = vec![
        "01.Djembe Bottom 47X_01.wav",
        "02.Djembe Top 67X_01.wav",
        "03.Bass DI.01_02.wav",
        "04.Bass Amp 47X_01.wav",
        "05.Gtr DI.01_02.wav",
        "06.Gtr Amp.67X_01.wav",
        "07.Gtr Warren Amp.01_02.wav",
        "08.Gtr Solo DI.01_02.wav",
        "09.Gtr Solo Amp.67X_01.wav",
        "10.Acoustic 47X_01.wav",
        "11.Vocal.47X_01.wav",
        "12.Ain't No Sunshine Mix_Roswell Mini K47x Mini K67x_02.wav",
        "Djembe Top Quantized.wav",
        "HiHat.wav",
        "Kick.wav",
        "Overhead.wav",
        "Sidestick.wav",
        "Snare.wav",
        "STRINGS HIGH.wav",
        "STRINGS MAIN.wav",
        "Tom  3.wav",
        "Tom  4.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
