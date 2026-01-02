use dynamic_template::*;

#[test]
fn beatles_dont_let_me_down() {
    // Track list from "The Beatles - Don't Let Me Down"
    let items = vec![
        "01.Kick_01.wav",
        "02.Snare_01.wav",
        "03.OH_01.wav",
        "04.Bass Di_01.wav",
        "05.Bass Amp_01.wav",
        "06.Gtr DI_01.wav",
        "07.Gtr Amp_01.wav",
        "08.Keys_01.wav",
        "09.Vi.Vocal_01.wav",
        "10.Caitlin Vocal_01.wav",
        "11.Don't Let Me Down Joe Carrell Mix_01.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
