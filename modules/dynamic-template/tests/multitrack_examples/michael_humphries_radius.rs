use dynamic_template::*;

#[test]
fn michael_humphries_radius() {
    // Track list from "Michael Humphries - Radius"
    let items = vec![
        "01.Kick_01.wav",
        "02.SN Top_01.wav",
        "03.SN Bot_01.wav",
        "04.Rack.Tom_01.wav",
        "05.Flr.Tom_01.wav",
        "06.Hi Hat_01.wav",
        "07.OH_01.wav",
        "08.Room_01.wav",
        "09.Bass DI_01.wav",
        "10.Gtr Amp_01.wav",
        "11.Nord Piano_01.wav",
        "12.Radius Mix_01.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
