use dynamic_template::*;

#[test]
fn iron_maiden_the_trooper_stems() {
    // Track list from "Iron+Maiden+-+The+Trooper-Stems"
    let items = vec![
        "08-Kick-TheTrooper.wav",
        "09-Snare-TheTrooper.wav",
        "10-OH-TheTrooper.wav",
        "12-Bass DI-TheTrooper.wav",
        "13-Bass Amp-TheTrooper.wav",
        "15-Rhy Gtr L-TheTrooper.wav",
        "16-Rhy Gtr R-TheTrooper.wav",
        "17-Solo 1-TheTrooper.wav",
        "18-Solo 2-TheTrooper.wav",
        "19-Solo 3-TheTrooper.wav",
        "21-Vocal 1-TheTrooper.wav",
        "22-Vocal 2-TheTrooper.wav",
        "23-Vocal 3-TheTrooper.wav",
        "Trooper-mix1.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
