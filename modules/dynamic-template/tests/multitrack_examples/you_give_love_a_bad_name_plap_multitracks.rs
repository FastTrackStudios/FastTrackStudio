use dynamic_template::*;

#[test]
fn you_give_love_a_bad_name_plap_multitracks() {
    // Track list from "You Give Love A Bad Name PLAP Multitracks"
    let items = vec![
        "095 Pop Tamb.L.wav",
        "095 Pop Tamb.R.wav",
        "Acoustic.Right.wav",
        "Acoustic.wav",
        "banjo.One.wav",
        "banjo.Solo.wav",
        "banjo.Two.wav",
        "Bass.wav",
        "Drums.PNT.L.wav",
        "Drums.PNT.R.wav",
        "Fiddle PNT.L.wav",
        "Fiddle PNT.R.wav",
        "Guitar Slide.wav",
        "Guitar Solo.wav",
        "Mando.wav",
        "Vocal.Harmony.One.wav",
        "Vocal.Harmony.Two.wav",
        "Vocal.Tune.Lead.wav",
        "You Give Love A Bad Name.PRINT.L.wav",
        "You Give Love A Bad Name.PRINT.R.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
