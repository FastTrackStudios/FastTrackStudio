use dynamic_template::*;

#[test]
fn katie_ferrara_valerie() {
    // Track list from "Katie Ferrara - Valerie"
    let items = vec![
        "01.Kick_01-01.wav",
        "02.Snare_01-01.wav",
        "03.OH_01-01.wav",
        "04.Bass DI_01-01.wav",
        "05.Bass Amp_01-01.wav",
        "06.Acoustic_01-01.wav",
        "07.Gtr DI_01-01.wav",
        "08.Gtr Amp_01-01.wav",
        "09.Gtr DI.Riff_01-01.wav",
        "10.Gtr Amp.Riff_01-01.wav",
        "11.Gtr DI.Solo_01-01.wav",
        "12.Gtr Amp.Solo_01-01.wav",
        "13.Vocal_01-01.wav",
        "14.KD.BV_01-01.wav",
        "15.KD.BV Dbl_01-01.wav",
        "16.H3000.One_01-01.wav",
        "17.H3000.Two_01-01.wav",
        "18.H3000.Three_01-01.wav",
        "19.Vocal.Eko.Plate_01-01.wav",
        "20.Vocal.Magic_01-01.wav",
        "21.Valerie Mix_01-01.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
