use dynamic_template::*;

#[test]
fn the_cure_friday_im_in_love() {
    // Track list from "The Cure - Friday I'm In Love"
    let items = vec![
        "01.Kick_01.wav",
        "02.Snare_01.wav",
        "03.Ohd_01 L.wav",
        "04.Ohd_01 R.wav",
        "05.Tambo_01.wav",
        "06.Bass DI_01.wav",
        "07.Bass Amp_01.wav",
        "08.Acoustic_01.wav",
        "09.Acoustic.dbl_01.wav",
        "10.Acoustic OD_01.wav",
        "11.Acoustic OD Dbl_01.wav",
        "12.Gtr 1 DI_01.wav",
        "13.Gtr 1 Amp_01.wav",
        "14.Gtr 2 DI_01.wav",
        "15.Gtr 2 Amp_01.wav",
        "16.Gtr 2 DI Overlap_01.wav",
        "17.Gtr 2 Amp Overlap_01.wav",
        "18.Gtr Solo DI_01.wav",
        "19.Gtr Solo Amp_01.wav",
        "20.Gtr Root Note DI_01.wav",
        "21.Gtr Root Note Amp_01.wav",
        "23.Pad_01.wav",
        "24.Vocal_01.wav",
        "25.Vocal Overlap_01.wav",
        "26. Vocal Dbl_01.wav",
        "27.HARM_01.wav",
        "28.Harm Overlap_01.wav",
        "29.Friday I'm In Love Mix_01.wav",
    ];

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // TODO: Add expected structure once provided
}
