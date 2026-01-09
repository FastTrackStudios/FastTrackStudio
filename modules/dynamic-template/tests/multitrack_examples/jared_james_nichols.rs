use dynamic_template::*;

#[test]
fn jared_james_nichols() {
    // Track list from "Jared James Nichols - Unknown"
    let items = vec![
        "01 Kick In .wav",
        "02 Kick Out .wav",
        "03 Sub Kick .wav",
        "04 Snare Top .wav",
        "05 Snare Top.dup2 .wav",
        "06 Snare Bot .wav",
        "07 Hat .wav",
        "08 Rack .wav",
        "09 Floor .wav",
        "10 OH Hat .wav",
        "11 OH Ride .wav",
        "12 OH Mono .wav",
        "13 Crotch .wav",
        "14 Close RM Hat .wav",
        "15 Close RM Ride .wav",
        "16 Mid RM Hat .wav",
        "17 Mid RM Ride .wav",
        "18 Far RM Hat .wav",
        "19 Far RM Ride .wav",
        "20 Mono .wav",
        "21 Mono U47 .wav",
        "22 Bass DI .wav",
        "23 Bass Mic .wav",
        "24 Gtr Bus .wav",
        "25 Gtr Bus.dup1 .wav",
        "26 SoloGtr Bus.d .wav",
        "27 SoloGtr RM Left .wav",
        "28 SoloGtr RM Right .wav",
        "29 Talk Box .wav",
        "30 Jared .wav",
        "31 Jared call back .wav",
        "32 Jared Harm .wav",
        "33 Jared Harm.dup1 .wav",
        "Man In the Box Print 20220502 v2 .wav",
        "Smart Tempo Multitrack Set 1.wav",
    ];

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // TODO: Add expected structure once provided
}
