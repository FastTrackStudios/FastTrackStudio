use dynamic_template::*;

#[test]
fn james_dupre_another_love_song_128bpm() {
    // Track list from "James Dupre - Another Love Song 128BPM"
    let items = vec![
        "01.Kick_01.wav",
        "02.Kick Out_01.wav",
        "03.Kick Sample_01.wav",
        "04.Snare _01.wav",
        "05.Snare Sample Stereo_01.wav",
        "06.Snare Sample Stereo 2_01.wav",
        "07.Snare Bottom_01.wav",
        "08.Hat_01.wav",
        "09.Tom 1_01.wav",
        "10.Tom 2_01.wav",
        "11.OH_01.wav",
        "12.Room Stereo_01.wav",
        "13.Room Mono_01.wav",
        "14.Kit Mono_01.wav",
        "15.Bass_01.wav",
        "16.AG1_01.wav",
        "17.Banjo  _01.wav",
        "18.EG2 (57)_01.wav",
        "19.EG2 (FH)_01.wav",
        "20.EG1 (57)_01.wav",
        "21.EG1 (FH)_01.wav",
        "22.EG3 (57)_01.wav",
        "23.EG3 (FH)_01.wav",
        "24.Steel_01.wav",
        "25.Keys_01.wav",
        "26.B3_01.wav",
        "27.B3 Low_01.wav",
        "28.Lead Vocal_01.wav",
        "29.BGV 1_01.wav",
        "30.BGV 2_01.wav",
        "31.AnotherLoveSong Joe MIX_01.wav",
        "Click 128.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
