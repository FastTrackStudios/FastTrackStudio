use dynamic_template::*;

#[test]
fn day_tripper_cover_warm_audio_wa_19_multitracks() {
    // Track list from "Day Tripper Cover - Warm Audio Wa-19 Multitracks"
    let items = vec![
        "bass hofner_01.wav",
        "bass hofner.Duplicate _01.wav",
        "click.Edit_01.wav",
        "fernando strat_02.wav",
        "gretsch fernando_02.wav",
        "kick_01.wav",
        "oh_01.wav",
        "snare_01.wav",
        "Steve BGV1_SUM.06_01.wav",
        "Steve BGV1A.06_01.wav",
        "Steve BGV1B.06_01.wav",
        "Steve BGV1C.06_01.wav",
        "Steve BGV1D.06_01.wav",
        "Steve BGV2_SUM.06_01.wav",
        "Steve BGV2A.06_01.wav",
        "Steve BGV2B.06_01.wav",
        "Steve BGV2C.06_01.wav",
        "Steve BGV2D.06_01.wav",
        "Steve Lead VOX Take 1.06_01.wav",
        "Steve Lead VOX Take 2.06_01.wav",
        "swell_01.wav",
        "tambourine_01.wav",
        "warren strat_02.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
