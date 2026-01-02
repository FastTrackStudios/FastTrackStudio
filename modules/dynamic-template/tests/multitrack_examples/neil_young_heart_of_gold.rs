use dynamic_template::*;

#[test]
fn neil_young_heart_of_gold() {
    // Track list from "Neil Young - Heart of Gold"
    let items = vec![
        "01.Kick OptoComp_01.wav",
        "02.Snare No Compression_01.wav",
        "03.OH L 260VU_01.wav",
        "04.OH R 260VU_01.wav",
        "05.Bass DI OptoComp_01.wav",
        "06.Bass Amp 44A. 260VU_01.wav",
        "07.Bass Amp 421 260VU_01.wav",
        "08.Acoustic OptoComp_01.wav",
        "09.Acoustic Dbl OptoComp_01.wav",
        "10.Acoustic Harmonics OptoComp_01.wav",
        "11.Acoustic Outro Strum OptoComp_01.wav",
        "12.Mando OptoComp_01.wav",
        "13.Steel Guitar OptoComp_01.wav",
        "14.Piano 260VU_01.wav",
        "15.Piano Lead 260VU_01.wav",
        "16.Vocal OptoComp_01.wav",
        "17.Vocal.DBL OptoComp_01.wav",
        "18.Vocal.Triple OptoComp_01.wav",
        "19.Vocal.HARMONY OptoComp_01.wav",
        "20.Vocal.HARMONY Dbl OptoComp_01.wav",
        "21.Heart Of Gold Mix_01.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
