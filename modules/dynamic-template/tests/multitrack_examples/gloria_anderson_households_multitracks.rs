use dynamic_template::*;

#[test]
fn gloria_anderson_households_multitracks() {
    // Track list from "Gloria Anderson - Households Multitracks"
    let items = vec![
        "01.Households ACO JC MIXX_01.wav",
        "02.Kick LIM_01.wav",
        "03.Kick Out LIM_01.wav",
        "04.Sn T_01.wav",
        "05.Sn B_01.wav",
        "06.Hat_01.wav",
        "07.Tom 1 EDT_01.wav",
        "08.Tom 2 EDT_01.wav",
        "09.OH_01.wav",
        "10.Room_01.wav",
        "11.Bass Body_01.wav",
        "12.Bass Neck_01.wav",
        "13.AG 1_01.wav",
        "14.Piano_01.wav",
        "15.Steel_01.wav",
        "16.Vocal_01.wav",
        "Choir.wav",
        "Households_TimHoek_NashGtrL.wav",
        "Households_TimHoek_NashGtrR.wav",
        "Households_TimHoek_Strings.wav",
        "Strings (R).wav",
        "Vocal harmony (D).wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
