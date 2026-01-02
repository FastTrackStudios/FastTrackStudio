use dynamic_template::*;

#[test]
fn marc_martel_don_t_stop_me_now_cover_multitracks() {
    // Track list from "Marc Martel - Don't Stop Me Now (Cover) Multitracks"
    let items = vec![
        "01.Kick In_01.wav",
        "02.Kick Out_01.wav",
        "03.Kick Sample_01.wav",
        "04.Snare Top_01.wav",
        "05.Snare Bottom_01.wav",
        "06.Snare Sample_01.wav",
        "07.Snare Sample Two_01.wav",
        "08.Tom1_01.wav",
        "09.Tom2_01.wav",
        "10.HighHat_01.wav",
        "11.OH_01.wav",
        "12.Rooms_01.wav",
        "13.Percussion_01.wav",
        "14.Bass DI_01.wav",
        "15.Piano_01.wav",
        "16.Lead Guitar Amplitube Left_01.wav",
        "17.Lead Guitar Amplitube Right_01.wav",
        "18.Lead Guitar Clean DI Left_01.wav",
        "19.Lead Guitar Clean DI Right_01.wav",
        "20.Vocal_01.wav",
        "21.H3000.One_01.wav",
        "22.H3000.Two_01.wav",
        "23.H3000.Three_01.wav",
        "24.Vocal.Eko.Plate_01.wav",
        "25.Vocal.Magic_01.wav",
        "26.BGV1_01.wav",
        "27.BGV2_01.wav",
        "28.BGV3_01.wav",
        "29.BGV4_01.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
