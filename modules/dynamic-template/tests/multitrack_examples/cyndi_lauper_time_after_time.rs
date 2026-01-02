use dynamic_template::*;

#[test]
fn cyndi_lauper_time_after_time() {
    // Track list from "Cyndi Lauper - Time After Time"
    let items = vec![
        "01.LV BECCA.TimeAfterTime.126bpm_01.wav",
        "02.BV LUCA.TimeAfterTime.126bpm_01.wav",
        "03.Acoustic Guitar.Nick.TimeAfterTime.126bpm_01.wav",
        "04.Electric.Gtr.Left.Warren.TimeAfterTime.126bpm_01.wav",
        "05.Electric.Gtr.Right.Warren.TimeAfterTime.126bpm_01.wav",
        "06.BASS AMP.Warren.TimeAfterTime.126bpm_01.wav",
        "07.BASS DI.Warren.TimeAfterTime.126bpm_01.wav",
        "08.Kick.TimeAfterTime.126bpm_01.wav",
        "09.Snare.TimeAfterTime.126bpm_01.wav",
        "10.Kit.Mono.Room.TimeAfterTime.126bpm_01.wav",
        "11.Overhead Hat.TimeAfterTime.126bpm_01.wav",
        "12.Overhead Ride.TimeAfterTime.126bpm_01.wav",
        "13.Room.Left.TimeAfterTime.126bpm_01.wav",
        "14.Room.Right.TimeAfterTime.126bpm_01.wav",
        "15.SHAKER.Left_01.wav",
        "16.SHAKER.Right_01.wav",
        "17.Click.Print_01.wav",
        "18.Time After Time Presonus HD8 Mix_01.wav",
    ];
    
    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();
    
    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);
    
    // TODO: Add expected structure once provided
}
