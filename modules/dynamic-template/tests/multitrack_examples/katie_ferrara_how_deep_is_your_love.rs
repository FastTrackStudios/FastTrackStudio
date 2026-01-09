use dynamic_template::*;

#[test]
fn katie_ferrara_how_deep_is_your_love() {
    // Track list from "Katie Ferrara & Steve Maggiora - How Deep Is Your Love"
    let items = vec![
        "Acoustic SILK BLUE 100%_03.wav",
        "Acoustic SILK BLUE 50%_03.wav",
        "Acoustic SILK OFF.02_03-02.wav",
        "Acoustic SILK RED 100%_03.wav",
        "Acoustic SILK RED 50%_03.wav",
        "Bass SILK BLUE 100%_02.wav",
        "Bass SILK BLUE 50%_02.wav",
        "Bass SILK OFF_02.wav",
        "Bass SILK RED 100%_02.wav",
        "Bass SILK RED 50%_02.wav",
        "HH SILK BLUE 100%_01.wav",
        "HH SILK BLUE 50%_01.wav",
        "HH SILK OFF_02.wav",
        "HH SILK RED 100%_01.wav",
        "HH SILK RED 50%_01.wav",
        "Katie Vocal SILK BLUE 100%_01.wav",
        "Katie Vocal SILK BLUE 50%_01.wav",
        "Katie Vocal SILK OFF.02_01.wav",
        "Katie Vocal SILK RED 100%_01.wav",
        "Katie Vocal SILK RED 50%_01.wav",
        "Keys Silk Blue 100%_01.wav",
        "Keys Silk Blue 50%_02.wav",
        "Keys Silk OFF_01.wav",
        "Keys Silk RED 100%_01-03.wav",
        "Keys Silk RED 50%_01.wav",
        "Kick SILK BLUE 100%_01.wav",
        "Kick SILK BLUE 50%_01.wav",
        "Kick SILK OFF_05.wav",
        "Kick SILK RED 100%_01.wav",
        "Kick SILK RED 50%_01.wav",
        "Ride SILK BLUE 100%_01.wav",
        "Ride SILK BLUE 50%_01.wav",
        "Ride SILK OFF_02.wav",
        "Ride SILK RED 100%_01.wav",
        "Ride SILK RED 50%_01.wav",
        "Snare SILK BLUE 100%_01.wav",
        "Snare SILK BLUE 50%_01.wav",
        "Snare SILK OFF_01.wav",
        "Snare SILK RED 100%_01.wav",
        "Snare SILK RED 50%_01.wav",
        "Steve BGV Vocal SILK OFF.(th.LDble_01.wav",
        "Steve BGV Vocal SILK OFF.9th_01.wav",
        "Steve BGV Vocal SILK OFF.9th.R.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.BV_01.wav",
        "Steve BGV Vocal SILK OFF.BV.2_01.wav",
        "Steve BGV Vocal SILK OFF.BV.2.L.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.BV.2.R.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.BV.L.dble_01.wav",
        "Steve BGV Vocal SILK OFF.BV.R.dble_01.wav",
        "Steve BGV Vocal SILK OFF.BV3_01.wav",
        "Steve BGV Vocal SILK OFF.BV3.L.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.BV3.R.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.High_01.wav",
        "Steve BGV Vocal SILK OFF.High.2_01.wav",
        "Steve BGV Vocal SILK OFF.High.2.L.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.High.2.R.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.High.L.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.High.R.Dble_01.wav",
        "Steve BGV Vocal SILK OFF.LOW_01.wav",
        "Steve BGV Vocal SILK OFF.LOW.DBLE_01.wav",
        "Steve Vocal SILK Blue 100%.wav",
        "Steve Vocal SILK Blue 50%_01.wav",
        "Steve Vocal SILK OFF.03_01.wav",
        "Steve Vocal SILK RED 100%_01.wav",
        "Steve Vocal SILK RED 50%_01.wav",
    ];

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // TODO: Add expected structure once provided
}
