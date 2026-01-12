use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn katie_ferrara_how_deep_is_your_love() -> Result<()> {
    // -- Setup & Fixtures
    // Katie Ferrara - How Deep Is Your Love: Complex session with SILK preamp variations and extensive BGVs
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
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .folder("Kick")
                .track("Kick 1", "Kick SILK BLUE 100%_01.wav")
                .track("Kick 2", "Kick SILK BLUE 50%_01.wav")
                .track("Kick 3", "Kick SILK OFF_05.wav")
                .track("Kick 4", "Kick SILK RED 100%_01.wav")
                .track("Kick 5", "Kick SILK RED 50%_01.wav")
            .end()
            .folder("Snare")
                .track("Snare 1", "Snare SILK BLUE 100%_01.wav")
                .track("Snare 2", "Snare SILK BLUE 50%_01.wav")
                .track("Snare 3", "Snare SILK OFF_01.wav")
                .track("Snare 4", "Snare SILK RED 100%_01.wav")
                .track("Snare 5", "Snare SILK RED 50%_01.wav")
            .end()
            .folder("Cymbals")
                .folder("Hi Hat")
                    .track("Hi Hat 1", "HH SILK BLUE 100%_01.wav")
                    .track("Hi Hat 2", "HH SILK BLUE 50%_01.wav")
                    .track("Hi Hat 3", "HH SILK OFF_02.wav")
                    .track("Hi Hat 4", "HH SILK RED 100%_01.wav")
                    .track("Hi Hat 5", "HH SILK RED 50%_01.wav")
                .end()
                .folder("Ride")
                    .track("Ride 1", "Ride SILK BLUE 100%_01.wav")
                    .track("Ride 2", "Ride SILK BLUE 50%_01.wav")
                    .track("Ride 3", "Ride SILK OFF_02.wav")
                    .track("Ride 4", "Ride SILK RED 100%_01.wav")
                    .track("Ride 5", "Ride SILK RED 50%_01.wav")
                .end()
            .end()
        .end()
        .folder("Bass")
            .track("Bass 1", "Bass SILK BLUE 100%_02.wav")
            .track("Bass 2", "Bass SILK BLUE 50%_02.wav")
            .track("Bass 3", "Bass SILK OFF_02.wav")
            .track("Bass 4", "Bass SILK RED 100%_02.wav")
            .track("Bass 5", "Bass SILK RED 50%_02.wav")
        .end()
        .folder("Guitars")
            .track("Acoustic 1", "Acoustic SILK BLUE 100%_03.wav")
            .track("Acoustic 2", "Acoustic SILK BLUE 50%_03.wav")
            .track("Acoustic 3", "Acoustic SILK OFF.02_03-02.wav")
            .track("Acoustic 4", "Acoustic SILK RED 100%_03.wav")
            .track("Acoustic 5", "Acoustic SILK RED 50%_03.wav")
        .end()
        .folder("Keys")
            .track("Keys 1", "Keys Silk Blue 100%_01.wav")
            .track("Keys 2", "Keys Silk Blue 50%_02.wav")
            .track("Keys 3", "Keys Silk OFF_01.wav")
            .track("Keys 4", "Keys Silk RED 100%_01-03.wav")
            .track("Keys 5", "Keys Silk RED 50%_01.wav")
        .end()
        .folder("Vocals")
            .folder("Lead")
                .folder("Katie")
                    .track("Katie 1", "Katie Vocal SILK BLUE 100%_01.wav")
                    .track("Katie 2", "Katie Vocal SILK BLUE 50%_01.wav")
                    .track("Katie 3", "Katie Vocal SILK OFF.02_01.wav")
                    .track("Katie 4", "Katie Vocal SILK RED 100%_01.wav")
                    .track("Katie 5", "Katie Vocal SILK RED 50%_01.wav")
                .end()
                .folder("Steve")
                    .track("Steve 1", "Steve Vocal SILK Blue 100%.wav")
                    .track("Steve 2", "Steve Vocal SILK Blue 50%_01.wav")
                    .track("Steve 3", "Steve Vocal SILK OFF.03_01.wav")
                    .track("Steve 4", "Steve Vocal SILK RED 100%_01.wav")
                    .track("Steve 5", "Steve Vocal SILK RED 50%_01.wav")
                .end()
            .end()
            .folder("BGVs")
                .folder("Low")
                    .track("Steve 1", "Steve BGV Vocal SILK OFF.LOW_01.wav")
                    .track("Steve 2", "Steve BGV Vocal SILK OFF.LOW.DBLE_01.wav")
                .end()
                .folder("High")
                    .folder("High")
                        .track("L", "Steve BGV Vocal SILK OFF.High.L.Dble_01.wav")
                        .track("R", "Steve BGV Vocal SILK OFF.High.R.Dble_01.wav")
                        .track("Steve", "Steve BGV Vocal SILK OFF.High_01.wav")
                    .end()
                    .folder("High 2")
                        .track("L", "Steve BGV Vocal SILK OFF.High.2.L.Dble_01.wav")
                        .track("R", "Steve BGV Vocal SILK OFF.High.2.R.Dble_01.wav")
                        .track("Steve 2", "Steve BGV Vocal SILK OFF.High.2_01.wav")
                    .end()
                .end()
                .folder("BGVs")
                    .folder("L")
                        .track("Steve L 1", "Steve BGV Vocal SILK OFF.BV.L.dble_01.wav")
                        .track("Steve L 2", "Steve BGV Vocal SILK OFF.BV3.L.Dble_01.wav")
                    .end()
                    .folder("R")
                        .track("Steve R 1", "Steve BGV Vocal SILK OFF.9th.R.Dble_01.wav")
                        .track("Steve R 2", "Steve BGV Vocal SILK OFF.BV.R.dble_01.wav")
                        .track("Steve R 3", "Steve BGV Vocal SILK OFF.BV3.R.Dble_01.wav")
                    .end()
                    .track("Steve 1", "Steve BGV Vocal SILK OFF.(th.LDble_01.wav")
                    .track("Steve 2", "Steve BGV Vocal SILK OFF.9th_01.wav")
                    .track("Steve 3", "Steve BGV Vocal SILK OFF.BV_01.wav")
                    .track("Steve 4", "Steve BGV Vocal SILK OFF.BV3_01.wav")
                .end()
                .folder("BGVs 2")
                    .track("L", "Steve BGV Vocal SILK OFF.BV.2.L.Dble_01.wav")
                    .track("R", "Steve BGV Vocal SILK OFF.BV.2.R.Dble_01.wav")
                    .track("Steve 2", "Steve BGV Vocal SILK OFF.BV.2_01.wav")
                .end()
            .end()
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
