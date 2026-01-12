use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn marc_martel_dont_stop_me_now() -> Result<()> {
    // -- Setup & Fixtures
    // Marc Martel - Don't Stop Me Now: Queen cover with layered drums and H3000 effects
    let items = vec![
        "Kick In",
        "Kick Out",
        "Kick Sample",
        "Snare Top",
        "Snare Bottom",
        "Snare Sample",
        "Snare Sample Two",
        "Tom1",
        "Tom2",
        "HighHat",
        "OH",
        "Rooms",
        "Percussion",
        "Bass DI",
        "Piano",
        "Lead Guitar Amplitube Left",
        "Lead Guitar Amplitube Right",
        "Lead Guitar Clean DI Left",
        "Lead Guitar Clean DI Right",
        "Vocal",
        "H3000.One",
        "H3000.Two",
        "H3000.Three",
        "Vocal.Eko.Plate",
        "Vocal.Magic",
        "BGV1",
        "BGV2",
        "BGV3",
        "BGV4",
    ];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .folder("Drum Kit")
                .folder("Kick")
                    .folder("SUM")
                        .track("In", "Kick In")
                        .track("Out", "Kick Out")
                    .end()
                    .track("Kick", "Kick Sample")
                .end()
                .folder("Snare")
                    .track("Top", "Snare Top")
                    .track("Bottom", "Snare Bottom")
                .end()
                .folder("Toms")
                    .track("T1", "Tom1")
                    .track("T2", "Tom2")
                .end()
                .folder("Cymbals")
                    .track("OH", "OH")
                    .track("Hi Hat", "HighHat")
                .end()
                .track("Rooms", "Rooms")
            .end()
            .folder("Snare")
                .track("Snare 1", "Snare Sample")
                .track("Snare 2", "Snare Sample Two")
            .end()
        .end()
        .track("Percussion", "Percussion")
        .track("Bass", "Bass DI")
        .folder("Guitars")
            .folder("Clean")
                .track("Left", "Lead Guitar Clean DI Left")
                .track("Right", "Lead Guitar Clean DI Right")
            .end()
            .folder("Lead")
                .track("Left", "Lead Guitar Amplitube Left")
                .track("Right", "Lead Guitar Amplitube Right")
            .end()
        .end()
        .track("Keys", "Piano")
        .folder("Vocals")
            .folder("Lead")
                .track("Lead 1", "Vocal")
                .track("Plate", "Vocal.Eko.Plate")
                .track("Lead 2", "Vocal.Magic")
            .end()
            .folder("BGVs")
                .track("BGVs 1", "BGV1")
                .track("BGVs 2", "BGV2")
                .track("BGVs 3", "BGV3")
                .track("BGVs 4", "BGV4")
            .end()
        .end()
        .folder("SFX")
            .track("H3000.One", "H3000.One")
            .track("H3000.Two", "H3000.Two")
            .track("H3000.Three", "H3000.Three")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
