use daw_proto::{assert_tracks_equal, TrackStructureBuilder};
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
    daw_proto::display_tracklist(&tracks);

    let expected = TrackStructureBuilder::new()
        .folder("Drums")
        .folder("Drum Kit")
        .folder("Kick")
        .folder("SUM")
        .track("In")
        .item("Kick In")
        .track("Out")
        .item("Kick Out")
        .end()
        .track("Kick")
        .item("Kick Sample")
        .end()
        .folder("Snare")
        .track("Top")
        .item("Snare Top")
        .track("Bottom")
        .item("Snare Bottom")
        .end()
        .folder("Toms")
        .track("T1")
        .item("Tom1")
        .track("T2")
        .item("Tom2")
        .end()
        .folder("Cymbals")
        .track("OH")
        .item("OH")
        .track("Hi Hat")
        .item("HighHat")
        .end()
        .track("Rooms")
        .item("Rooms")
        .end()
        .folder("Snare")
        .track("Snare 1")
        .item("Snare Sample")
        .track("Snare 2")
        .item("Snare Sample Two")
        .end()
        .end()
        .track("Percussion")
        .item("Percussion")
        .track("Bass")
        .item("Bass DI")
        .folder("Guitars")
        .folder("Clean")
        .track("Left")
        .item("Lead Guitar Clean DI Left")
        .track("Right")
        .item("Lead Guitar Clean DI Right")
        .end()
        .folder("Lead")
        .track("Left")
        .item("Lead Guitar Amplitube Left")
        .track("Right")
        .item("Lead Guitar Amplitube Right")
        .end()
        .end()
        .track("Keys")
        .item("Piano")
        .folder("Vocals")
        .folder("Lead")
        .track("Lead 1")
        .item("Vocal")
        .track("Plate")
        .item("Vocal.Eko.Plate")
        .track("Lead 2")
        .item("Vocal.Magic")
        .end()
        .folder("BGVs")
        .track("BGVs 1")
        .item("BGV1")
        .track("BGVs 2")
        .item("BGV2")
        .track("BGVs 3")
        .item("BGV3")
        .track("BGVs 4")
        .item("BGV4")
        .end()
        .end()
        .folder("SFX")
        .track("H3000.One")
        .item("H3000.One")
        .track("H3000.Two")
        .item("H3000.Two")
        .track("H3000.Three")
        .item("H3000.Three")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
