use daw_proto::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn james_dupre_another_love_song() -> Result<()> {
    // -- Setup & Fixtures
    // James Dupre - Another Love Song: Country/americana with B3 organ and various guitars
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
        .track("SUM").item("02.Kick Out_01.wav")
        .track("Kick 1").item("01.Kick_01.wav")
        .track("Kick 2").item("03.Kick Sample_01.wav")
        .end()
        .folder("Snare")
        .track("Bottom").item("07.Snare Bottom_01.wav")
        .track("Snare").item("04.Snare _01.wav")
        .end()
        .folder("Toms")
        .track("T1").item("09.Tom 1_01.wav")
        .track("T2").item("10.Tom 2_01.wav")
        .end()
        .folder("Cymbals")
        .track("OH").item("11.OH_01.wav")
        .track("Hi Hat").item("08.Hat_01.wav")
        .end()
        .folder("Rooms")
        .track("Mono").item("13.Room Mono_01.wav")
        .track("Stereo").item("12.Room Stereo_01.wav")
        .end()
        .track("Drum Kit").item("14.Kit Mono_01.wav")
        .end()
        .folder("Snare")
        .track("Snare 1").item("05.Snare Sample Stereo_01.wav")
        .track("Snare 2").item("06.Snare Sample Stereo 2_01.wav")
        .end()
        .end()
        .track("Bass").item("15.Bass_01.wav")
        .folder("Guitars")
        .folder("Electric")
        .track("Electric 1").item("18.EG2 (57)_01.wav")
        .track("Electric 2").item("19.EG2 (FH)_01.wav")
        .track("Electric 3").item("20.EG1 (57)_01.wav")
        .track("Electric 4").item("21.EG1 (FH)_01.wav")
        .track("Electric 5").item("22.EG3 (57)_01.wav")
        .track("Electric 6").item("23.EG3 (FH)_01.wav")
        .end()
        .track("Acoustic").item("16.AG1_01.wav")
        .track("Steel").item("24.Steel_01.wav")
        .track("Banjo").item("17.Banjo  _01.wav")
        .end()
        .folder("Keys")
        .folder("Organ")
        .track("Organ 1").item("26.B3_01.wav")
        .track("Organ 2").item("27.B3 Low_01.wav")
        .end()
        .track("Keys").item("25.Keys_01.wav")
        .end()
        .folder("Vocals")
        .track("Lead").item("28.Lead Vocal_01.wav")
        .folder("BGVs")
        .track("BGVs 1").item("29.BGV 1_01.wav")
        .track("BGVs 2").item("30.BGV 2_01.wav")
        .end()
        .end()
        .track("Guide").item("Click 128.wav")
        .track("Reference").item("31.AnotherLoveSong Joe MIX_01.wav")
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
