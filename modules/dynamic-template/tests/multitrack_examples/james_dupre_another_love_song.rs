use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
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
    daw::tracks::display_tracklist(&tracks);

    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .folder("Drum Kit")
                .folder("Kick")
                    .track("SUM", "02.Kick Out_01.wav")
                    .track("Kick 1", "01.Kick_01.wav")
                    .track("Kick 2", "03.Kick Sample_01.wav")
                .end()
                .folder("Snare")
                    .track("Bottom", "07.Snare Bottom_01.wav")
                    .track("Snare", "04.Snare _01.wav")
                .end()
                .folder("Toms")
                    .track("T1", "09.Tom 1_01.wav")
                    .track("T2", "10.Tom 2_01.wav")
                .end()
                .folder("Cymbals")
                    .track("OH", "11.OH_01.wav")
                    .track("Hi Hat", "08.Hat_01.wav")
                .end()
                .folder("Rooms")
                    .track("Mono", "13.Room Mono_01.wav")
                    .track("Stereo", "12.Room Stereo_01.wav")
                .end()
                .track("Drum Kit", "14.Kit Mono_01.wav")
            .end()
            .folder("Snare")
                .track("Snare 1", "05.Snare Sample Stereo_01.wav")
                .track("Snare 2", "06.Snare Sample Stereo 2_01.wav")
            .end()
        .end()
        .track("Bass", "15.Bass_01.wav")
        .folder("Guitars")
            .folder("Electric")
                .track("Electric 1", "18.EG2 (57)_01.wav")
                .track("Electric 2", "19.EG2 (FH)_01.wav")
                .track("Electric 3", "20.EG1 (57)_01.wav")
                .track("Electric 4", "21.EG1 (FH)_01.wav")
                .track("Electric 5", "22.EG3 (57)_01.wav")
                .track("Electric 6", "23.EG3 (FH)_01.wav")
            .end()
            .track("Acoustic", "16.AG1_01.wav")
            .track("Steel", "24.Steel_01.wav")
            .track("Banjo", "17.Banjo  _01.wav")
        .end()
        .folder("Keys")
            .folder("Organ")
                .track("Organ 1", "26.B3_01.wav")
                .track("Organ 2", "27.B3 Low_01.wav")
            .end()
            .track("Keys", "25.Keys_01.wav")
        .end()
        .folder("Vocals")
            .track("Lead", "28.Lead Vocal_01.wav")
            .folder("BGVs")
                .track("BGVs 1", "29.BGV 1_01.wav")
                .track("BGVs 2", "30.BGV 2_01.wav")
            .end()
        .end()
        .track("Guide", "Click 128.wav")
        .track("Reference", "31.AnotherLoveSong Joe MIX_01.wav")
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
