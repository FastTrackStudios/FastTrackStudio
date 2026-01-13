use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn laura_clapp_davidson_hey_hey_sister() -> Result<()> {
    // -- Setup & Fixtures
    // Laura Clapp Davidson - Hey Hey Sister: Folk/pop with harmony layers
    let items = vec![
        "01.KICK_01.wav",
        "02.SNARE_01.wav",
        "03.HH_01.wav",
        "04.OHS_01.wav",
        "05.ROOM_01.wav",
        "06.Hey_Sister_Bass DI_01.wav",
        "07.Hey_Sister_Bass_Tone_X_01.wav",
        "08.Hey_Sister_Acoustic_L_01.wav",
        "09.Hey_Sister_Acoustic_R_01.wav",
        "10.Hey_Sister_Gtr_1_L_01.wav",
        "11.Hey_Sister_Gtr_2_R_01.wav",
        "12.Hey_Sister_Licks_01.wav",
        "13.Hey_Sister_Solo_01.wav",
        "14.Laura Lead Vocal_01.wav",
        "15.Laura Verse Harmony_01.wav",
        "16.Laura Chorus Double_01.wav",
        "17.Laura Chorus Harmony 1_01.wav",
        "18.Laura Chorus Harmony 2_01.wav",
        "19.Laura Hey Hey High 1_01.wav",
        "20.Laura Hey Hey High 2_01.wav",
        "21.Laura Hey Hey Highest 1_01.wav",
        "22.Laura Hey Hey Highest 2_01.wav",
        "23.Laura Hey Hey Low 1_01.wav",
        "24.Laura Hey Hey Low 2_01.wav",
        "25.Laura Hey Hey Mid 1_01.wav",
        "26.Laura Hey Hey Mid 2_01.wav",
        "27. Hey Hey Sister_01.wav",
    ];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Note: "Hey Hey" patterns now match BGVs, so the harmony parts are properly organized
    // The "Hey Hey Sister" reference also matches due to containing "hey hey"
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .track("Kick", "01.KICK_01.wav")
            .track("Snare", "02.SNARE_01.wav")
            .folder("Cymbals")
                .track("OH", "04.OHS_01.wav")
                .track("Hi Hat", "03.HH_01.wav")
            .end()
            .track("Rooms", "05.ROOM_01.wav")
        .end()
        .folder("Bass")
            .track("Bass 1", "06.Hey_Sister_Bass DI_01.wav")
            .track("Bass 2", "07.Hey_Sister_Bass_Tone_X_01.wav")
        .end()
        .folder("Guitars")
            .folder("Acoustic")
                .track("Acoustic 1", "08.Hey_Sister_Acoustic_L_01.wav")
                .track("Acoustic 2", "09.Hey_Sister_Acoustic_R_01.wav")
            .end()
            .track("Hey_Sister_Gtr_1_L", "10.Hey_Sister_Gtr_1_L_01.wav")
            .track("Hey_Sister_Gtr_2_R", "11.Hey_Sister_Gtr_2_R_01.wav")
        .end()
        .folder("Vocals")
            .track("Lead", "14.Laura Lead Vocal_01.wav")
            .folder("BGVs")
                .folder("Laura")
                    .folder("Chorus")
                        .track("Harmony 1", "17.Laura Chorus Harmony 1_01.wav")
                        .track("Harmony 2", "18.Laura Chorus Harmony 2_01.wav")
                    .end()
                    .track("Verse", "15.Laura Verse Harmony_01.wav")
                    .folder("Low")
                        .track("Low 1", "23.Laura Hey Hey Low 1_01.wav")
                        .track("Low 2", "24.Laura Hey Hey Low 2_01.wav")
                    .end()
                    .folder("High")
                        .track("High 1", "19.Laura Hey Hey High 1_01.wav")
                        .track("High 2", "20.Laura Hey Hey High 2_01.wav")
                    .end()
                    .folder("Highest")
                        .track("Highest 1", "21.Laura Hey Hey Highest 1_01.wav")
                        .track("Highest 2", "22.Laura Hey Hey Highest 2_01.wav")
                    .end()
                    .folder("Mid")
                        .track("Mid 1", "25.Laura Hey Hey Mid 1_01.wav")
                        .track("Mid 2", "26.Laura Hey Hey Mid 2_01.wav")
                    .end()
                .end()
                .track("BGVs", "27. Hey Hey Sister_01.wav")
            .end()
        .end()
        .track("Choir", "16.Laura Chorus Double_01.wav")
        .folder("Unsorted")
            .track("Hey_Sister_Licks", "12.Hey_Sister_Licks_01.wav")
            .track("Hey_Sister_Solo", "13.Hey_Sister_Solo_01.wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
