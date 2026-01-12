use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn chad_burton_nashville() -> Result<()> {
    // -- Setup & Fixtures
    // Chad Burton - Nashville: Country session with detailed percussion and piano
    let items = vec![
        "ACO L1_02.wav",
        "ACO R1_02.wav",
        "ACO RHY_02.wav",
        "ACO Slide SOLO L1_02.wav",
        "ACo slide solo r1_02.wav",
        "ACO Slide Solo_02 R.wav",
        "ACO Sllide Solo_02 L.wav",
        "ACO_02.wav",
        "AMB Slide_02.wav",
        "Ambient Loop_03.wav",
        "BU VOC DBL_02.wav",
        "BU VOC1_02.wav",
        "LD VOC_02.wav",
        "Nashville - DELAY PIANO_STRING SYNTH (might be too pop)_02.wav",
        "Nashville - DELAY PIANO_STRING SYNTH (might be too pop)(2)_02.wav",
        "Nashville - RHODES (take 2)_02.wav",
        "Nashville_BassShawn_02.wav",
        "Nashville_Center_Kit_Mic_02.wav",
        "Nashville_Clave_02.wav",
        "Nashville_Conga_High_02.wav",
        "Nashville_Conga_Low_02.wav",
        "Nashville_Cymbal_02.wav",
        "Nashville_Kick_In_02.wav",
        "Nashville_Kick_Out.dup1_01.wav",
        "Nashville_MIP2_MDN.wav",
        "Nashville_OH_Left_02.wav",
        "Nashville_OH_Right_02.wav",
        "Nashville_Room_Left_02.wav",
        "Nashville_Room_Right_02.wav",
        "Nashville_Shaker_02.wav",
        "Nashville_Snare_Bottom_02.wav",
        "Nashville_Snare_Top_02.wav",
        "Nashville_Tambourine_02.wav",
        "Nashville_Woodblock_02.wav",
        "Slide RHY_02.wav",
        "Steel GTR_02.wav",
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
                .track("In", "Nashville_Kick_In_02.wav")
                .track("Out", "Nashville_Kick_Out.dup1_01.wav")
            .end()
            .folder("Snare")
                .track("Top", "Nashville_Snare_Top_02.wav")
                .track("Bottom", "Nashville_Snare_Bottom_02.wav")
            .end()
            .folder("Cymbals")
                .folder("OH")
                    .track("L", "Nashville_OH_Left_02.wav")
                    .track("R", "Nashville_OH_Right_02.wav")
                .end()
                .track("Cymbals", "Nashville_Cymbal_02.wav")
            .end()
            .folder("Rooms")
                .track("L", "Nashville_Room_Left_02.wav")
                .track("R", "Nashville_Room_Right_02.wav")
                .track("Rooms 1", "AMB Slide_02.wav")
                .track("Rooms 2", "Ambient Loop_03.wav")
            .end()
            .track("Drum Kit", "Nashville_Center_Kit_Mic_02.wav")
        .end()
        .folder("Percussion")
            .track("Shaker", "Nashville_Shaker_02.wav")
            .track("Tambourine", "Nashville_Tambourine_02.wav")
            .track("Clave", "Nashville_Clave_02.wav")
            .folder("Conga")
                .track("Conga 1", "Nashville_Conga_High_02.wav")
                .track("Conga 2", "Nashville_Conga_Low_02.wav")
            .end()
            .track("Woodblock", "Nashville_Woodblock_02.wav")
        .end()
        .track("Bass", "Nashville_BassShawn_02.wav")
        .track("Guitars", "Steel GTR_02.wav")
        .folder("Keys")
            .folder("Piano")
                .track("Delay", "Nashville - DELAY PIANO_STRING SYNTH (might be too pop)_02.wav")
                .track("Delay 2", "Nashville - DELAY PIANO_STRING SYNTH (might be too pop)(2)_02.wav")
            .end()
            .track("Rhodes", "Nashville - RHODES (take 2)_02.wav")
        .end()
        .folder("Vocals")
            .folder("Main")
                .track("Lead 1", "BU VOC1_02.wav")
                .track("Lead 2", "LD VOC_02.wav")
            .end()
            .track("DBL", "BU VOC DBL_02.wav")
        .end()
        .track("Reference", "Nashville_MIP2_MDN.wav")
        .folder("Unsorted")
            .track("ACO L1", "ACO L1_02.wav")
            .track("ACO R1", "ACO R1_02.wav")
            .track("ACO RHY", "ACO RHY_02.wav")
            .track("ACO Slide SOLO L1", "ACO Slide SOLO L1_02.wav")
            .track("ACo Slide Solo R1", "ACo slide solo r1_02.wav")
            .track("ACO Slide Solo_02 R", "ACO Slide Solo_02 R.wav")
            .track("ACO Sllide Solo_02 L", "ACO Sllide Solo_02 L.wav")
            .track("ACO", "ACO_02.wav")
            .track("Slide RHY", "Slide RHY_02.wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
