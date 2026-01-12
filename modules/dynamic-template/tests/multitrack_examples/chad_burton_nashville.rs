use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn chad_burton_nashville() -> Result<()> {
    // -- Setup & Fixtures
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

    // TODO: Add expected structure once provided

    Ok(())
}
