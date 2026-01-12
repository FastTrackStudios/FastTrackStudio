use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn katie_ferrara_valerie() -> Result<()> {
    // -- Setup & Fixtures
    // Katie Ferrara - Valerie: 21-track pop cover with DI/Amp guitar pairs and vocal effects tracks
    let items = vec![
        "01.Kick_01-01.wav",
        "02.Snare_01-01.wav",
        "03.OH_01-01.wav",
        "04.Bass DI_01-01.wav",
        "05.Bass Amp_01-01.wav",
        "06.Acoustic_01-01.wav",
        "07.Gtr DI_01-01.wav",
        "08.Gtr Amp_01-01.wav",
        "09.Gtr DI.Riff_01-01.wav",
        "10.Gtr Amp.Riff_01-01.wav",
        "11.Gtr DI.Solo_01-01.wav",
        "12.Gtr Amp.Solo_01-01.wav",
        "13.Vocal_01-01.wav",
        "14.KD.BV_01-01.wav",
        "15.KD.BV Dbl_01-01.wav",
        "16.H3000.One_01-01.wav",
        "17.H3000.Two_01-01.wav",
        "18.H3000.Three_01-01.wav",
        "19.Vocal.Eko.Plate_01-01.wav",
        "20.Vocal.Magic_01-01.wav",
        "21.Valerie Mix_01-01.wav",
    ];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Expected structure:
    // Drums/
    //   ├─ Kick                     ← 01.Kick
    //   ├─ Snare                    ← 02.Snare
    //   └─ OH                       ← 03.OH
    // Bass/
    //   ├─ Bass                     ← 04.Bass DI
    //   └─ Amp                      ← 05.Bass Amp
    // Guitars/
    //   ├─ Acoustic                 ← 06.Acoustic
    //   ├─ Guitars 1/2              ← 07/09 Gtr DI
    //   ├─ Amp 1/2                  ← 08/10 Gtr Amp
    //   ├─ Solo DI                  ← 11.Gtr DI.Solo
    //   └─ Solo Amp                 ← 12.Gtr Amp.Solo
    // Vocals/
    //   ├─ Lead/
    //   │   ├─ Lead 1               ← 13.Vocal
    //   │   ├─ Plate                ← 19.Vocal.Eko.Plate (vocal effect)
    //   │   └─ Lead 2               ← 20.Vocal.Magic (vocal effect)
    //   └─ BGVs/
    //       ├─ Main                 ← 14.KD.BV
    //       └─ DBL                  ← 15.KD.BV Dbl
    // SFX/                          ← H3000 effects (Eventide H3000)
    // Reference                     ← 21.Valerie Mix
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .track("Kick", "01.Kick_01-01.wav")
            .track("Snare", "02.Snare_01-01.wav")
            .track("OH", "03.OH_01-01.wav")
        .end()
        .folder("Bass")
            .track("Bass", "04.Bass DI_01-01.wav")
            .track("Amp", "05.Bass Amp_01-01.wav")
        .end()
        .folder("Guitars")
            .track("Acoustic", "06.Acoustic_01-01.wav")
            .track("Guitars 1", "07.Gtr DI_01-01.wav")
            .track("Amp 1", "08.Gtr Amp_01-01.wav")
            .track("Guitars 2", "09.Gtr DI.Riff_01-01.wav")
            .track("Amp 2", "10.Gtr Amp.Riff_01-01.wav")
            .track("Solo DI", "11.Gtr DI.Solo_01-01.wav")
            .track("Solo Amp", "12.Gtr Amp.Solo_01-01.wav")
        .end()
        .folder("Vocals")
            .folder("Lead")
                .track("Lead 1", "13.Vocal_01-01.wav")
                .track("Plate", "19.Vocal.Eko.Plate_01-01.wav")
                .track("Lead 2", "20.Vocal.Magic_01-01.wav")
            .end()
            .folder("BGVs")
                .track("Main", "14.KD.BV_01-01.wav")
                .track("DBL", "15.KD.BV Dbl_01-01.wav")
            .end()
        .end()
        .folder("SFX")
            .track("16.H3000.One_01-01", "16.H3000.One_01-01.wav")
            .track("17.H3000.Two_01-01", "17.H3000.Two_01-01.wav")
            .track("18.H3000.Three_01-01", "18.H3000.Three_01-01.wav")
        .end()
        .track("Reference", "21.Valerie Mix_01-01.wav")
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
