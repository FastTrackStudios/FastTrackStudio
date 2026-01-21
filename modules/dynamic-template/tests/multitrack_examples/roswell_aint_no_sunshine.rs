use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn roswell_aint_no_sunshine() -> Result<()> {
    // -- Setup & Fixtures
    // Roswell - Ain't No Sunshine: Mix with djembe, guitars, strings and reference
    let items = vec![
        "01.Djembe Bottom 47X_01.wav",
        "02.Djembe Top 67X_01.wav",
        "03.Bass DI.01_02.wav",
        "04.Bass Amp 47X_01.wav",
        "05.Gtr DI.01_02.wav",
        "06.Gtr Amp.67X_01.wav",
        "07.Gtr Warren Amp.01_02.wav",
        "08.Gtr Solo DI.01_02.wav",
        "09.Gtr Solo Amp.67X_01.wav",
        "10.Acoustic 47X_01.wav",
        "11.Vocal.47X_01.wav",
        "12.Ain't No Sunshine Mix_Roswell Mini K47x Mini K67x_02.wav",
        "Djembe Top Quantized.wav",
        "HiHat.wav",
        "Kick.wav",
        "Overhead.wav",
        "Sidestick.wav",
        "Snare.wav",
        "STRINGS HIGH.wav",
        "STRINGS MAIN.wav",
        "Tom  3.wav",
        "Tom  4.wav",
    ];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Expected structure:
    // Drums/
    //   ├─ Kick                    ← Kick.wav
    //   ├─ Snare                   ← Snare.wav
    //   ├─ Toms/
    //   │   ├─ Tom 3               ← Tom  3.wav
    //   │   └─ Tom 4               ← Tom  4.wav
    //   └─ Cymbals/
    //       ├─ OH                  ← Overhead.wav
    //       └─ Hi Hat              ← HiHat.wav
    // Percussion/
    //   ├─ Djembe/
    //   │   ├─ Bottom              ← 01.Djembe Bottom 47X_01.wav
    //   │   ├─ Top 1               ← 02.Djembe Top 67X_01.wav
    //   │   └─ Top 2               ← Djembe Top Quantized.wav
    //   └─ Sidestick               ← Sidestick.wav (now matches Percussion group)
    // Bass/
    //   ├─ Bass                    ← 03.Bass DI.01_02.wav (DI)
    //   └─ Amp                     ← 04.Bass Amp 47X_01.wav
    // Guitars/
    //   ├─ Acoustic                ← 10.Acoustic 47X_01.wav
    //   ├─ Guitars                 ← 05.Gtr DI.01_02.wav (DI)
    //   ├─ Amp                     ← 06.Gtr Amp.67X_01.wav
    //   ├─ Warren Amp              ← 07.Gtr Warren Amp.01_02.wav
    //   ├─ Solo DI                 ← 08.Gtr Solo DI.01_02.wav
    //   └─ Solo Amp                ← 09.Gtr Solo Amp.67X_01.wav
    // Vocals                       ← 11.Vocal.47X_01.wav
    // Orchestra/
    //   ├─ Strings 1               ← STRINGS HIGH.wav
    //   └─ Strings 2               ← STRINGS MAIN.wav
    // Reference                    ← 12.Ain't No Sunshine Mix_Roswell Mini K47x Mini K67x_02.wav
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
        .track("Kick", "Kick.wav")
        .track("Snare", "Snare.wav")
        .folder("Toms")
        .track("Tom 3", "Tom  3.wav")
        .track("Tom 4", "Tom  4.wav")
        .end()
        .folder("Cymbals")
        .track("OH", "Overhead.wav")
        .track("Hi Hat", "HiHat.wav")
        .end()
        .end()
        .folder("Percussion")
        .folder("Djembe")
        .track("Bottom", "01.Djembe Bottom 47X_01.wav")
        .track("Top 1", "02.Djembe Top 67X_01.wav")
        .track("Top 2", "Djembe Top Quantized.wav")
        .end()
        .track("Sidestick", "Sidestick.wav")
        .end()
        .folder("Bass")
        .track("Bass", "03.Bass DI.01_02.wav")
        .track("Amp", "04.Bass Amp 47X_01.wav")
        .end()
        .folder("Guitars")
        .track("Acoustic", "10.Acoustic 47X_01.wav")
        .track("Guitars", "05.Gtr DI.01_02.wav")
        .track("Amp", "06.Gtr Amp.67X_01.wav")
        .track("Warren Amp", "07.Gtr Warren Amp.01_02.wav")
        .track("Solo DI", "08.Gtr Solo DI.01_02.wav")
        .track("Solo Amp", "09.Gtr Solo Amp.67X_01.wav")
        .end()
        .track("Vocals", "11.Vocal.47X_01.wav")
        .folder("Orchestra")
        .track("Strings 1", "STRINGS HIGH.wav")
        .track("Strings 2", "STRINGS MAIN.wav")
        .end()
        .track(
            "Reference",
            "12.Ain't No Sunshine Mix_Roswell Mini K47x Mini K67x_02.wav",
        )
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
