use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn iron_maiden_the_trooper() -> Result<()> {
    // -- Setup & Fixtures
    // Iron Maiden - The Trooper: 14-track metal session with stereo rhythm guitars and layered vocals
    let items = vec![
        "08-Kick-TheTrooper.wav",
        "09-Snare-TheTrooper.wav",
        "10-OH-TheTrooper.wav",
        "12-Bass DI-TheTrooper.wav",
        "13-Bass Amp-TheTrooper.wav",
        "15-Rhy Gtr L-TheTrooper.wav",
        "16-Rhy Gtr R-TheTrooper.wav",
        "17-Solo 1-TheTrooper.wav",
        "18-Solo 2-TheTrooper.wav",
        "19-Solo 3-TheTrooper.wav",
        "21-Vocal 1-TheTrooper.wav",
        "22-Vocal 2-TheTrooper.wav",
        "23-Vocal 3-TheTrooper.wav",
        "Trooper-mix1.wav",
    ];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Expected structure:
    // Drums/
    //   ├─ Kick                    ← 08-Kick-TheTrooper.wav
    //   ├─ Snare                   ← 09-Snare-TheTrooper.wav
    //   └─ OH                      ← 10-OH-TheTrooper.wav
    // Bass/
    //   ├─ Bass                    ← 12-Bass DI-TheTrooper.wav (DI)
    //   └─ Amp                     ← 13-Bass Amp-TheTrooper.wav
    // Guitars/
    //   ├─ 15-Rhy L-TheTrooper     ← 15-Rhy Gtr L-TheTrooper.wav (Rhythm L)
    //   └─ 16-Rhy R-TheTrooper     ← 16-Rhy Gtr R-TheTrooper.wav (Rhythm R)
    // Vocals/
    //   ├─ Vocals 1                ← 21-Vocal 1-TheTrooper.wav
    //   ├─ Vocals 2                ← 22-Vocal 2-TheTrooper.wav
    //   └─ Vocals 3                ← 23-Vocal 3-TheTrooper.wav
    // Reference                    ← Trooper-mix1.wav
    // Unsorted/
    //   ├─ 17-Solo 1-TheTrooper    ← 17-Solo 1-TheTrooper.wav (guitar solo - ambiguous)
    //   ├─ 18-Solo 2-TheTrooper    ← 18-Solo 2-TheTrooper.wav
    //   └─ 19-Solo 3-TheTrooper    ← 19-Solo 3-TheTrooper.wav
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .track("Kick", "08-Kick-TheTrooper.wav")
            .track("Snare", "09-Snare-TheTrooper.wav")
            .track("OH", "10-OH-TheTrooper.wav")
        .end()
        .folder("Bass")
            .track("Bass", "12-Bass DI-TheTrooper.wav")
            .track("Amp", "13-Bass Amp-TheTrooper.wav")
        .end()
        .folder("Guitars")
            .track("15-Rhy L-TheTrooper", "15-Rhy Gtr L-TheTrooper.wav")
            .track("16-Rhy R-TheTrooper", "16-Rhy Gtr R-TheTrooper.wav")
        .end()
        .folder("Vocals")
            .track("Vocals 1", "21-Vocal 1-TheTrooper.wav")
            .track("Vocals 2", "22-Vocal 2-TheTrooper.wav")
            .track("Vocals 3", "23-Vocal 3-TheTrooper.wav")
        .end()
        .track("Reference", "Trooper-mix1.wav")
        .folder("Unsorted")
            .track("17-Solo 1-TheTrooper", "17-Solo 1-TheTrooper.wav")
            .track("18-Solo 2-TheTrooper", "18-Solo 2-TheTrooper.wav")
            .track("19-Solo 3-TheTrooper", "19-Solo 3-TheTrooper.wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
