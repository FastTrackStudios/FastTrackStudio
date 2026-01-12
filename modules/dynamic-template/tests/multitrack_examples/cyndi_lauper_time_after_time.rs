use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn cyndi_lauper_time_after_time() -> Result<()> {
    // -- Setup & Fixtures
    // Cyndi Lauper - Time After Time: 18-track pop session with performer names in filenames
    let items = vec![
        "01.LV BECCA.TimeAfterTime.126bpm_01.wav",
        "02.BV LUCA.TimeAfterTime.126bpm_01.wav",
        "03.Acoustic Guitar.Nick.TimeAfterTime.126bpm_01.wav",
        "04.Electric.Gtr.Left.Warren.TimeAfterTime.126bpm_01.wav",
        "05.Electric.Gtr.Right.Warren.TimeAfterTime.126bpm_01.wav",
        "06.BASS AMP.Warren.TimeAfterTime.126bpm_01.wav",
        "07.BASS DI.Warren.TimeAfterTime.126bpm_01.wav",
        "08.Kick.TimeAfterTime.126bpm_01.wav",
        "09.Snare.TimeAfterTime.126bpm_01.wav",
        "10.Kit.Mono.Room.TimeAfterTime.126bpm_01.wav",
        "11.Overhead Hat.TimeAfterTime.126bpm_01.wav",
        "12.Overhead Ride.TimeAfterTime.126bpm_01.wav",
        "13.Room.Left.TimeAfterTime.126bpm_01.wav",
        "14.Room.Right.TimeAfterTime.126bpm_01.wav",
        "15.SHAKER.Left_01.wav",
        "16.SHAKER.Right_01.wav",
        "17.Click.Print_01.wav",
        "18.Time After Time Presonus HD8 Mix_01.wav",
    ];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Expected structure:
    // Drums/
    //   ├─ Kick                ← 08.Kick.TimeAfterTime.126bpm_01.wav
    //   ├─ Snare               ← 09.Snare.TimeAfterTime.126bpm_01.wav
    //   ├─ OH/
    //   │   ├─ Hi Hat          ← 11.Overhead Hat.TimeAfterTime.126bpm_01.wav
    //   │   └─ Ride            ← 12.Overhead Ride.TimeAfterTime.126bpm_01.wav
    //   └─ Rooms/
    //       ├─ L               ← 13.Room.Left.TimeAfterTime.126bpm_01.wav
    //       ├─ R               ← 14.Room.Right.TimeAfterTime.126bpm_01.wav
    //       └─ Mono            ← 10.Kit.Mono.Room.TimeAfterTime.126bpm_01.wav
    // Percussion/
    //   ├─ Shaker 1            ← 15.SHAKER.Left_01.wav
    //   └─ Shaker 2            ← 16.SHAKER.Right_01.wav
    // Bass/
    //   ├─ Warren Amp          ← 06.BASS AMP.Warren.TimeAfterTime.126bpm_01.wav
    //   └─ Warren DI           ← 07.BASS DI.Warren.TimeAfterTime.126bpm_01.wav
    // Guitars/
    //   ├─ Electric/
    //   │   ├─ Left            ← 04.Electric.Gtr.Left.Warren.TimeAfterTime.126bpm_01.wav
    //   │   └─ Right           ← 05.Electric.Gtr.Right.Warren.TimeAfterTime.126bpm_01.wav
    //   └─ Acoustic            ← 03.Acoustic Guitar.Nick.TimeAfterTime.126bpm_01.wav
    // Guide                    ← 17.Click.Print_01.wav
    // Reference                ← 18.Time After Time Presonus HD8 Mix_01.wav
    // Unsorted/
    //   ├─ 01.LV BECCA...      ← 01.LV BECCA.TimeAfterTime.126bpm_01.wav (Lead Vocal - LV not recognized)
    //   └─ 02.BV LUCA...       ← 02.BV LUCA.TimeAfterTime.126bpm_01.wav (Background Vocal - BV not recognized)
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .track("Kick", "08.Kick.TimeAfterTime.126bpm_01.wav")
            .track("Snare", "09.Snare.TimeAfterTime.126bpm_01.wav")
            .folder("OH")
                .track("Hi Hat", "11.Overhead Hat.TimeAfterTime.126bpm_01.wav")
                .track("Ride", "12.Overhead Ride.TimeAfterTime.126bpm_01.wav")
            .end()
            .folder("Rooms")
                .track("L", "13.Room.Left.TimeAfterTime.126bpm_01.wav")
                .track("R", "14.Room.Right.TimeAfterTime.126bpm_01.wav")
                .track("Mono", "10.Kit.Mono.Room.TimeAfterTime.126bpm_01.wav")
            .end()
        .end()
        .folder("Percussion")
            .track("Shaker 1", "15.SHAKER.Left_01.wav")
            .track("Shaker 2", "16.SHAKER.Right_01.wav")
        .end()
        .folder("Bass")
            .track("Warren Amp", "06.BASS AMP.Warren.TimeAfterTime.126bpm_01.wav")
            .track("Warren DI", "07.BASS DI.Warren.TimeAfterTime.126bpm_01.wav")
        .end()
        .folder("Guitars")
            .folder("Electric")
                .track("Left", "04.Electric.Gtr.Left.Warren.TimeAfterTime.126bpm_01.wav")
                .track("Right", "05.Electric.Gtr.Right.Warren.TimeAfterTime.126bpm_01.wav")
            .end()
            .track("Acoustic", "03.Acoustic Guitar.Nick.TimeAfterTime.126bpm_01.wav")
        .end()
        .track("Guide", "17.Click.Print_01.wav")
        .track("Reference", "18.Time After Time Presonus HD8 Mix_01.wav")
        .folder("Unsorted")
            .track("01.LV BECCA.TimeAfterTime.126bpm", "01.LV BECCA.TimeAfterTime.126bpm_01.wav")
            .track("02.BV LUCA.TimeAfterTime.126bpm", "02.BV LUCA.TimeAfterTime.126bpm_01.wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
