use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn gloria_anderson_households() -> Result<()> {
    // -- Setup & Fixtures
    // Gloria Anderson - Households: 22-track session with Nashville guitars and string arrangements
    let items = vec![
        "01.Households ACO JC MIXX_01.wav",
        "02.Kick LIM_01.wav",
        "03.Kick Out LIM_01.wav",
        "04.Sn T_01.wav",
        "05.Sn B_01.wav",
        "06.Hat_01.wav",
        "07.Tom 1 EDT_01.wav",
        "08.Tom 2 EDT_01.wav",
        "09.OH_01.wav",
        "10.Room_01.wav",
        "11.Bass Body_01.wav",
        "12.Bass Neck_01.wav",
        "13.AG 1_01.wav",
        "14.Piano_01.wav",
        "15.Steel_01.wav",
        "16.Vocal_01.wav",
        "Choir.wav",
        "Households_TimHoek_NashGtrL.wav",
        "Households_TimHoek_NashGtrR.wav",
        "Households_TimHoek_Strings.wav",
        "Strings (R).wav",
        "Vocal harmony (D).wav",
    ];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // Expected structure:
    // Drums/
    //   ├─ Kick/
    //   │   ├─ SUM                  ← 03.Kick Out LIM
    //   │   └─ Lim                  ← 02.Kick LIM
    //   ├─ Snare/
    //   │   ├─ Snare 1              ← 04.Sn T (Top)
    //   │   └─ Snare 2              ← 05.Sn B (Bottom)
    //   ├─ Toms/
    //   │   ├─ T1                   ← 07.Tom 1 EDT
    //   │   └─ T2                   ← 08.Tom 2 EDT
    //   ├─ Cymbals/
    //   │   ├─ OH                   ← 09.OH
    //   │   └─ Hi Hat               ← 06.Hat
    //   └─ Rooms                    ← 10.Room
    // Bass/                         ← Body, Neck
    // Guitars/
    //   ├─ Acoustic/
    //   │   ├─ Acoustic 1           ← 01.Households ACO JC MIXX
    //   │   └─ Acoustic 2           ← 13.AG 1
    //   ├─ Steel                    ← 15.Steel
    //   ├─ Tim L                    ← Households_TimHoek_NashGtrL
    //   └─ Tim R                    ← Households_TimHoek_NashGtrR
    // Keys                          ← 14.Piano
    // Vocals/
    //   ├─ Lead                     ← 16.Vocal
    //   └─ BGVs/                    ← Choir, Vocal harmony (D)
    // Orchestra/
    //   ├─ Strings Tim              ← Households_TimHoek_Strings
    //   └─ Strings                  ← Strings (R)
    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .folder("Kick")
                .track("SUM", "03.Kick Out LIM_01.wav")
                .track("Lim", "02.Kick LIM_01.wav")
            .end()
            .folder("Snare")
                .track("Snare 1", "04.Sn T_01.wav")
                .track("Snare 2", "05.Sn B_01.wav")
            .end()
            .folder("Toms")
                .track("T1", "07.Tom 1 EDT_01.wav")
                .track("T2", "08.Tom 2 EDT_01.wav")
            .end()
            .folder("Cymbals")
                .track("OH", "09.OH_01.wav")
                .track("Hi Hat", "06.Hat_01.wav")
            .end()
            .track("Rooms", "10.Room_01.wav")
        .end()
        .folder("Bass")
            .track("Bass 1", "11.Bass Body_01.wav")
            .track("Bass 2", "12.Bass Neck_01.wav")
        .end()
        .folder("Guitars")
            .folder("Acoustic")
                .track("Acoustic 1", "01.Households ACO JC MIXX_01.wav")
                .track("Acoustic 2", "13.AG 1_01.wav")
            .end()
            .track("Steel", "15.Steel_01.wav")
            .track("Tim L", "Households_TimHoek_NashGtrL.wav")
            .track("Tim R", "Households_TimHoek_NashGtrR.wav")
        .end()
        .track("Keys", "14.Piano_01.wav")
        .folder("Vocals")
            .track("Lead", "16.Vocal_01.wav")
            .folder("BGVs")
                .track("BGVs 1", "Choir.wav")
                .track("BGVs 2", "Vocal harmony (D).wav")
            .end()
        .end()
        .folder("Orchestra")
            .track("Strings Tim", "Households_TimHoek_Strings.wav")
            .track("Strings", "Strings (R).wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
