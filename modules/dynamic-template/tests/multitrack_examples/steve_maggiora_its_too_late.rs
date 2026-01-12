use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn steve_maggiora_its_too_late() -> Result<()> {
    // -- Setup & Fixtures
    let items = vec![
        "01.Kick_02.wav",
        "02.SN TP_02.wav",
        "03.OH.01_02 L.wav",
        "04.OH.01_02 R.wav",
        "05.Bass DI_02.wav",
        "06.Bass_02.wav",
        "07.Acoustic_02.wav",
        "08.Guitar DI Rhythm_01.wav",
        "09.Guitar Rhythm_01.wav",
        "10.Piano_02.wav",
        "11.Gtr Di Solo Michael_02.wav",
        "12.Gtr Amp Solo Michael_02.wav",
        "13.Guitar DI Solo Warren_02.wav",
        "14.Guitar Amp Solo Warren_01.wav",
        "15.Vocal_02.wav",
        "16.H3000.One_02.wav",
        "17.H3000.Two_02.wav",
        "18.H3000.Three_02.wav",
        "19.Vocal.Eko.Plate_02.wav",
        "20.Vocal.Magic_02.wav",
        "21.It's Too Late Mix_02.wav",
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
