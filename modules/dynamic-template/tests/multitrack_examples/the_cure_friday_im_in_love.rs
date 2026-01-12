use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

#[test]
fn the_cure_friday_im_in_love() -> Result<()> {
    // -- Setup & Fixtures
    // The Cure - Friday I'm In Love: Pop rock with multiple guitar overdubs and harmonies
    let items = vec![
        "01.Kick_01.wav",
        "02.Snare_01.wav",
        "03.Ohd_01 L.wav",
        "04.Ohd_01 R.wav",
        "05.Tambo_01.wav",
        "06.Bass DI_01.wav",
        "07.Bass Amp_01.wav",
        "08.Acoustic_01.wav",
        "09.Acoustic.dbl_01.wav",
        "10.Acoustic OD_01.wav",
        "11.Acoustic OD Dbl_01.wav",
        "12.Gtr 1 DI_01.wav",
        "13.Gtr 1 Amp_01.wav",
        "14.Gtr 2 DI_01.wav",
        "15.Gtr 2 Amp_01.wav",
        "16.Gtr 2 DI Overlap_01.wav",
        "17.Gtr 2 Amp Overlap_01.wav",
        "18.Gtr Solo DI_01.wav",
        "19.Gtr Solo Amp_01.wav",
        "20.Gtr Root Note DI_01.wav",
        "21.Gtr Root Note Amp_01.wav",
        "23.Pad_01.wav",
        "24.Vocal_01.wav",
        "25.Vocal Overlap_01.wav",
        "26. Vocal Dbl_01.wav",
        "27.HARM_01.wav",
        "28.Harm Overlap_01.wav",
        "29.Friday I'm In Love Mix_01.wav",
    ];

    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    let expected = TrackStructureBuilder::new()
        .folder("Drums")
            .track("Kick", "01.Kick_01.wav")
            .track("Snare", "02.Snare_01.wav")
        .end()
        .folder("Bass")
            .track("Bass", "06.Bass DI_01.wav")
            .track("Amp", "07.Bass Amp_01.wav")
        .end()
        .folder("Guitars")
            .folder("Acoustic")
                .track("Acoustic 1", "08.Acoustic_01.wav")
                .track("Acoustic 2", "09.Acoustic.dbl_01.wav")
                .track("Acoustic 3", "10.Acoustic OD_01.wav")
                .track("Acoustic 4", "11.Acoustic OD Dbl_01.wav")
            .end()
            .track("Guitars 1", "12.Gtr 1 DI_01.wav")
            .track("Amp 1", "13.Gtr 1 Amp_01.wav")
            .track("Guitars 2", "14.Gtr 2 DI_01.wav")
            .track("Amp 2 1", "15.Gtr 2 Amp_01.wav")
            .track("Guitars 3", "16.Gtr 2 DI Overlap_01.wav")
            .track("Amp 2 2", "17.Gtr 2 Amp Overlap_01.wav")
            .track("Solo DI", "18.Gtr Solo DI_01.wav")
            .track("Solo Amp", "19.Gtr Solo Amp_01.wav")
            .track("Guitars 4", "20.Gtr Root Note DI_01.wav")
            .track("Amp", "21.Gtr Root Note Amp_01.wav")
        .end()
        .track("Synths", "23.Pad_01.wav")
        .folder("Vocals")
            .folder("Main")
                .track("Lead 1", "24.Vocal_01.wav")
                .track("Lead 2", "25.Vocal Overlap_01.wav")
            .end()
            .track("DBL", "26. Vocal Dbl_01.wav")
        .end()
        .track("Reference", "29.Friday I'm In Love Mix_01.wav")
        .folder("Unsorted")
            .track("Ohd_01 L", "03.Ohd_01 L.wav")
            .track("Ohd_01 R", "04.Ohd_01 R.wav")
            .track("Tambo", "05.Tambo_01.wav")
            .track("HARM", "27.HARM_01.wav")
            .track("Harm Overlap", "28.Harm Overlap_01.wav")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
