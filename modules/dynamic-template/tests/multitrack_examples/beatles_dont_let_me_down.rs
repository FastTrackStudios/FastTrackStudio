use daw::tracks::{assert_tracks_equal, TrackGroup, TrackStructureBuilder};
use dynamic_template::*;

#[test]
fn beatles_dont_let_me_down() {
    // Track list from "The Beatles - Don't Let Me Down"
    let items = vec![
        "01.Kick_01.wav",
        "02.Snare_01.wav",
        "03.OH_01.wav",
        "04.Bass Di_01.wav",
        "05.Bass Amp_01.wav",
        "06.Gtr DI_01.wav",
        "07.Gtr Amp_01.wav",
        "08.Keys_01.wav",
        "09.Vi.Vocal_01.wav",
        "10.Caitlin Vocal_01.wav",
        "11.Don't Let Me Down Joe Carrell Mix_01.wav",
    ];

    // Organize into tracks using monarchy sort
    let config = default_config();
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // Display the track list
    println!("\nTrack list:");
    daw::tracks::display_tracklist(&tracks);

    // ============================================================================
    // Expected structure (WITH EXPANSION)
    // ============================================================================
    // With default expansion enabled:
    // - Cymbals folder collapses to OH (single child)
    // - Bass items expand to separate tracks (Bass DI, Bass Amp)
    // - Guitar items expand to separate tracks (Guitar DI, Guitar Amp)
    // - Lead vocals expand to Lead 1, Lead 2

    // --- Drums ---
    // Cymbals folder collapsed since only one cymbal (OH)
    let drums = TrackGroup::folder("Drums")
        .track("Kick", "01.Kick_01.wav")
        .track("Snare", "02.Snare_01.wav")
        .track("OH", "03.OH_01.wav")
        .end();

    // --- Bass ---
    // Items expanded to separate tracks
    let bass = TrackGroup::folder("Bass")
        .track("Bass", "04.Bass Di_01.wav")
        .track("Amp", "05.Bass Amp_01.wav")
        .end();

    // --- Guitars ---
    // Items expanded to separate tracks
    let guitars = TrackGroup::folder("Guitars")
        .track("Guitars", "06.Gtr DI_01.wav")
        .track("Amp", "07.Gtr Amp_01.wav")
        .end();

    // --- Keys ---
    let keys = TrackGroup::single_track("Keys", "08.Keys_01.wav");

    // --- Lead Vocals ---
    // Lead collapses into Vocals folder when it's the only vocal type
    // Child tracks keep "Lead 1", "Lead 2" names since they're in the Lead grouping
    let lead = TrackGroup::folder("Vocals")
        .track("Lead 1", "09.Vi.Vocal_01.wav")
        .track("Lead 2", "10.Caitlin Vocal_01.wav")
        .end();

    // --- Reference ---
    let reference =
        TrackGroup::single_track("Reference", "11.Don't Let Me Down Joe Carrell Mix_01.wav");

    // ============================================================================
    // Compose final structure
    // ============================================================================

    let expected = TrackStructureBuilder::new()
        .group(drums)
        .group(bass)
        .group(guitars)
        .group(keys)
        .group(lead)
        .group(reference)
        .build();

    // Full structure assertion
    assert_tracks_equal(&tracks, &expected).unwrap();
}
