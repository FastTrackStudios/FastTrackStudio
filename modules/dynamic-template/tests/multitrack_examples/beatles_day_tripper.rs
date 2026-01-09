use daw::tracks::{TrackGroup, TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

#[test]
fn beatles_day_tripper() {
    // Track list from "The Beatles - Day Tripper"
    let items = vec![
        "bass hofner_01.wav",
        "bass hofner.Duplicate _01.wav",
        "click.Edit_01.wav",
        "fernando strat_02.wav",
        "gretsch fernando_02.wav",
        "kick_01.wav",
        "oh_01.wav",
        "snare_01.wav",
        "Steve BGV1_SUM.06_01.wav",
        "Steve BGV1A.06_01.wav",
        "Steve BGV1B.06_01.wav",
        "Steve BGV1C.06_01.wav",
        "Steve BGV1D.06_01.wav",
        "Steve BGV2_SUM.06_01.wav",
        "Steve BGV2A.06_01.wav",
        "Steve BGV2B.06_01.wav",
        "Steve BGV2C.06_01.wav",
        "Steve BGV2D.06_01.wav",
        "Steve Lead VOX Take 1.06_01.wav",
        "Steve Lead VOX Take 2.06_01.wav",
        "swell_01.wav",
        "tambourine_01.wav",
        "warren strat_02.wav",
    ];

    // Organize into tracks
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
    // - Bass items expand to separate tracks
    // - BGVs items expand to separate tracks
    // - Lead vocals expand to Lead 1, Lead 2

    // --- Drums ---
    // Cymbals folder collapsed since only one cymbal (OH)
    let drums = TrackGroup::folder("Drums")
        .track("Kick", "kick_01.wav")
        .track("Snare", "snare_01.wav")
        .track("OH", "oh_01.wav")
        .end();

    // --- Percussion ---
    // Single track, no folder
    let percussion = TrackGroup::single_track("Percussion", "tambourine_01.wav");

    // --- Bass ---
    // Items expand to separate tracks with variant names
    let bass = TrackGroup::folder("Bass")
        .track("Hofner Guitar", "bass hofner_01.wav")
        .track("Hofner Guitar Duplicate", "bass hofner.Duplicate _01.wav")
        .end();

    // --- Guitars ---
    // Fernando and Warren are performers with their guitar types
    let guitars = TrackGroup::folder("Guitars")
        .start_folder("Fernando")
        .track("Strat", "fernando strat_02.wav")
        .track("Gretsch", "gretsch fernando_02.wav")
        .end()
        .track("Warren", "warren strat_02.wav")
        .end();

    // --- Lead Vocals ---
    // Items expand to Lead 1, Lead 2
    let lead = TrackGroup::folder("Lead")
        .track("Lead 1", "Steve Lead VOX Take 1.06_01.wav")
        .track("Lead 2", "Steve Lead VOX Take 2.06_01.wav")
        .end();

    // --- BGVs ---
    // Items expand to individual tracks with Steve prefix
    let bgvs = TrackGroup::folder("BGVs")
        .track("Steve 1", "Steve BGV1_SUM.06_01.wav")
        .track("Steve 2", "Steve BGV1A.06_01.wav")
        .track("Steve 3", "Steve BGV1B.06_01.wav")
        .track("Steve 4", "Steve BGV1C.06_01.wav")
        .track("Steve 5", "Steve BGV1D.06_01.wav")
        .track("Steve 6", "Steve BGV2_SUM.06_01.wav")
        .track("Steve 7", "Steve BGV2A.06_01.wav")
        .track("Steve 8", "Steve BGV2B.06_01.wav")
        .track("Steve 9", "Steve BGV2C.06_01.wav")
        .track("Steve 10", "Steve BGV2D.06_01.wav")
        .end();

    // --- Vocals parent folder ---
    let vocals = TrackGroup::folder("Vocals").group(lead).group(bgvs).end();

    // --- SFX ---
    let sfx = TrackGroup::single_track("SFX", "swell_01.wav");

    // --- Guide ---
    let guide = TrackGroup::single_track("Guide", "click.Edit_01.wav");

    // ============================================================================
    // Compose final structure
    // ============================================================================

    let expected = TrackStructureBuilder::new()
        .group(drums)
        .group(percussion)
        .group(bass)
        .group(guitars)
        .group(vocals)
        .group(sfx)
        .group(guide)
        .build();

    // Full structure assertion
    assert_tracks_equal(&tracks, &expected).unwrap();
}
