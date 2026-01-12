//! Tests to verify that `assert_tracks_equal` catches minute differences.
//!
//! These tests use `#[should_panic]` to verify that small mismatches are detected.
//! If any of these tests pass without panicking, it means our assertion is not
//! sensitive enough to catch the specified type of error.

use daw::tracks::{TrackStructureBuilder, assert_tracks_equal};
use dynamic_template::*;

type Result<T> = core::result::Result<T, Box<dyn std::error::Error>>;

// ============================================================================
// Tests that SHOULD PANIC (verifying assertion sensitivity)
// ============================================================================

#[test]
#[should_panic(expected = "name - expected 'Oat', got 'Out'")]
fn detects_single_letter_difference_in_track_name() {
    // -- Setup & Fixtures
    let items = vec!["Kick In", "Kick Out"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // -- Check: "Out" vs "Oat" - single letter difference
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
        .track("In", "Kick In")
        .track("Oat", "Kick Out") // Wrong: should be "Out"
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected).unwrap();
}

#[test]
#[should_panic(expected = "name - expected 'in', got 'In'")]
fn detects_case_difference_in_track_name() {
    // -- Setup & Fixtures
    let items = vec!["Kick In", "Kick Out"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // -- Check: "In" vs "in" - case difference
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
        .track("in", "Kick In") // Wrong: should be "In" (capital I)
        .track("Out", "Kick Out")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected).unwrap();
}

#[test]
#[should_panic(expected = "name - expected 'Kock', got 'Kick'")]
fn detects_folder_name_typo() {
    // -- Setup & Fixtures
    let items = vec!["Kick In", "Kick Out"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // -- Check: "Kick" vs "Kock" - typo in folder name
    let expected = TrackStructureBuilder::new()
        .folder("Kock") // Wrong: should be "Kick"
        .track("In", "Kick In")
        .track("Out", "Kick Out")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected).unwrap();
}

#[test]
#[should_panic(expected = "items - expected")]
fn detects_wrong_item_on_track() {
    // -- Setup & Fixtures
    let items = vec!["Kick In", "Kick Out"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // -- Check: Wrong item assigned to track
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
        .track("In", "Kick Out") // Wrong: should be "Kick In"
        .track("Out", "Kick Out")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected).unwrap();
}

#[test]
#[should_panic(expected = "count mismatch")]
fn detects_missing_track() {
    // -- Setup & Fixtures
    let items = vec!["Kick In", "Kick Out"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // -- Check: Missing "Out" track
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
        .track("In", "Kick In")
        // Missing: .track("Out", "Kick Out")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected).unwrap();
}

#[test]
#[should_panic(expected = "count mismatch")]
fn detects_extra_track() {
    // -- Setup & Fixtures
    let items = vec!["Kick In"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // -- Check: Extra track that doesn't exist
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
        .track("In", "Kick In")
        .track("Out", "Kick Out") // Wrong: this track doesn't exist
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected).unwrap();
}

#[test]
#[should_panic(expected = "mismatch")]
fn detects_wrong_hierarchy_depth() {
    // -- Setup & Fixtures
    let items = vec!["Kick In", "Kick Out", "Snare Top", "Snare Bottom"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // -- Check: Flat structure instead of nested Drums folder
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
        .track("In", "Kick In")
        .track("Out", "Kick Out")
        .end()
        .folder("Snare")
        .track("Top", "Snare Top")
        .track("Bottom", "Snare Bottom")
        .end()
        .build();

    // This should fail because actual has Drums as parent folder
    assert_tracks_equal(&tracks, &expected).unwrap();
}

#[test]
#[should_panic(expected = "name - expected 'In ', got 'In'")]
fn detects_extra_whitespace_in_name() {
    // -- Setup & Fixtures
    let items = vec!["Kick In", "Kick Out"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None).unwrap();

    // -- Check: Extra space in track name
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
        .track("In ", "Kick In") // Wrong: trailing space
        .track("Out", "Kick Out")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected).unwrap();
}

// ============================================================================
// Control test: This one SHOULD PASS (verifying test setup is correct)
// ============================================================================

#[test]
fn control_test_correct_expectation_passes() -> Result<()> {
    // -- Setup & Fixtures
    let items = vec!["Kick In", "Kick Out"];
    let config = default_config();

    // -- Exec
    let tracks = items.organize_into_tracks(&config, None)?;

    // -- Check: Correct expectation
    let expected = TrackStructureBuilder::new()
        .folder("Kick")
        .track("In", "Kick In")
        .track("Out", "Kick Out")
        .end()
        .build();

    assert_tracks_equal(&tracks, &expected)?;

    Ok(())
}
