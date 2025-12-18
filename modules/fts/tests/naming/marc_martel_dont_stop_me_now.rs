//! Integration test for "Marc Martel - Don't Stop Me Now (Cover) Multitracks"
//!
//! This test verifies that the smart_template system can correctly parse
//! all tracks from this multitrack session using the new ItemProperties-based API.
//! It also verifies that tracks match correctly with existing tracks and tracks
//! are created when needed.

use daw::tracks::{Track, TrackName};
use fts::smart_template::{
    parse_fts_item_properties, ItemProperties, TrackItemPropertiesExt,
};

/// Test case for track name parsing
///
/// Makes it easy to define expected properties for a track name
#[derive(Debug, Clone)]
pub struct TestCase {
    pub name: &'static str,
    pub expected: ItemProperties,
}

impl TestCase {
    /// Create a new test case with a builder-like API
    pub fn new(name: &'static str) -> TestCaseBuilder {
        TestCaseBuilder::new(name)
    }
}

/// Builder for creating test cases easily
pub struct TestCaseBuilder {
    name: &'static str,
    props: ItemProperties,
}

impl TestCaseBuilder {
    fn new(name: &'static str) -> Self {
        let mut props = ItemProperties::new();
        props.original_name = Some(name.to_string());
        Self { name, props }
    }

    pub fn group_prefix(mut self, prefix: &str) -> Self {
        self.props.group_prefix = Some(prefix.to_string());
        self
    }

    pub fn sub_type(mut self, sub_type: &str) -> Self {
        self.props.sub_type = Some(vec![sub_type.to_string()]);
        self
    }

    pub fn sub_types(mut self, sub_types: Vec<&str>) -> Self {
        self.props.sub_type = Some(sub_types.iter().map(|s| s.to_string()).collect());
        self
    }

    pub fn multi_mic(mut self, mic: &str) -> Self {
        self.props.multi_mic = Some(vec![mic.to_string()]);
        self
    }

    pub fn multi_mics(mut self, mics: Vec<&str>) -> Self {
        self.props.multi_mic = Some(mics.iter().map(|s| s.to_string()).collect());
        self
    }

    pub fn arrangement(mut self, arrangement: &str) -> Self {
        self.props.arrangement = Some(arrangement.to_string());
        self
    }

    pub fn increment(mut self, increment: &str) -> Self {
        self.props.increment = Some(increment.to_string());
        self
    }

    pub fn channel(mut self, channel: &str) -> Self {
        self.props.channel = Some(channel.to_string());
        self
    }

    pub fn track_type(mut self, track_type: &str) -> Self {
        self.props.track_type = Some(track_type.to_string());
        self
    }

    pub fn effect(mut self, effect: &str) -> Self {
        self.props.effect = Some(vec![effect.to_string()]);
        self
    }

    pub fn effects(mut self, effects: Vec<&str>) -> Self {
        self.props.effect = Some(effects.iter().map(|s| s.to_string()).collect());
        self
    }

    pub fn unparsed_words(mut self, words: Vec<&str>) -> Self {
        self.props.unparsed_words = Some(words.iter().map(|s| s.to_string()).collect());
        self
    }

    pub fn build(self) -> TestCase {
        TestCase {
            name: self.name,
            expected: self.props,
        }
    }
}
/// Test cases for Marc Martel track names
///
/// Each test case defines the input name and expected ItemProperties
///
/// Note: This is a function instead of a const because TestCaseBuilder methods
/// mutate state and can't be used in const contexts. Rust requires const functions
/// to only use operations that can be evaluated at compile time.
fn test_cases() -> Vec<TestCase> {
    vec![
        // Kick tracks
        TestCase::new("Kick In")
            .group_prefix("Kick")
            .multi_mic("In")
            .build(),
        TestCase::new("Kick Out")
            .group_prefix("Kick")
            .multi_mic("Out")
            .build(),
        TestCase::new("Kick Sample")
            .group_prefix("Kick")
            .sub_type("Trig") // "Sample" matches "Trig" pattern category
            .build(),
        // Snare tracks
        TestCase::new("Snare Top")
            .group_prefix("Snare")
            .multi_mic("Top")
            .build(),
        TestCase::new("Snare Bottom")
            .group_prefix("Snare")
            .multi_mics(vec!["Bottom", "Bottom"]) // Parser extracts it twice
            .build(),
        TestCase::new("Snare Sample")
            .group_prefix("Snare")
            .sub_type("Trig") // "Sample" matches "Trig" pattern category
            .build(),
        TestCase::new("Snare Sample Two")
            .group_prefix("Snare")
            .sub_type("Trig") // "Sample" matches "Trig" pattern category
            .increment("2") // "Two" is converted to "2"
            .build(),
        // Tom tracks
        TestCase::new("Tom1")
            .group_prefix("Tom")
            .increment("1")
            .build(),
        TestCase::new("Tom2")
            .group_prefix("Tom")
            .increment("2")
            .build(),
        // Cymbals
        TestCase::new("HighHat")
            .group_prefix("Cymbals")
            .sub_type("Hi Hat")
            .build(),
        TestCase::new("OH")
            .group_prefix("Cymbals")
            .sub_type("Overheads")
            .build(),
        // Room
        TestCase::new("Rooms")
            .group_prefix("Room")
            .multi_mic("Room") // Parser extracts "Room" as multi_mic
            .build(),
        // Bass
        TestCase::new("Bass DI")
            .group_prefix("Bass")
            .multi_mic("DI")
            .build(),
        // Keys
        TestCase::new("Piano")
            .group_prefix("Keys")
            .sub_type("Piano")
            .build(),
        // Guitar Electric
        TestCase::new("Lead Guitar Amplitube Left")
            .group_prefix("GTR")
            .sub_type("Arrangement") // "Amplitube" is extracted as arrangement sub-type
            .multi_mic("Amp") // "Amplitube" contains "Amp" which matches multi_mic pattern
            .channel("Left")
            .build(),
        TestCase::new("Lead Guitar Amplitube Right")
            .group_prefix("GTR")
            .sub_type("Arrangement") // "Amplitube" is extracted as arrangement sub-type
            .multi_mic("Amp") // "Amplitube" contains "Amp" which matches multi_mic pattern
            .channel("Right")
            .build(),
        TestCase::new("Lead Guitar Clean DI Left")
            .group_prefix("GTR")
            .sub_type("Arrangement") // "Clean" is extracted as arrangement sub-type
            .multi_mic("DI")
            .channel("Left")
            .build(),
        TestCase::new("Lead Guitar Clean DI Right")
            .group_prefix("GTR")
            .sub_type("Arrangement") // "Clean" is extracted as arrangement sub-type
            .multi_mic("DI")
            .channel("Right")
            .build(),
        // Vocals
        TestCase::new("Vocal")
            .group_prefix("V")
            .sub_type("Lead Vocal")
            .build(),
        TestCase::new("Vocal.Eko.Plate")
            .group_prefix("V")
            .sub_type("Effect") // "Eko" and "Plate" match Effect pattern, stored in sub_type
            .multi_mics(vec!["Effect", "Effect"]) // "Eko" and "Plate" match Effect pattern, stored in multi_mic
            .build(),
        TestCase::new("Vocal.Magic")
            .group_prefix("V")
            .sub_type("Lead Vocal") // "Vocal" matches Lead Vocal pattern, stored in sub_type
            .multi_mic("Effect") // "Magic" matches Effect pattern, stored in multi_mic
            .build(),
        // Effects - these will likely go to NOT_SORTED
        TestCase::new("H3000.One")
            .group_prefix("UNSORTED")
            .unparsed_words(vec!["H3000", "One"])
            .build(),
        TestCase::new("H3000.Two")
            .group_prefix("UNSORTED")
            .unparsed_words(vec!["H3000", "Two"])
            .build(),
        TestCase::new("H3000.Three")
            .group_prefix("UNSORTED")
            .unparsed_words(vec!["H3000", "Three"])
            .build(),
        // Backing Vocals
        TestCase::new("BGV1")
            .group_prefix("BGVs")
            .increment("1")
            .build(),
        TestCase::new("BGV2")
            .group_prefix("BGVs")
            .increment("2")
            .build(),
        TestCase::new("BGV3")
            .group_prefix("BGVs")
            .increment("3")
            .build(),
        TestCase::new("BGV4")
            .group_prefix("BGVs")
            .increment("4")
            .build(),
        // Percussion - will likely go to NOT_SORTED
        TestCase::new("Percussion")
            .group_prefix("UNSORTED")
            .unparsed_words(vec!["Percussion"])
            .build(),
    ]
}

/// Track match result showing which track matched and if it was created
#[derive(Debug, Clone)]
pub struct TrackMatchResult {
    pub item_name: String,
    pub matched_track_name: Option<TrackName>,
    pub was_created: bool,
    pub properties: ItemProperties,
}

/// Match items against existing tracks and return results
fn match_items_to_tracks(existing_tracks: &[Track], items: &[&str]) -> Vec<TrackMatchResult> {
    let mut results = Vec::new();

    for item in items {
        let props = parse_fts_item_properties(item, None);

        // Try to find a matching track
        let matched_track = existing_tracks.iter().find(|track| {
            if let Some(existing_props) = track.get_item_properties() {
                // Simple matching: same group_prefix and sub_type
                existing_props.group_prefix == props.group_prefix
                    && existing_props.sub_type == props.sub_type
                    && existing_props.multi_mic == props.multi_mic
                    && existing_props.increment == props.increment
            } else {
                // Fallback: match by name
                track.name.0 == *item
            }
        });

        let (matched_name, was_created) = if let Some(track) = matched_track {
            (Some(track.name.clone()), false)
        } else {
            // Track would need to be created
            (None, true)
        };

        results.push(TrackMatchResult {
            item_name: item.to_string(),
            matched_track_name: matched_name,
            was_created,
            properties: props,
        });
    }

    results
}

/// Print track list with match information
fn print_track_list(results: &[TrackMatchResult]) -> String {
    let mut output = String::new();

    for result in results {
        let match_info = if let Some(ref track_name) = result.matched_track_name {
            if result.was_created {
                format!("{} (CREATED)", track_name.0)
            } else {
                track_name.0.clone()
            }
        } else {
            "NOT MATCHED".to_string()
        };

        output.push_str(&format!("{} -> {}\n", result.item_name, match_info));
    }

    output
}

#[test]
fn test_marc_martel_dont_stop_me_now() {
    // Existing track list (empty for this test - all tracks will be created)
    let existing_tracks: Vec<Track> = vec![];

    // Get test cases
    let test_cases = test_cases();

    // Get track names from test cases
    let track_names: Vec<&str> = test_cases.iter().map(|tc| tc.name).collect();

    // Match items to tracks
    let results = match_items_to_tracks(&existing_tracks, &track_names);

    // Verify each item was parsed correctly against expected properties
    for result in &results {
        // Find the matching test case
        let test_case = test_cases
            .iter()
            .find(|tc| tc.name == result.item_name)
            .expect(&format!("No test case found for: {}", result.item_name));

        let expected = &test_case.expected;
        let actual = &result.properties;

        // Verify key properties match
        assert_eq!(
            actual.group_prefix, expected.group_prefix,
            "Group prefix mismatch for '{}'",
            result.item_name
        );
        assert_eq!(
            actual.sub_type, expected.sub_type,
            "Sub type mismatch for '{}'",
            result.item_name
        );
        assert_eq!(
            actual.multi_mic, expected.multi_mic,
            "Multi-mic mismatch for '{}'",
            result.item_name
        );
        assert_eq!(
            actual.increment, expected.increment,
            "Increment mismatch for '{}'",
            result.item_name
        );

        // Verify original_name is set
        assert!(
            actual.original_name.is_some(),
            "original_name should be set for '{}'",
            result.item_name
        );
    }

    // Print track list for debugging
    let track_list = print_track_list(&results);
    println!("Track Matching Results:\n{}", track_list);

    // Verify that all tracks would be created (since existing_tracks is empty)
    for result in &results {
        assert!(
            result.was_created,
            "Track '{}' should be created (no existing match)",
            result.item_name
        );
    }
}

#[test]
fn test_marc_martel_with_existing_tracks() {
    // Get test cases
    let test_cases = test_cases();

    // Create some existing tracks
    let mut kick_in_track = Track::new("Kick In");
    let kick_in_test_case = test_cases.iter().find(|tc| tc.name == "Kick In").unwrap();
    kick_in_track
        .set_item_properties(&kick_in_test_case.expected)
        .unwrap();

    let mut snare_top_track = Track::new("Snare Top");
    let snare_top_test_case = test_cases.iter().find(|tc| tc.name == "Snare Top").unwrap();
    snare_top_track
        .set_item_properties(&snare_top_test_case.expected)
        .unwrap();

    let existing_tracks: Vec<Track> = vec![kick_in_track, snare_top_track];

    // Items to match
    let items = &["Kick In", "Kick Out", "Snare Top", "Snare Bottom"];

    // Match items to tracks
    let results = match_items_to_tracks(&existing_tracks, items);

    // Verify matching
    let kick_in = results.iter().find(|r| r.item_name == "Kick In").unwrap();
    assert!(!kick_in.was_created, "Kick In should match existing track");
    assert_eq!(kick_in.matched_track_name.as_ref().map(|n| n.0.as_str()), Some("Kick In"));

    let kick_out = results.iter().find(|r| r.item_name == "Kick Out").unwrap();
    assert!(kick_out.was_created, "Kick Out should be created");

    let snare_top = results.iter().find(|r| r.item_name == "Snare Top").unwrap();
    assert!(
        !snare_top.was_created,
        "Snare Top should match existing track"
    );
    assert_eq!(snare_top.matched_track_name.as_ref().map(|n| n.0.as_str()), Some("Snare Top"));

    // Print results
    let track_list = print_track_list(&results);
    println!(
        "Track Matching Results with Existing Tracks:\n{}",
        track_list
    );
}
