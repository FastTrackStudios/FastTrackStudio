//! Integration test for "Marc Martel - Don't Stop Me Now (Cover) Multitracks"
//!
//! This test verifies that the smart_template system can correctly parse
//! all tracks from this multitrack session using the new ItemProperties-based API.

use fts::smart_template::naming::parse_fts_item_properties;

/// Track names from "Marc Martel - Don't Stop Me Now (Cover) Multitracks"
const TRACK_NAMES: &[&str] = &[
    "Kick In",
    "Kick Out",
    "Kick Sample",
    "Snare Top",
    "Snare Bottom",
    "Snare Sample",
    "Snare Sample Two",
    "Tom1",
    "Tom2",
    "HighHat",
    "OH",
    "Rooms",
];

#[test]
fn test_marc_martel_dont_stop_me_now() {
    // Parse each track name using the new API
    for track_name in TRACK_NAMES {
        let props = parse_fts_item_properties(track_name, None);
        
        // Verify that parsing was successful (has meaningful components)
        assert!(
            props.is_valid(),
            "Failed to parse track name: {}",
            track_name
        );
        
        // Verify that we got meaningful parsed data
        // The exact values depend on the track name, but we should have at least
        // group_prefix, sub_type, or other meaningful components for valid track names
        // Some names like "Tom1" might only have increment extracted, which is still valid
        assert!(
            props.group_prefix.is_some() 
                || props.sub_type.is_some()
                || props.increment.is_some()
                || props.multi_mic.is_some()
                || props.arrangement.is_some(),
            "Parsed track name '{}' should have at least one meaningful component. Got: group_prefix={:?}, sub_type={:?}, increment={:?}, multi_mic={:?}, arrangement={:?}",
            track_name,
            props.group_prefix,
            props.sub_type,
            props.increment,
            props.multi_mic,
            props.arrangement
        );
    }
}
