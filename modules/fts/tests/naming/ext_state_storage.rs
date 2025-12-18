//! Tests for storing and retrieving ItemProperties in Track.ext_state
//!
//! This test verifies that ItemProperties can be correctly serialized to JSON
//! and stored in Track.ext_state, then deserialized back.

use fts::smart_template::naming::{
    parse_fts_item_properties,
    ItemProperties,
    TrackItemPropertiesExt,
};
use daw::tracks::Track;

#[test]
fn test_store_and_retrieve_item_properties() {
    let track_name = "D Kick In .02";
    let props = parse_fts_item_properties(track_name, None);
    
    // Create a track
    let mut track = {
        let mut builder = Track::builder();
        builder
            .name(track_name.to_string())
            .volume(1.0)
            .pan(0.0)
            .muted(false)
            .solo_state(daw::tracks::api::solo::SoloMode::Off)
            .automation_mode(daw::tracks::api::automation::AutomationMode::TrimRead)
            .invert_phase(false)
            .show_in_mixer(true)
            .show_in_track_list(true);
        builder.build()
    };
    
    // Store ItemProperties in ext_state
    track.set_item_properties(&props)
        .expect("Failed to store ItemProperties");
    
    // Verify ext_state is set
    assert!(
        track.ext_state.is_some(),
        "ext_state should be set after storing ItemProperties"
    );
    
    // Retrieve ItemProperties from ext_state
    let retrieved = track.get_item_properties()
        .expect("Failed to retrieve ItemProperties from ext_state");
    
    // Verify all fields match
    assert_eq!(retrieved.original_name, props.original_name);
    assert_eq!(retrieved.group_prefix, props.group_prefix);
    assert_eq!(retrieved.sub_type, props.sub_type);
    assert_eq!(retrieved.performer, props.performer);
    assert_eq!(retrieved.arrangement, props.arrangement);
    assert_eq!(retrieved.section, props.section);
    assert_eq!(retrieved.layers, props.layers);
    assert_eq!(retrieved.multi_mic, props.multi_mic);
    assert_eq!(retrieved.effect, props.effect);
    assert_eq!(retrieved.increment, props.increment);
    assert_eq!(retrieved.channel, props.channel);
    assert_eq!(retrieved.playlist, props.playlist);
    assert_eq!(retrieved.track_type, props.track_type);
    assert_eq!(retrieved.unparsed_words, props.unparsed_words);
    assert_eq!(retrieved.file_extension, props.file_extension);
}

#[test]
fn test_store_multiple_times_overwrites() {
    let track_name = "Bass Guitar DI";
    let mut track = {
        let mut builder = Track::builder();
        builder
            .name(track_name.to_string())
            .volume(1.0)
            .pan(0.0)
            .muted(false)
            .solo_state(daw::tracks::api::solo::SoloMode::Off)
            .automation_mode(daw::tracks::api::automation::AutomationMode::TrimRead)
            .invert_phase(false)
            .show_in_mixer(true)
            .show_in_track_list(true);
        builder.build()
    };
    
    // Store first ItemProperties
    let props1 = parse_fts_item_properties("Bass Guitar DI", None);
    track.set_item_properties(&props1)
        .expect("Failed to store first ItemProperties");
    
    // Store second ItemProperties (should overwrite)
    let props2 = parse_fts_item_properties("Bass Guitar Amp", None);
    track.set_item_properties(&props2)
        .expect("Failed to store second ItemProperties");
    
    // Verify we get the second one back
    let retrieved = track.get_item_properties()
        .expect("Failed to retrieve ItemProperties");
    
    assert_eq!(retrieved.multi_mic, props2.multi_mic);
    assert_eq!(retrieved.multi_mic, Some(vec!["Amp".to_string()]));
    assert_ne!(retrieved.multi_mic, props1.multi_mic);
}

#[test]
fn test_get_item_properties_returns_none_when_not_set() {
    let track = {
        let mut builder = Track::builder();
        builder
            .name("Test Track".to_string())
            .volume(1.0)
            .pan(0.0)
            .muted(false)
            .solo_state(daw::tracks::api::solo::SoloMode::Off)
            .automation_mode(daw::tracks::api::automation::AutomationMode::TrimRead)
            .invert_phase(false)
            .show_in_mixer(true)
            .show_in_track_list(true);
        builder.build()
    };
    
    // Should return None when ext_state is not set
    assert_eq!(track.get_item_properties(), None);
}

#[test]
fn test_parse_and_store_convenience_method() {
    let mut track = {
        let mut builder = Track::builder();
        builder
            .name("GTR E Clean".to_string())
            .volume(1.0)
            .pan(0.0)
            .muted(false)
            .solo_state(daw::tracks::api::solo::SoloMode::Off)
            .automation_mode(daw::tracks::api::automation::AutomationMode::TrimRead)
            .invert_phase(false)
            .show_in_mixer(true)
            .show_in_track_list(true);
        builder.build()
    };
    
    // Use the convenience method to parse and store
    track.parse_and_store_item_properties()
        .expect("Failed to parse and store ItemProperties");
    
    // Verify it was stored
    let retrieved = track.get_item_properties()
        .expect("Failed to retrieve ItemProperties");
    
    // Verify it parsed correctly
    assert_eq!(retrieved.group_prefix, Some("GTR".to_string()));
    // Note: arrangement might not be extracted if "E" is matched first
    // The parser should still work, so we just verify group_prefix was set
    assert_eq!(retrieved.original_name, Some("GTR E Clean".to_string()));
}

#[test]
fn test_json_serialization_roundtrip() {
    let props = parse_fts_item_properties("D Kick In .02", None);
    
    // Serialize to JSON
    let json = serde_json::to_string(&props)
        .expect("Failed to serialize ItemProperties to JSON");
    
    // Deserialize back
    let deserialized: ItemProperties = serde_json::from_str(&json)
        .expect("Failed to deserialize ItemProperties from JSON");
    
    // Verify roundtrip
    assert_eq!(props.original_name, deserialized.original_name);
    assert_eq!(props.group_prefix, deserialized.group_prefix);
    assert_eq!(props.sub_type, deserialized.sub_type);
    assert_eq!(props.multi_mic, deserialized.multi_mic);
    assert_eq!(props.playlist, deserialized.playlist);
}
