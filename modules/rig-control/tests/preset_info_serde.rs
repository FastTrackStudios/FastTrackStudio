//! Test PresetInfo serialization/deserialization directly

use rig_control::{
    defaults::guitar,
    service::{MockRigControlService, RigControlData, PresetInfo},
    LocalRigControlClient,
};
use std::sync::Arc;

#[tokio::test]
async fn test_preset_info_roundtrip() {
    let defaults = guitar::build_guitar_rig();
    let data = RigControlData {
        rig: defaults.rig.clone(),
        presets: defaults.presets.clone(),
        songs: defaults.songs.clone(),
        profiles: defaults.profiles.clone(),
    };

    let service = Arc::new(MockRigControlService::new(data));
    let client = LocalRigControlClient::new(service);

    // Get a preset
    let presets = client.get_available_presets().await;
    let ac30 = presets
        .iter()
        .find(|p| p.name == "AC30 Ambient Clean")
        .unwrap()
        .clone();

    println!("Original PresetInfo:");
    println!("  name: {}", ac30.name);
    println!("  default_scene_index: {:?}", ac30.default_scene_index);
    println!("  scene_count: {}", ac30.scene_count);

    // Serialize with postcard
    let bytes = postcard::to_stdvec(&ac30).expect("Should serialize");
    println!("\nSerialized to {} bytes", bytes.len());
    println!("First 20 bytes: {:?}", &bytes[..bytes.len().min(20)]);

    // Deserialize
    let deserialized: PresetInfo = postcard::from_bytes(&bytes).expect("Should deserialize");

    println!("\nDeserialized PresetInfo:");
    println!("  name: {}", deserialized.name);
    println!("  default_scene_index: {:?}", deserialized.default_scene_index);
    println!("  scene_count: {}", deserialized.scene_count);

    // Verify they match
    assert_eq!(ac30, deserialized);
    println!("\n✓ Round-trip successful");
}
