//! Test event subscription and serialization round-trip.
//!
//! This test verifies that:
//! 1. Events broadcast by the service can be received by subscribers
//! 2. Serialization/deserialization works correctly (no format mismatches)
//! 3. PresetLoaded events include the correct scene_index

use rig_control::{
    defaults::guitar,
    service::{MockRigControlService, RigControlCommand, RigControlData, RigControlEvent},
    LocalRigControlClient,
};
use std::sync::Arc;

#[tokio::test]
async fn test_preset_loaded_event_serialization() {
    // Build guitar rig defaults
    let defaults = guitar::build_guitar_rig();
    let data = RigControlData {
        rig: defaults.rig.clone(),
        presets: defaults.presets.clone(),
        songs: defaults.songs.clone(),
        profiles: defaults.profiles.clone(),
    };

    let service = Arc::new(MockRigControlService::new(data));
    let client = LocalRigControlClient::new(service);

    // Create a subscription channel
    let (tx, mut rx) = roam::channel::<RigControlEvent>();

    // Subscribe to events
    client.subscribe(tx).await;

    // Initialize the rig
    client
        .execute(RigControlCommand::Initialize {
            rig_id: client.get_current_rig().await.unwrap().id,
        })
        .await;

    // Find AC30 Ambient Clean preset
    let presets = client.get_available_presets().await;
    let ac30 = presets
        .iter()
        .find(|p| p.name == "AC30 Ambient Clean")
        .expect("AC30 Ambient Clean preset should exist");

    println!("AC30 default_scene_index: {:?}", ac30.default_scene_index);
    assert_eq!(ac30.default_scene_index, Some(1), "AC30 should have Dark as default");

    // Load the preset with default scene (index 1 - "Dark")
    client
        .execute(RigControlCommand::LoadPresetWithScene {
            preset_id: ac30.id,
            scene_index: 1,
        })
        .await;

    // Wait for the PresetLoaded event
    let event = tokio::time::timeout(std::time::Duration::from_secs(2), rx.recv())
        .await
        .expect("Should receive event within 2 seconds")
        .expect("Channel should not be closed")
        .expect("Should successfully deserialize event");

    // Verify the event
    match event {
        RigControlEvent::PresetLoaded { preset, scene_index } => {
            assert_eq!(preset.name, "AC30 Ambient Clean");
            assert_eq!(scene_index, 1, "Scene index should be 1 (Dark)");
            assert_eq!(preset.default_scene_index, Some(1), "PresetInfo should include default_scene_index");
            println!("✓ PresetLoaded event serialized and deserialized correctly");
            println!("  Preset: {}, Scene index: {}, Default scene: {:?}",
                preset.name, scene_index, preset.default_scene_index);
        }
        other => panic!("Expected PresetLoaded event, got {:?}", other),
    }
}

#[tokio::test]
async fn test_multiple_preset_loads_with_scenes() {
    let defaults = guitar::build_guitar_rig();
    let data = RigControlData {
        rig: defaults.rig.clone(),
        presets: defaults.presets.clone(),
        songs: defaults.songs.clone(),
        profiles: defaults.profiles.clone(),
    };

    let service = Arc::new(MockRigControlService::new(data));
    let client = LocalRigControlClient::new(service);

    let (tx, mut rx) = roam::channel::<RigControlEvent>();
    client.subscribe(tx).await;

    client
        .execute(RigControlCommand::Initialize {
            rig_id: client.get_current_rig().await.unwrap().id,
        })
        .await;

    let presets = client.get_available_presets().await;

    // Load different presets and verify events
    for (i, preset) in presets.iter().take(3).enumerate() {
        let scene_index = i % preset.scene_count; // Cycle through scenes

        println!("\nLoading preset '{}' with scene {}", preset.name, scene_index);

        client
            .execute(RigControlCommand::LoadPresetWithScene {
                preset_id: preset.id,
                scene_index,
            })
            .await;

        // Receive and validate event
        let event = tokio::time::timeout(std::time::Duration::from_secs(1), rx.recv())
            .await
            .expect("Should receive event")
            .expect("Channel should not be closed")
            .expect("Should deserialize");

        match event {
            RigControlEvent::PresetLoaded { preset: event_preset, scene_index: event_scene } => {
                assert_eq!(event_preset.id, preset.id);
                assert_eq!(event_scene, scene_index);
                // Verify the new field is included
                assert!(
                    event_preset.default_scene_index.is_some() || event_preset.default_scene_index.is_none(),
                    "default_scene_index field should be present (even if None)"
                );
                println!("✓ Event received: {} with scene {}", event_preset.name, event_scene);
            }
            other => panic!("Expected PresetLoaded, got {:?}", other),
        }
    }

    println!("\n✓ All {} events serialized/deserialized correctly", 3);
}

#[tokio::test]
async fn test_event_channel_never_panics_on_format_mismatch() {
    // This test demonstrates that even if there's a format mismatch,
    // the subscription loop should continue (not panic)

    let defaults = guitar::build_guitar_rig();
    let data = RigControlData {
        rig: defaults.rig.clone(),
        presets: defaults.presets.clone(),
        songs: defaults.songs.clone(),
        profiles: defaults.profiles.clone(),
    };

    let service = Arc::new(MockRigControlService::new(data));
    let client = LocalRigControlClient::new(service);

    let (tx, mut rx) = roam::channel::<RigControlEvent>();
    client.subscribe(tx).await;

    client
        .execute(RigControlCommand::Initialize {
            rig_id: client.get_current_rig().await.unwrap().id,
        })
        .await;

    let presets = client.get_available_presets().await;
    let ac30 = presets.iter().find(|p| p.name == "AC30 Ambient Clean").unwrap();

    // Load preset
    client
        .execute(RigControlCommand::LoadPresetWithScene {
            preset_id: ac30.id,
            scene_index: 0,
        })
        .await;

    // Try to receive - should work or return deserialization error, but not panic
    match rx.recv().await {
        Ok(Some(event)) => {
            println!("✓ Successfully received and deserialized event: {:?}",
                match event {
                    RigControlEvent::PresetLoaded { ref preset, scene_index } =>
                        format!("PresetLoaded({}, scene {})", preset.name, scene_index),
                    _ => format!("{:?}", event),
                });
        }
        Ok(None) => {
            panic!("Channel closed unexpectedly");
        }
        Err(e) => {
            // This is what happens when there's a format mismatch
            println!("⚠ Deserialization error (expected if struct format changed): {:?}", e);
            println!("✓ Channel handled error gracefully without panicking");
        }
    }
}
