//! Test if simple events serialize correctly

use rig_control::{
    defaults::guitar,
    service::{MockRigControlService, RigControlCommand, RigControlData, RigControlEvent},
    LocalRigControlClient,
};
use std::sync::Arc;

#[tokio::test]
async fn test_simple_event_serialization() {
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

    // Load song scene - should broadcast SongChanged and SceneIndexChanged events
    client
        .execute(RigControlCommand::LoadSongScene {
            song_index: 1,
            scene_index: 0,
        })
        .await;

    // Try to receive the events
    println!("Waiting for events...");
    for i in 0..3 {
        match rx.recv().await {
            Ok(Some(event)) => {
                println!("Event {}: {:?}", i, match &event {
                    RigControlEvent::SongChanged { song_index } => format!("SongChanged({})", song_index),
                    RigControlEvent::SceneIndexChanged { scene_index } => format!("SceneIndexChanged({})", scene_index),
                    RigControlEvent::PresetLoaded { preset, scene_index } => format!("PresetLoaded({}, scene {})", preset.name, scene_index),
                    _ => format!("{:?}", event),
                });
            }
            Ok(None) => {
                println!("Channel closed at event {}", i);
                break;
            }
            Err(e) => {
                println!("Deserialization error at event {}: {:?}", i, e);
                // If we get an error, stop trying
                panic!("Failed to deserialize event {}: {:?}", i, e);
            }
        }
    }

    println!("✓ All simple events serialized correctly");
}
