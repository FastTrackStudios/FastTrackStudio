//! Integration tests for `AudioProductionService`.

use task_core::service::{AddTrackRequest, AudioProductionService, SubmitMixRequest};
use task_core::service_impl::{AudioProductionServiceDeps, AudioProductionServiceImpl};
use task_core::track::TrackStatus;
use task_db::seed::{DEMO_NAMESPACE, seed_demo_data};
use uuid::Uuid;

fn demo_id(key: &str) -> Uuid {
    Uuid::new_v5(&DEMO_NAMESPACE, key.as_bytes())
}

#[tokio::test]
async fn list_tracks_returns_montreal_album_in_sequence_order() {
    let db = task_db::init_memory().await.expect("init db");
    let summary = seed_demo_data(&db).await.expect("seed");
    assert!(
        summary.tracks_created >= 12,
        "want >= 12 tracks, got {}",
        summary.tracks_created
    );

    let svc = AudioProductionServiceImpl::new(AudioProductionServiceDeps { db: db.clone() });
    let project_id = demo_id("project:fasttrack-album");
    let tracks = svc.list_tracks(project_id).await.expect("list");
    assert!(
        tracks.len() >= 8,
        "want >=8 montreal tracks, got {}",
        tracks.len()
    );
    for window in tracks.windows(2) {
        assert!(
            window[0].sequence <= window[1].sequence,
            "tracks not sorted by sequence: {:?}",
            tracks.iter().map(|t| t.sequence).collect::<Vec<_>>()
        );
    }
}

#[tokio::test]
async fn add_track_appends_with_next_sequence() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = AudioProductionServiceImpl::new(AudioProductionServiceDeps { db: db.clone() });
    let project_id = demo_id("project:fasttrack-album");

    let before = svc.list_tracks(project_id).await.expect("list before");
    let max_before = before.iter().map(|t| t.sequence).max().unwrap_or(0);

    let added = svc
        .add_track(AddTrackRequest {
            project_id,
            title: "New Bonus Track".to_string(),
            sequence: None,
            bpm: Some(120.0),
            key: Some("Em".into()),
            artist: Some("Just Friends".into()),
            created_by: Some("cody".into()),
        })
        .await
        .expect("add");
    assert_eq!(added.sequence, max_before + 1, "should append after max");

    let again = svc
        .add_track(AddTrackRequest {
            project_id,
            title: "Another Bonus".to_string(),
            sequence: None,
            bpm: None,
            key: None,
            artist: None,
            created_by: None,
        })
        .await
        .expect("add");
    assert_eq!(again.sequence, max_before + 2);
}

#[tokio::test]
async fn transition_status_validates_and_applies() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = AudioProductionServiceImpl::new(AudioProductionServiceDeps { db: db.clone() });
    let id = demo_id("track:montreal-rue-de-bleury");

    let bad = svc.transition_status(id, "BadStatus".into()).await;
    assert!(bad.is_err(), "unknown status should fail");

    let ok = svc
        .transition_status(id, "Mixing".into())
        .await
        .expect("transition");
    assert_eq!(ok.status, TrackStatus::Mixing);
}

#[tokio::test]
async fn submit_mix_advances_status_and_increments_revision() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = AudioProductionServiceImpl::new(AudioProductionServiceDeps { db: db.clone() });
    // Tracking → Mixing on submit.
    let id = demo_id("track:tom-half-light");
    let before = svc.get_track(id).await.expect("get").expect("present");
    assert_eq!(before.status, TrackStatus::Tracking);

    let after = svc
        .submit_mix(SubmitMixRequest {
            track_id: id,
            notes: Some("first mix pass".into()),
            mix_path: Some("Projects/Tom Solo EP/mixes/half-light-v1.wav".into()),
            mix_label: None,
            mix_mime: Some("audio/wav".into()),
            uploader: Some("cody".into()),
        })
        .await
        .expect("submit");
    assert_eq!(after.status, TrackStatus::Mixing);
    assert_eq!(after.revision_number, before.revision_number + 1);
}

#[tokio::test]
async fn approve_mix_sets_status_and_stamps_properties() {
    let db = task_db::init_memory().await.expect("init db");
    seed_demo_data(&db).await.expect("seed");
    let svc = AudioProductionServiceImpl::new(AudioProductionServiceDeps { db: db.clone() });
    let id = demo_id("track:montreal-headlights");

    let approved = svc.approve_mix(id, "amy".into()).await.expect("approve");
    assert_eq!(approved.status, TrackStatus::Approved);

    let props_value: serde_json::Value =
        serde_json::to_value(&approved.properties).expect("serialize props");
    let map = props_value.as_object().expect("object");
    assert_eq!(map.get("approved_by").and_then(|v| v.as_str()), Some("amy"));
    assert!(
        map.get("approved_at").and_then(|v| v.as_str()).is_some(),
        "approved_at should be set"
    );
}
