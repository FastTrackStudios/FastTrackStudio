#![cfg(test)]

use architect::Page;
use chrono::Utc;
use conference_crdt::{CrdtDoc, MeetingRepoLoro};
use conference_proto::{MeetingCreate, MeetingRepo, MeetingUpdate};
use loro::ExportMode;

fn repo() -> MeetingRepoLoro {
    MeetingRepoLoro::new(&CrdtDoc::ephemeral())
}

fn fixture() -> MeetingCreate {
    MeetingCreate {
        name: "Weekly sync".into(),
        host_user: Some("alice".into()),
        scheduled_at: Utc::now(),
        started_at: None,
        ended_at: None,
        status: "scheduled".into(),
        recording_url: None,
        notes: Some("agenda: ship it".into()),
        participants: vec!["alice".into(), "bob".into()],
        tags: vec!["weekly".into(), "engineering".into()],
    }
}

#[tokio::test]
async fn round_trip_all_fields() {
    let r = repo();
    let m = r.create(fixture()).await.unwrap();
    let got = r.get(m.id).await.unwrap();
    assert_eq!(got.name, "Weekly sync");
    assert_eq!(got.host_user.as_deref(), Some("alice"));
    assert_eq!(got.status, "scheduled");
    assert_eq!(got.notes.as_deref(), Some("agenda: ship it"));
    assert_eq!(got.participants, vec!["alice".to_string(), "bob".into()]);
    assert_eq!(got.tags, vec!["weekly".to_string(), "engineering".into()]);
}

#[tokio::test]
async fn update_status_to_ended() {
    let r = repo();
    let m = r.create(fixture()).await.unwrap();
    let ended_at = Utc::now();
    let updated = r
        .update(
            m.id,
            MeetingUpdate {
                status: Some("ended".into()),
                ended_at: Some(Some(ended_at)),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert_eq!(updated.status, "ended");
    assert_eq!(updated.ended_at, Some(ended_at));
}

#[tokio::test]
async fn two_replicas_converge() {
    let a = repo();
    let b = repo();
    a.create(fixture()).await.unwrap();
    b.create(MeetingCreate {
        name: "Standup".into(),
        ..fixture()
    })
    .await
    .unwrap();
    let ab = a.doc().export(ExportMode::all_updates()).unwrap();
    let bb = b.doc().export(ExportMode::all_updates()).unwrap();
    b.doc().import(&ab).unwrap();
    a.doc().import(&bb).unwrap();
    assert_eq!(
        a.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
    assert_eq!(
        b.list(
            Page {
                index: 0,
                size: 100
            },
            None,
            None
        )
        .await
        .unwrap()
        .total,
        2
    );
}
