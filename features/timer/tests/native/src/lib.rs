//! Native integration tests for the `timer` feature. Drives the
//! wire contract against the Loro-backed implementation through an
//! ephemeral `CrdtDoc`.

#![cfg(test)]

use architect::Page;
use chrono::{Duration, Utc};
use loro::ExportMode;
use timer_crdt::{CrdtDoc, TimeEntryRepoLoro};
use timer_proto::{TimeEntryCreate, TimeEntryRepo, TimeEntryUpdate};
use uuid::Uuid;

fn repo() -> TimeEntryRepoLoro {
    TimeEntryRepoLoro::new(&CrdtDoc::ephemeral())
}

fn baseline(start_offset_min: i64) -> TimeEntryCreate {
    TimeEntryCreate {
        task_id: Some(Uuid::new_v4()),
        project_id: None,
        client_id: None,
        user: Some("alice".into()),
        start_time: Utc::now() + Duration::minutes(start_offset_min),
        end_time: None,
        description: Some("design review".into()),
        billable: true,
        manual: false,
        billable_rate_cents: Some(15000),
        tags: vec!["design".into(), "review".into()],
        invoiced_at: None,
    }
}

// Loro stores DateTimes as RFC3339 strings; sub-second precision can
// differ by nanos vs. micros. Normalize both sides for equality.
fn round_trip_rfc3339(dt: chrono::DateTime<Utc>) -> chrono::DateTime<Utc> {
    chrono::DateTime::parse_from_rfc3339(&dt.to_rfc3339())
        .unwrap()
        .with_timezone(&Utc)
}

#[tokio::test]
async fn create_then_get_round_trips_all_fields() {
    let r = repo();
    let created = r.create(baseline(0)).await.unwrap();
    let got = r.get(created.id).await.unwrap();

    assert_eq!(got.id, created.id);
    assert_eq!(got.task_id, created.task_id);
    assert_eq!(got.user.as_deref(), Some("alice"));
    assert_eq!(got.description.as_deref(), Some("design review"));
    assert!(got.billable);
    assert_eq!(got.billable_rate_cents, Some(15000));
    assert_eq!(got.tags, vec!["design".to_string(), "review".to_string()]);
    assert_eq!(got.end_time, None);
    assert_eq!(got.invoiced_at, None);
}

#[tokio::test]
async fn update_clears_optional_fields() {
    let r = repo();
    let created = r.create(baseline(0)).await.unwrap();
    let now = Utc::now();

    let updated = r
        .update(
            created.id,
            TimeEntryUpdate {
                end_time: Some(Some(now)),
                description: Some(None), // clear it
                billable: Some(false),
                ..Default::default()
            },
        )
        .await
        .unwrap();

    assert_eq!(updated.end_time, Some(round_trip_rfc3339(now)));
    assert_eq!(updated.description, None);
    assert!(!updated.billable);
    // Untouched fields survive.
    assert_eq!(updated.user.as_deref(), Some("alice"));
    assert_eq!(
        updated.tags,
        vec!["design".to_string(), "review".to_string()]
    );
}

#[tokio::test]
async fn list_sorted_by_start_time() {
    let r = repo();
    for offset in [-30i64, 0, -15] {
        r.create(baseline(offset)).await.unwrap();
    }
    let page = r
        .list(
            Page {
                index: 0,
                size: 100,
            },
            Some(architect::Sort {
                field: "start_time".into(),
                order: architect::SortOrder::Asc,
            }),
            None,
        )
        .await
        .unwrap();
    let offsets: Vec<_> = page.items.iter().map(|e| e.start_time).collect();
    let mut sorted = offsets.clone();
    sorted.sort();
    assert_eq!(offsets, sorted);
}

#[tokio::test]
async fn two_replicas_converge_after_sync() {
    let a = repo();
    let b = repo();
    a.create(baseline(0)).await.unwrap();
    b.create(baseline(-10)).await.unwrap();
    let ab = a.doc().export(ExportMode::all_updates()).unwrap();
    let bb = b.doc().export(ExportMode::all_updates()).unwrap();
    b.doc().import(&ab).unwrap();
    a.doc().import(&bb).unwrap();
    let a_list = a
        .list(
            Page {
                index: 0,
                size: 100,
            },
            None,
            None,
        )
        .await
        .unwrap();
    let b_list = b
        .list(
            Page {
                index: 0,
                size: 100,
            },
            None,
            None,
        )
        .await
        .unwrap();
    assert_eq!(a_list.total, 2);
    assert_eq!(b_list.total, 2);
}

#[tokio::test]
async fn project_client_manual_round_trip() {
    let r = repo();
    let project_id = Uuid::new_v4();
    let client_id = Uuid::new_v4();

    let created = r
        .create(TimeEntryCreate {
            task_id: None,
            project_id: Some(project_id),
            client_id: Some(client_id),
            user: Some("bob".into()),
            start_time: Utc::now(),
            end_time: Some(Utc::now() + Duration::minutes(45)),
            description: Some("manually-logged session".into()),
            billable: true,
            manual: true,
            billable_rate_cents: Some(20_000),
            tags: vec!["logged".into()],
            invoiced_at: None,
        })
        .await
        .unwrap();

    let got = r.get(created.id).await.unwrap();
    assert_eq!(got.project_id, Some(project_id));
    assert_eq!(got.client_id, Some(client_id));
    assert!(got.manual);

    // Flip `manual` via Update.
    let updated = r
        .update(
            created.id,
            TimeEntryUpdate {
                manual: Some(false),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert!(!updated.manual);
    // Project / client survive an unrelated update.
    assert_eq!(updated.project_id, Some(project_id));
    assert_eq!(updated.client_id, Some(client_id));
}
