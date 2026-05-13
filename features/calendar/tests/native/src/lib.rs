//! Native tests for the `calendar` feature. Loro-backed, ephemeral.

#![cfg(test)]

use architect::Page;
use calendar_crdt::{CalendarEventRepoLoro, CrdtDoc};
use calendar_proto::{CalendarEventCreate, CalendarEventRepo, CalendarEventUpdate};
use chrono::{Duration, Utc};
use loro::ExportMode;

fn repo() -> CalendarEventRepoLoro {
    CalendarEventRepoLoro::new(&CrdtDoc::ephemeral())
}

fn fixture() -> CalendarEventCreate {
    let start = Utc::now();
    CalendarEventCreate {
        title: "Studio session".into(),
        description: Some("Mix down the new single".into()),
        start_at: start,
        end_at: start + Duration::hours(2),
        all_day: false,
        location_id: None,
        location_text: Some("Studio A".into()),
        rrule: Some("FREQ=WEEKLY;BYDAY=MO".into()),
        organizer: Some("cody".into()),
        attendees: vec!["alice".into(), "bob".into()],
        calendar_id: Some("work".into()),
        status: "confirmed".into(),
        tags: vec!["music".into(), "studio".into()],
    }
}

#[tokio::test]
async fn round_trip_all_fields() {
    let r = repo();
    let e = r.create(fixture()).await.unwrap();
    let got = r.get(e.id).await.unwrap();
    assert_eq!(got.title, "Studio session");
    assert_eq!(got.description.as_deref(), Some("Mix down the new single"));
    assert_eq!(got.all_day, false);
    assert_eq!(got.location_text.as_deref(), Some("Studio A"));
    assert_eq!(got.rrule.as_deref(), Some("FREQ=WEEKLY;BYDAY=MO"));
    assert_eq!(got.organizer.as_deref(), Some("cody"));
    assert_eq!(got.attendees, vec!["alice".to_string(), "bob".into()]);
    assert_eq!(got.calendar_id.as_deref(), Some("work"));
    assert_eq!(got.status, "confirmed");
    assert_eq!(got.tags, vec!["music".to_string(), "studio".into()]);
}

#[tokio::test]
async fn update_cancel_event() {
    let r = repo();
    let e = r.create(fixture()).await.unwrap();
    let updated = r
        .update(
            e.id,
            CalendarEventUpdate {
                status: Some("cancelled".into()),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert_eq!(updated.status, "cancelled");
}

#[tokio::test]
async fn two_replicas_converge() {
    let a = repo();
    let b = repo();
    a.create(fixture()).await.unwrap();
    b.create(CalendarEventCreate {
        title: "Rehearsal".into(),
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
