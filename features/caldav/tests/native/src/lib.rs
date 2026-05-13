//! Native integration tests for the `caldav` feature. One ephemeral
//! `CrdtDoc` hosts both entity types — proves they coexist under
//! distinct roots and survive cross-entity sync round-trips.

#![cfg(test)]

use architect::Page;
use caldav_crdt::{CalDavAccountRepoLoro, CalDavCalendarRepoLoro, CrdtDoc};
use caldav_proto::{
    CalDavAccountCreate, CalDavAccountRepo, CalDavCalendarCreate, CalDavCalendarRepo,
};
use chrono::Utc;
use loro::ExportMode;

#[tokio::test]
async fn account_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = CalDavAccountRepoLoro::new(&doc);

    let now = Utc::now();
    let created = repo
        .create(CalDavAccountCreate {
            display_name: "Personal Nextcloud".into(),
            base_url: "https://cloud.example.com/remote.php/dav/principals/users/alice/".into(),
            username: "alice".into(),
            kind: Some("nextcloud".into()),
            last_sync_at: Some(now),
            last_error: None,
            tags: vec!["primary".into(), "home".into()],
        })
        .await
        .unwrap();

    let got = repo.get(created.id).await.unwrap();
    assert_eq!(got.display_name, "Personal Nextcloud");
    assert_eq!(got.username, "alice");
    assert_eq!(got.kind.as_deref(), Some("nextcloud"));
    assert!(got.last_sync_at.is_some());
    assert_eq!(got.tags, vec!["primary".to_string(), "home".to_string()]);
}

#[tokio::test]
async fn calendar_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let accounts = CalDavAccountRepoLoro::new(&doc);
    let calendars = CalDavCalendarRepoLoro::new(&doc);

    let account = accounts
        .create(CalDavAccountCreate {
            display_name: "Work".into(),
            base_url: "https://cloud.example.com/remote.php/dav/principals/users/bob/".into(),
            username: "bob".into(),
            kind: Some("nextcloud".into()),
            last_sync_at: None,
            last_error: None,
            tags: vec![],
        })
        .await
        .unwrap();

    let cal = calendars
        .create(CalDavCalendarCreate {
            account_id: account.id,
            name: "Work Calendar".into(),
            url: "https://cloud.example.com/remote.php/dav/calendars/bob/work/".into(),
            color: Some("#3366ff".into()),
            enabled: true,
            server_ctag: Some("abc123".into()),
            last_sync_at: Some(Utc::now()),
            event_count: 42,
        })
        .await
        .unwrap();

    let got = calendars.get(cal.id).await.unwrap();
    assert_eq!(got.name, "Work Calendar");
    assert_eq!(got.account_id, account.id);
    assert_eq!(got.enabled, true);
    assert_eq!(got.color.as_deref(), Some("#3366ff"));
    assert_eq!(got.server_ctag.as_deref(), Some("abc123"));
    assert_eq!(got.event_count, 42);
}

#[tokio::test]
async fn both_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let accounts = CalDavAccountRepoLoro::new(&doc);
    let calendars = CalDavCalendarRepoLoro::new(&doc);

    let acct = accounts
        .create(CalDavAccountCreate {
            display_name: "A".into(),
            base_url: "https://x/".into(),
            username: "u".into(),
            kind: None,
            last_sync_at: None,
            last_error: None,
            tags: vec![],
        })
        .await
        .unwrap();

    calendars
        .create(CalDavCalendarCreate {
            account_id: acct.id,
            name: "Cal".into(),
            url: "https://x/cal/".into(),
            color: None,
            enabled: true,
            server_ctag: None,
            last_sync_at: None,
            event_count: 0,
        })
        .await
        .unwrap();

    assert_eq!(
        accounts
            .list(
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
        1
    );
    assert_eq!(
        calendars
            .list(
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
        1
    );
}

#[tokio::test]
async fn replicas_converge() {
    let a = CrdtDoc::ephemeral();
    let b = CrdtDoc::ephemeral();
    let aa = CalDavAccountRepoLoro::new(&a);
    let ab = CalDavAccountRepoLoro::new(&b);

    aa.create(CalDavAccountCreate {
        display_name: "from-a".into(),
        base_url: "https://a/".into(),
        username: "alice".into(),
        kind: None,
        last_sync_at: None,
        last_error: None,
        tags: vec![],
    })
    .await
    .unwrap();
    ab.create(CalDavAccountCreate {
        display_name: "from-b".into(),
        base_url: "https://b/".into(),
        username: "bob".into(),
        kind: None,
        last_sync_at: None,
        last_error: None,
        tags: vec![],
    })
    .await
    .unwrap();

    let a_bytes = aa.doc().export(ExportMode::all_updates()).unwrap();
    let b_bytes = ab.doc().export(ExportMode::all_updates()).unwrap();
    ab.doc().import(&a_bytes).unwrap();
    aa.doc().import(&b_bytes).unwrap();

    assert_eq!(
        aa.list(
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
        ab.list(
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
