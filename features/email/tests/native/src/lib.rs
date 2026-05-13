#![cfg(test)]

use architect::Page;
use chrono::Utc;
use email_crdt::{CrdtDoc, EmailRepoLoro};
use email_proto::{EmailCreate, EmailRepo, EmailUpdate};
use loro::ExportMode;

fn repo() -> EmailRepoLoro {
    EmailRepoLoro::new(&CrdtDoc::ephemeral())
}

fn fixture() -> EmailCreate {
    EmailCreate {
        message_id: "<abc123@example.com>".into(),
        subject: "Hello world".into(),
        from_addr: "alice@example.com".into(),
        to_addrs: vec!["bob@example.com".into()],
        cc_addrs: vec!["carol@example.com".into()],
        bcc_addrs: vec![],
        body: Some("greetings".into()),
        received_at: Utc::now(),
        read: false,
        starred: false,
        folder: Some("inbox".into()),
        thread_id: Some("thread-1".into()),
        tags: vec!["work".into(), "important".into()],
    }
}

#[tokio::test]
async fn round_trip_all_fields() {
    let r = repo();
    let e = r.create(fixture()).await.unwrap();
    let got = r.get(e.id).await.unwrap();
    assert_eq!(got.message_id, "<abc123@example.com>");
    assert_eq!(got.subject, "Hello world");
    assert_eq!(got.from_addr, "alice@example.com");
    assert_eq!(got.to_addrs, vec!["bob@example.com".to_string()]);
    assert_eq!(got.cc_addrs, vec!["carol@example.com".to_string()]);
    assert!(got.bcc_addrs.is_empty());
    assert_eq!(got.body.as_deref(), Some("greetings"));
    assert!(!got.read);
    assert!(!got.starred);
    assert_eq!(got.folder.as_deref(), Some("inbox"));
    assert_eq!(got.thread_id.as_deref(), Some("thread-1"));
    assert_eq!(got.tags, vec!["work".to_string(), "important".into()]);
}

#[tokio::test]
async fn update_marks_read_and_starred() {
    let r = repo();
    let e = r.create(fixture()).await.unwrap();
    let updated = r
        .update(
            e.id,
            EmailUpdate {
                read: Some(true),
                starred: Some(true),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert!(updated.read);
    assert!(updated.starred);
}

#[tokio::test]
async fn two_replicas_converge() {
    let a = repo();
    let b = repo();
    a.create(fixture()).await.unwrap();
    b.create(EmailCreate {
        message_id: "<def456@example.com>".into(),
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
