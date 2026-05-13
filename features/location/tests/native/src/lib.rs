//! Native tests for the `location` feature.

#![cfg(test)]

use architect::Page;
use location_crdt::{CrdtDoc, LocationRepoLoro};
use location_proto::{LocationCreate, LocationRepo, LocationUpdate};
use loro::ExportMode;

fn repo() -> LocationRepoLoro {
    LocationRepoLoro::new(&CrdtDoc::ephemeral())
}

fn fixture() -> LocationCreate {
    LocationCreate {
        name: "Studio A".into(),
        kind: Some("studio".into()),
        address1: Some("123 Main St".into()),
        address2: None,
        city: Some("Brooklyn".into()),
        state: Some("NY".into()),
        postal_code: Some("11201".into()),
        country_code: Some("US".into()),
        contact_name: None,
        contact_email: None,
        parent_id: None,
        notes: None,
        tags: vec!["primary".into()],
    }
}

#[tokio::test]
async fn round_trip() {
    let r = repo();
    let l = r.create(fixture()).await.unwrap();
    let got = r.get(l.id).await.unwrap();
    assert_eq!(got.name, "Studio A");
    assert_eq!(got.city.as_deref(), Some("Brooklyn"));
    assert_eq!(got.tags, vec!["primary".to_string()]);
}

#[tokio::test]
async fn update_clears_optional() {
    let r = repo();
    let l = r.create(fixture()).await.unwrap();
    let updated = r
        .update(
            l.id,
            LocationUpdate {
                address2: Some(Some("Suite 200".into())),
                contact_email: Some(Some("hi@example.com".into())),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert_eq!(updated.address2.as_deref(), Some("Suite 200"));
    assert_eq!(updated.contact_email.as_deref(), Some("hi@example.com"));
}

#[tokio::test]
async fn two_replicas_converge() {
    let a = repo();
    let b = repo();
    a.create(fixture()).await.unwrap();
    b.create(LocationCreate {
        name: "Studio B".into(),
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
}
