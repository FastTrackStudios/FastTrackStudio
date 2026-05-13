//! Native tests for the `asset` feature. Loro-backed, ephemeral.

#![cfg(test)]

use architect::Page;
use asset_crdt::{AssetRepoLoro, CrdtDoc};
use asset_proto::{AssetCreate, AssetRepo, AssetUpdate};
use chrono::Utc;
use loro::ExportMode;

fn repo() -> AssetRepoLoro {
    AssetRepoLoro::new(&CrdtDoc::ephemeral())
}

fn fixture() -> AssetCreate {
    AssetCreate {
        name: "SSL Bus Compressor".into(),
        status: "active".into(),
        manufacturer: Some("Solid State Logic".into()),
        model: Some("G-Series".into()),
        serial_number: Some("SSL-12345".into()),
        owner_id: None,
        location_id: None,
        notes: Some("Studio A rack 3".into()),
        tags: vec!["outboard".into(), "compressor".into()],
        acquired_at: Some(Utc::now()),
    }
}

#[tokio::test]
async fn round_trip_all_fields() {
    let r = repo();
    let a = r.create(fixture()).await.unwrap();
    let got = r.get(a.id).await.unwrap();
    assert_eq!(got.name, "SSL Bus Compressor");
    assert_eq!(got.status, "active");
    assert_eq!(got.manufacturer.as_deref(), Some("Solid State Logic"));
    assert_eq!(got.tags, vec!["outboard".to_string(), "compressor".into()]);
}

#[tokio::test]
async fn update_status_and_clear_notes() {
    let r = repo();
    let a = r.create(fixture()).await.unwrap();
    let updated = r
        .update(
            a.id,
            AssetUpdate {
                status: Some("retired".into()),
                notes: Some(None),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert_eq!(updated.status, "retired");
    assert_eq!(updated.notes, None);
}

#[tokio::test]
async fn two_replicas_converge() {
    let a = repo();
    let b = repo();
    a.create(fixture()).await.unwrap();
    b.create(AssetCreate {
        name: "Neumann U87".into(),
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
