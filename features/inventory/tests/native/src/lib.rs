//! Native integration tests for the `inventory` feature. Ephemeral
//! `CrdtDoc` hosts both entities; tests cover round-trip, checkout
//! lifecycle, and status propagation.

#![cfg(test)]

use architect::Page;
use chrono::{Duration, Utc};
use inventory_crdt::{CheckoutEventRepoLoro, CrdtDoc, InventoryItemRepoLoro};
use inventory_proto::{
    CheckoutEventCreate, CheckoutEventRepo, InventoryItemCreate, InventoryItemRepo,
    InventoryItemUpdate,
};
use uuid::Uuid;

fn page() -> Page {
    Page {
        index: 0,
        size: 100,
    }
}

fn sample_item(name: &str) -> InventoryItemCreate {
    InventoryItemCreate {
        name: name.into(),
        category: Some("audio-interface".into()),
        status: "active".into(),
        manufacturer: Some("Universal Audio".into()),
        model: Some("Apollo x8p".into()),
        serial_number: Some("SN-001".into()),
        qr_code: Some("ASSET-001".into()),
        owner_id: None,
        location_id: None,
        sub_location: Some("Rack 1".into()),
        value_cents: Some(250_000),
        warranty_until: Some(Utc::now() + Duration::days(365)),
        vendor: Some("Sweetwater".into()),
        purchase_order: Some("PO-100".into()),
        acquired_at: Some(Utc::now() - Duration::days(30)),
        photo_url: None,
        notes: Some("studio main".into()),
        tags: vec!["studio-a".into()],
    }
}

#[tokio::test]
async fn item_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = InventoryItemRepoLoro::new(&doc);
    let item = repo.create(sample_item("Apollo")).await.unwrap();
    let got = repo.get(item.id).await.unwrap();
    assert_eq!(got.name, "Apollo");
    assert_eq!(got.status, "active");
    assert_eq!(got.manufacturer.as_deref(), Some("Universal Audio"));
    assert_eq!(got.value_cents, Some(250_000));
    assert_eq!(got.tags, vec!["studio-a".to_string()]);
}

#[tokio::test]
async fn checkout_event_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let items = InventoryItemRepoLoro::new(&doc);
    let events = CheckoutEventRepoLoro::new(&doc);
    let item = items.create(sample_item("U87")).await.unwrap();
    let person = Uuid::new_v4();
    let when = Utc::now();
    let ev = events
        .create(CheckoutEventCreate {
            item_id: item.id,
            person_id: Some(person),
            checked_out_at: when,
            due_at: Some(when + Duration::days(7)),
            returned_at: None,
            note: Some("location shoot".into()),
        })
        .await
        .unwrap();
    let got = events.get(ev.id).await.unwrap();
    assert_eq!(got.item_id, item.id);
    assert_eq!(got.person_id, Some(person));
    assert!(got.returned_at.is_none());
}

#[tokio::test]
async fn checkout_checkin_round_trip_via_updates() {
    // Exercise the status-change path that InventoryServiceImpl walks.
    let doc = CrdtDoc::ephemeral();
    let items = InventoryItemRepoLoro::new(&doc);
    let events = CheckoutEventRepoLoro::new(&doc);
    let item = items.create(sample_item("SM7B")).await.unwrap();
    let person = Uuid::new_v4();

    // Checkout: create event + flip status.
    let ev = events
        .create(CheckoutEventCreate {
            item_id: item.id,
            person_id: Some(person),
            checked_out_at: Utc::now(),
            due_at: None,
            returned_at: None,
            note: None,
        })
        .await
        .unwrap();
    items
        .update(
            item.id,
            InventoryItemUpdate {
                status: Some("checked-out".into()),
                owner_id: Some(Some(person)),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    assert_eq!(items.get(item.id).await.unwrap().status, "checked-out");
    assert_eq!(items.get(item.id).await.unwrap().owner_id, Some(person));

    // Checkin: close event + flip back.
    events
        .update(
            ev.id,
            inventory_proto::CheckoutEventUpdate {
                returned_at: Some(Some(Utc::now())),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    items
        .update(
            item.id,
            InventoryItemUpdate {
                status: Some("active".into()),
                owner_id: Some(None),
                ..Default::default()
            },
        )
        .await
        .unwrap();
    let after = items.get(item.id).await.unwrap();
    assert_eq!(after.status, "active");
    assert_eq!(after.owner_id, None);
    assert!(events.get(ev.id).await.unwrap().returned_at.is_some());
}

#[tokio::test]
async fn list_events_for_item() {
    let doc = CrdtDoc::ephemeral();
    let items = InventoryItemRepoLoro::new(&doc);
    let events = CheckoutEventRepoLoro::new(&doc);
    let a = items.create(sample_item("A")).await.unwrap();
    let b = items.create(sample_item("B")).await.unwrap();

    for _ in 0..3 {
        events
            .create(CheckoutEventCreate {
                item_id: a.id,
                person_id: None,
                checked_out_at: Utc::now(),
                due_at: None,
                returned_at: None,
                note: None,
            })
            .await
            .unwrap();
    }
    events
        .create(CheckoutEventCreate {
            item_id: b.id,
            person_id: None,
            checked_out_at: Utc::now(),
            due_at: None,
            returned_at: None,
            note: None,
        })
        .await
        .unwrap();

    let all = events.list(page(), None, None).await.unwrap();
    assert_eq!(all.total, 4);
    let for_a: Vec<_> = all.items.iter().filter(|e| e.item_id == a.id).collect();
    assert_eq!(for_a.len(), 3);
}

#[tokio::test]
async fn both_entities_coexist_in_one_doc() {
    let doc = CrdtDoc::ephemeral();
    let items = InventoryItemRepoLoro::new(&doc);
    let events = CheckoutEventRepoLoro::new(&doc);
    let item = items.create(sample_item("Coexist")).await.unwrap();
    events
        .create(CheckoutEventCreate {
            item_id: item.id,
            person_id: None,
            checked_out_at: Utc::now(),
            due_at: None,
            returned_at: None,
            note: None,
        })
        .await
        .unwrap();
    assert_eq!(items.list(page(), None, None).await.unwrap().total, 1);
    assert_eq!(events.list(page(), None, None).await.unwrap().total, 1);
}
