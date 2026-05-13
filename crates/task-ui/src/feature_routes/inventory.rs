//! Inventory feature route. Holds a shared `CrdtDoc` across the
//! `InventoryItemRepoLoro` + `CheckoutEventRepoLoro`, syncs over
//! WebSocket, seeds 18 demo items + 4 in-flight checkouts on mount,
//! and renders `InventoryDashboard` (Snipe-IT-style gear catalog).
//!
//! Person + Location maps are currently constructed from a tiny
//! in-route seed (4 people, 3 studios). FUTURE: replace with real
//! `PersonRepoLoro` + `LocationRepoLoro` queries once those land in
//! `task-ui`.

use std::collections::HashMap;
use std::rc::Rc;

use architect::Page;
use chrono::{Duration, Utc};
use dioxus::prelude::*;
use fake::{Fake, Faker};
use futures_channel::mpsc;
use futures_util::StreamExt;
use inventory_crdt::{CheckoutEventRepoLoro, CrdtDoc, InventoryItemRepoLoro};
use inventory_proto::{
    CheckoutEvent, CheckoutEventCreate, CheckoutEventRepo, InventoryItem, InventoryItemCreate,
    InventoryItemRepo, InventoryItemUpdate,
};
use inventory_ui::InventoryDashboard;
use location_proto::Location;
use person_proto::Person;
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

#[component]
pub fn InventoryView() -> Element {
    let items_repo: Rc<InventoryItemRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(InventoryItemRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(items_repo.doc().clone())));
    let events_repo: Rc<CheckoutEventRepoLoro> =
        use_hook(|| Rc::new(CheckoutEventRepoLoro::new(&doc)));

    // ── Person + location seeds (in-route until repos land in task-ui) ──
    let (people, locations): (Vec<Person>, Vec<Location>) = use_hook(seed_people_locations);
    let people_by_id: HashMap<Uuid, Person> = people.iter().map(|p| (p.id, p.clone())).collect();
    let locations_by_id: HashMap<Uuid, Location> =
        locations.iter().map(|l| (l.id, l.clone())).collect();

    let mut items = use_signal::<Vec<InventoryItem>>(Vec::new);
    let mut events = use_signal::<Vec<CheckoutEvent>>(Vec::new);
    let mut status_msg = use_signal(|| "starting…".to_string());

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let i = items_repo.clone();
        let e = events_repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                let mk = || Page {
                    index: 0,
                    size: 500,
                };
                if let Ok(list) = i.list(mk(), None, None).await {
                    items.set(list.items);
                }
                if let Ok(list) = e.list(mk(), None, None).await {
                    events.set(list.items);
                }
            }
        });
        tx
    });

    // Seed demo data once on mount.
    use_effect({
        let items_repo = items_repo.clone();
        let events_repo = events_repo.clone();
        let tx = refresh_tx.clone();
        let people = people.clone();
        let locations = locations.clone();
        move || {
            let items_repo = items_repo.clone();
            let events_repo = events_repo.clone();
            let tx = tx.clone();
            let people = people.clone();
            let locations = locations.clone();
            spawn_local(async move {
                // Skip seeding if there's already data (e.g. server pushed it).
                let already = items_repo
                    .list(Page { index: 0, size: 1 }, None, None)
                    .await
                    .map(|l| l.total > 0)
                    .unwrap_or(false);
                if already {
                    let _ = tx.unbounded_send(());
                    return;
                }
                for n in 0..18u32 {
                    let mut wire: InventoryItem = Faker.fake();
                    // Bind every other item to a known location for the demo.
                    let loc = locations.get((n as usize) % locations.len()).cloned();
                    let create = InventoryItemCreate {
                        name: wire.name.clone(),
                        category: wire.category.clone(),
                        status: wire.status.clone(),
                        manufacturer: wire.manufacturer.clone(),
                        model: wire.model.clone(),
                        serial_number: wire.serial_number.clone(),
                        qr_code: wire.qr_code.clone(),
                        owner_id: None,
                        location_id: loc.as_ref().map(|l| l.id),
                        sub_location: wire.sub_location.clone(),
                        value_cents: wire.value_cents,
                        warranty_until: wire.warranty_until,
                        vendor: wire.vendor.clone(),
                        purchase_order: wire.purchase_order.clone(),
                        acquired_at: wire.acquired_at,
                        photo_url: None,
                        notes: wire.notes.clone(),
                        tags: std::mem::take(&mut wire.tags),
                    };
                    let _ = items_repo.create(create).await;
                }
                // 4 in-flight checkouts — assign the first 4 active items to people.
                let all = items_repo
                    .list(
                        Page {
                            index: 0,
                            size: 500,
                        },
                        None,
                        None,
                    )
                    .await
                    .ok();
                if let Some(all) = all {
                    for (i, item) in all.items.iter().take(4).enumerate() {
                        let person = &people[i % people.len()];
                        let _ = events_repo
                            .create(CheckoutEventCreate {
                                item_id: item.id,
                                person_id: Some(person.id),
                                checked_out_at: Utc::now() - Duration::days((i + 1) as i64),
                                due_at: Some(Utc::now() + Duration::days(7)),
                                returned_at: None,
                                note: Some(format!("demo checkout #{i}")),
                            })
                            .await;
                        let _ = items_repo
                            .update(
                                item.id,
                                InventoryItemUpdate {
                                    status: Some("checked-out".into()),
                                    owner_id: Some(Some(person.id)),
                                    ..Default::default()
                                },
                            )
                            .await;
                    }
                }
                let _ = tx.unbounded_send(());
            });
        }
    });

    let _session: Rc<Option<sync::SyncSession>> = use_hook({
        let doc = doc.clone();
        let tx_for_sync = refresh_tx.clone();
        move || {
            let _ = tx_for_sync.unbounded_send(());
            let ws_url = sync::sync_url(&format!("/sync/{}", sync::WORKSPACE_DOC_ID));
            let tx = tx_for_sync.clone();
            match sync::connect(&ws_url, &doc, move || {
                let _ = tx.unbounded_send(());
            }) {
                Ok(s) => {
                    status_msg.set(format!("connected to {ws_url}"));
                    Rc::new(Some(s))
                }
                Err(e) => {
                    status_msg.set(format!("ws connect failed: {e:?}"));
                    Rc::new(None)
                }
            }
        }
    });

    let on_open = {
        move |id: Uuid| {
            tracing::debug!(?id, "open inventory item");
        }
    };
    let on_checkout = {
        let items_repo = items_repo.clone();
        let events_repo = events_repo.clone();
        let tx = refresh_tx.clone();
        let people = people.clone();
        move |id: Uuid| {
            let items_repo = items_repo.clone();
            let events_repo = events_repo.clone();
            let tx = tx.clone();
            let person = people.first().cloned();
            spawn_local(async move {
                if let Some(p) = person {
                    let _ = events_repo
                        .create(CheckoutEventCreate {
                            item_id: id,
                            person_id: Some(p.id),
                            checked_out_at: Utc::now(),
                            due_at: Some(Utc::now() + Duration::days(7)),
                            returned_at: None,
                            note: None,
                        })
                        .await;
                    let _ = items_repo
                        .update(
                            id,
                            InventoryItemUpdate {
                                status: Some("checked-out".into()),
                                owner_id: Some(Some(p.id)),
                                ..Default::default()
                            },
                        )
                        .await;
                }
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_checkin = {
        let items_repo = items_repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let items_repo = items_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = items_repo
                    .update(
                        id,
                        InventoryItemUpdate {
                            status: Some("active".into()),
                            owner_id: Some(None),
                            ..Default::default()
                        },
                    )
                    .await;
                let _ = tx.unbounded_send(());
            });
        }
    };
    let on_retire = {
        let items_repo = items_repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let items_repo = items_repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = items_repo
                    .update(
                        id,
                        InventoryItemUpdate {
                            status: Some("retired".into()),
                            ..Default::default()
                        },
                    )
                    .await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    rsx! {
        div { class: "mx-auto flex max-w-7xl flex-col gap-4 p-6 lg:p-10",
            InventoryDashboard {
                items: items(),
                events: events(),
                people_by_id: people_by_id,
                locations_by_id: locations_by_id,
                status: status_msg(),
                on_open: on_open,
                on_checkout: on_checkout,
                on_checkin: on_checkin,
                on_retire: on_retire,
            }
        }
    }
}

/// 4 people + 3 studios with one location each. FUTURE: pull from the
/// real `PersonRepoLoro` / `LocationRepoLoro` once the route holds
/// references to them.
fn seed_people_locations() -> (Vec<Person>, Vec<Location>) {
    let mk_person = |name: &str| {
        let mut p: Person = Faker.fake();
        p.name = name.into();
        p
    };
    let mk_loc = |name: &str| {
        let mut l: Location = Faker.fake();
        l.name = name.into();
        l
    };
    let people = vec![
        mk_person("Ada Lovelace"),
        mk_person("Grace Hopper"),
        mk_person("Alan Turing"),
        mk_person("Tim Berners-Lee"),
    ];
    let locations = vec![mk_loc("Studio A"), mk_loc("Studio B"), mk_loc("Mobile Rig")];
    (people, locations)
}
