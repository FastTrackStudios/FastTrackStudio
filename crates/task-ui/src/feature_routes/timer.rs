//! Timer feature route. Holds a local `CrdtDoc` + `TimeEntryRepoLoro`,
//! syncs over WebSocket, drives the Solidtime-style dashboard.
//!
//! FUTURE: project + client lists are still seeded locally here.
//! Real wiring needs `ProjectRepoLoro` + `ClientRepoLoro` queries —
//! once those land we pull them from the workspace doc the same way
//! we pull TimeEntries today.

use std::rc::Rc;

use architect::Page;
use chrono::{Duration, Utc};
use dioxus::prelude::*;
use futures_channel::mpsc;
use futures_util::StreamExt;
use invoice_proto::Client;
use project_proto::Project;
use timer_crdt::{CrdtDoc, TimeEntryRepoLoro};
use timer_proto::{TimeEntry, TimeEntryCreate, TimeEntryRepo, TimeEntryUpdate};
use timer_ui::{SolidtimeDashboard, TimeEntryStartInput};
use uuid::Uuid;
use wasm_bindgen_futures::spawn_local;

use crate::sync;

#[component]
pub fn TimeEntryView() -> Element {
    let repo: Rc<TimeEntryRepoLoro> = use_hook(|| {
        let doc = CrdtDoc::ephemeral();
        Rc::new(TimeEntryRepoLoro::new(&doc))
    });
    let doc: Rc<CrdtDoc> = use_hook(|| Rc::new(CrdtDoc::from_loro(repo.doc().clone())));

    let mut items = use_signal::<Vec<TimeEntry>>(Vec::new);
    let mut _status_msg = use_signal(|| "starting…".to_string());

    // Seed sample projects + clients + entries on first mount.
    let (sample_projects, sample_clients) = use_hook(sample_projects_and_clients);
    use_hook({
        let repo = repo.clone();
        let projects = sample_projects.clone();
        let clients = sample_clients.clone();
        move || {
            let entries = sample_entries(&projects, &clients);
            let repo = repo.clone();
            spawn_local(async move {
                for e in entries {
                    let _ = repo.create(e).await;
                }
            });
        }
    });

    let refresh_tx: mpsc::UnboundedSender<()> = use_hook(|| {
        let (tx, mut rx) = mpsc::unbounded::<()>();
        let repo_for_loop = repo.clone();
        spawn_local(async move {
            while rx.next().await.is_some() {
                if let Ok(list) = repo_for_loop
                    .list(
                        Page {
                            index: 0,
                            size: 500,
                        },
                        None,
                        None,
                    )
                    .await
                {
                    items.set(list.items);
                }
            }
        });
        tx
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
                    _status_msg.set(format!("connected to {ws_url}"));
                    Rc::new(Some(s))
                }
                Err(e) => {
                    _status_msg.set(format!("ws connect failed: {e:?}"));
                    Rc::new(None)
                }
            }
        }
    });

    let on_start = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        let projects = sample_projects.clone();
        move |input: TimeEntryStartInput| {
            let client_id = input.project_id.and_then(|pid| {
                projects
                    .iter()
                    .find(|p| p.id == pid)
                    .map(|_| input.client_id)
                    .flatten()
            });
            let payload = TimeEntryCreate {
                task_id: None,
                project_id: input.project_id,
                client_id,
                user: None,
                start_time: Utc::now(),
                end_time: None,
                description: input.description,
                billable: input.billable,
                manual: false,
                billable_rate_cents: Some(15000),
                tags: input.tags,
                invoiced_at: None,
            };
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_stop = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo
                    .update(
                        id,
                        TimeEntryUpdate {
                            end_time: Some(Some(Utc::now())),
                            ..Default::default()
                        },
                    )
                    .await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_create = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |p: TimeEntryCreate| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(p).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_edit = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |(id, upd): (Uuid, TimeEntryUpdate)| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.update(id, upd).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_delete = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        move |id: Uuid| {
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.delete(id).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    let on_duplicate = {
        let repo = repo.clone();
        let tx = refresh_tx.clone();
        let entries_signal = items;
        move |id: Uuid| {
            let snapshot = entries_signal.read().clone();
            let Some(src) = snapshot.into_iter().find(|e| e.id == id) else {
                return;
            };
            let payload = TimeEntryCreate {
                task_id: src.task_id,
                project_id: src.project_id,
                client_id: src.client_id,
                user: src.user,
                start_time: Utc::now(),
                end_time: src
                    .end_time
                    .map(|_| Utc::now() + (src.end_time.unwrap() - src.start_time)),
                description: src.description,
                billable: src.billable,
                manual: true,
                billable_rate_cents: src.billable_rate_cents,
                tags: src.tags,
                invoiced_at: None,
            };
            let repo = repo.clone();
            let tx = tx.clone();
            spawn_local(async move {
                let _ = repo.create(payload).await;
                let _ = tx.unbounded_send(());
            });
        }
    };

    rsx! {
        div { class: "mx-auto flex w-full max-w-7xl flex-col gap-4 p-4 lg:p-8",
            SolidtimeDashboard {
                entries: items(),
                projects: sample_projects.clone(),
                clients: sample_clients.clone(),
                on_start,
                on_stop,
                on_create,
                on_edit,
                on_delete,
                on_duplicate,
            }
        }
    }
}

// ── Sample data ────────────────────────────────────────────────────────
//
// FUTURE: replace with real `ProjectRepoLoro` + `ClientRepoLoro` reads
// once those feature wiring lands in task-ui.

fn sample_projects_and_clients() -> (Vec<Project>, Vec<Client>) {
    let now = Utc::now();
    let acme = Uuid::parse_str("11111111-1111-4111-8111-111111111111").unwrap();
    let bluegrass = Uuid::parse_str("22222222-2222-4222-8222-222222222222").unwrap();
    let internal = Uuid::parse_str("33333333-3333-4333-8333-333333333333").unwrap();

    let clients = vec![
        Client {
            id: acme,
            name: "Acme Corp".into(),
            email: Some("billing@acme.test".into()),
            phone: None,
            billing_address_line1: None,
            billing_address_line2: None,
            billing_city: None,
            billing_region: None,
            billing_postal_code: None,
            billing_country: None,
            currency: "USD".into(),
            default_rate_cents: Some(15000),
            notes: None,
            tags: Vec::new(),
            created_at: now,
            updated_at: now,
        },
        Client {
            id: bluegrass,
            name: "Bluegrass Studio".into(),
            email: Some("ops@bluegrass.test".into()),
            phone: None,
            billing_address_line1: None,
            billing_address_line2: None,
            billing_city: None,
            billing_region: None,
            billing_postal_code: None,
            billing_country: None,
            currency: "USD".into(),
            default_rate_cents: Some(12500),
            notes: None,
            tags: Vec::new(),
            created_at: now,
            updated_at: now,
        },
        Client {
            id: internal,
            name: "Internal".into(),
            email: None,
            phone: None,
            billing_address_line1: None,
            billing_address_line2: None,
            billing_city: None,
            billing_region: None,
            billing_postal_code: None,
            billing_country: None,
            currency: "USD".into(),
            default_rate_cents: None,
            notes: None,
            tags: Vec::new(),
            created_at: now,
            updated_at: now,
        },
    ];

    let projects = vec![
        Project {
            id: Uuid::parse_str("aaaaaaaa-aaaa-4aaa-8aaa-aaaaaaaaaaaa").unwrap(),
            name: "Acme Rebrand".into(),
            description: None,
            status: "active".into(),
            project_type: Some("client".into()),
            color: Some("#ef4444".into()),
            owner: None,
            created_at: now,
            updated_at: now,
        },
        Project {
            id: Uuid::parse_str("bbbbbbbb-bbbb-4bbb-8bbb-bbbbbbbbbbbb").unwrap(),
            name: "Studio Build".into(),
            description: None,
            status: "active".into(),
            project_type: Some("client".into()),
            color: Some("#3b82f6".into()),
            owner: None,
            created_at: now,
            updated_at: now,
        },
        Project {
            id: Uuid::parse_str("cccccccc-cccc-4ccc-8ccc-cccccccccccc").unwrap(),
            name: "Internal Tools".into(),
            description: None,
            status: "active".into(),
            project_type: Some("internal".into()),
            color: Some("#22c55e".into()),
            owner: None,
            created_at: now,
            updated_at: now,
        },
        Project {
            id: Uuid::parse_str("dddddddd-dddd-4ddd-8ddd-dddddddddddd").unwrap(),
            name: "Client X Mixing".into(),
            description: None,
            status: "active".into(),
            project_type: Some("client".into()),
            color: Some("#a855f7".into()),
            owner: None,
            created_at: now,
            updated_at: now,
        },
    ];

    (projects, clients)
}

fn sample_entries(projects: &[Project], clients: &[Client]) -> Vec<TimeEntryCreate> {
    let now = Utc::now();
    let acme_id = clients[0].id;
    let bluegrass_id = clients[1].id;
    let internal_id = clients[2].id;
    let p_rebrand = projects[0].id;
    let p_studio = projects[1].id;
    let p_internal = projects[2].id;
    let p_mix = projects[3].id;

    let descs = [
        "Logo concepts",
        "Mixing session — track 4",
        "Bugfix: WebSocket reconnect",
        "Client call — kickoff",
        "Pair programming",
        "Pre-master review",
        "Sprint planning",
        "Email triage",
        "Refactor sync layer",
        "Mastering pass",
        "Design review",
        "Architecture brainstorm",
    ];
    let tags = [
        vec!["design".to_string()],
        vec!["audio".to_string(), "mixing".to_string()],
        vec!["coding".to_string()],
        vec!["meeting".to_string(), "client".to_string()],
        vec!["coding".to_string()],
        vec!["audio".to_string(), "review".to_string()],
        vec!["meeting".to_string()],
        vec!["ops".to_string()],
        vec!["coding".to_string()],
        vec!["audio".to_string(), "mastering".to_string()],
        vec!["design".to_string(), "review".to_string()],
        vec!["coding".to_string()],
    ];

    let buckets: [(Uuid, Option<Uuid>, bool, u32); 4] = [
        (p_rebrand, Some(acme_id), true, 17500),
        (p_studio, Some(bluegrass_id), true, 12500),
        (p_internal, Some(internal_id), false, 0),
        (p_mix, Some(acme_id), true, 20000),
    ];

    let mut out: Vec<TimeEntryCreate> = Vec::new();
    for i in 0..29 {
        let day_offset = (i % 14) as i64;
        let hour = 9 + ((i * 3) % 8) as i64;
        let dur_min = 30 + ((i * 17) % 90) as i64;
        let start = now - Duration::days(day_offset) - Duration::hours(24 - hour);
        let end = start + Duration::minutes(dur_min);
        let bucket = buckets[i % buckets.len()];
        out.push(TimeEntryCreate {
            task_id: None,
            project_id: Some(bucket.0),
            client_id: bucket.1,
            user: Some("cody".into()),
            start_time: start,
            end_time: Some(end),
            description: Some(descs[i % descs.len()].to_string()),
            billable: bucket.2,
            manual: i % 5 == 0,
            billable_rate_cents: if bucket.3 == 0 { None } else { Some(bucket.3) },
            tags: tags[i % tags.len()].clone(),
            invoiced_at: None,
        });
    }
    // The "running" entry — no end_time.
    out.push(TimeEntryCreate {
        task_id: None,
        project_id: Some(p_studio),
        client_id: Some(bluegrass_id),
        user: Some("cody".into()),
        start_time: now - Duration::minutes(23),
        end_time: None,
        description: Some("Wiring up the time-tracking UI".into()),
        billable: true,
        manual: false,
        billable_rate_cents: Some(15000),
        tags: vec!["coding".into()],
        invoiced_at: None,
    });
    out
}
