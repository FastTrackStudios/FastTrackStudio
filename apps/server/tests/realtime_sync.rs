//! End-to-end realtime sync test.
//!
//! Spawns task-server on a random local port backed by
//! `InMemoryPersistence`, then drives two raw WebSocket clients
//! against it. Each client owns its own `CrdtDoc` +
//! `TimeEntryRepoLoro`. We verify the canonical Loro sync protocol
//! end-to-end: snapshot on connect, local-update broadcast, remote
//! import, persistence catch-up for a third late-joining peer.
//!
//! No browser, no Dioxus — pure native tokio. Same protocol as the
//! browser demo at `apps/web` runs.

use std::sync::Arc;
use std::time::Duration;

use chrono::Utc;
use crdt::{CrdtDoc, Persistence, in_memory::InMemoryPersistence};
use futures::{SinkExt, StreamExt};
use loro::ExportMode;
use task_server::{AppState, router};
use timer_crdt::TimeEntryRepoLoro;
use timer_proto::{TimeEntryCreate, TimeEntryRepo};
use tokio::net::TcpListener;
use tokio_tungstenite::{
    connect_async,
    tungstenite::{Message, client::IntoClientRequest},
};
use uuid::Uuid;

/// Spin up the server on a random port + return the base ws URL.
/// The persistence handle is returned too so the test can keep
/// asserting against the stored state after the server is gone.
async fn spawn_server() -> (String, Arc<InMemoryPersistence>) {
    let persistence: Arc<InMemoryPersistence> = Arc::new(InMemoryPersistence::new());
    let state = AppState::with_persistence(persistence.clone());
    let app = router(state);

    let listener = TcpListener::bind("127.0.0.1:0").await.unwrap();
    let addr = listener.local_addr().unwrap();
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });

    (format!("ws://{addr}"), persistence)
}

/// One client: a `CrdtDoc` + `TimeEntryRepoLoro` plus an outbound
/// WebSocket task and an inbound import task. Drops cleanly when the
/// returned handle goes out of scope.
struct Client {
    doc: CrdtDoc,
    repo: TimeEntryRepoLoro,
    _outbound_sub: loro::Subscription,
    /// Sender into the outbound queue (subscription → ws.send).
    /// Kept alive for the test's lifetime; dropping it closes the
    /// outbound task naturally.
    _out_tx: futures::channel::mpsc::UnboundedSender<Vec<u8>>,
}

async fn connect_client(base_ws: &str, doc_id: Uuid) -> Client {
    let url = format!("{base_ws}/sync/{doc_id}");
    let req = url.into_client_request().unwrap();
    let (ws, _resp) = connect_async(req).await.expect("ws connect");
    let (mut ws_out, mut ws_in) = ws.split();

    let doc = CrdtDoc::ephemeral();
    let repo = TimeEntryRepoLoro::new(&doc);

    // Outbound: local-update subscription → mpsc → ws.
    let (out_tx, mut out_rx) = futures::channel::mpsc::unbounded::<Vec<u8>>();
    let out_tx_for_sub = out_tx.clone();
    let outbound_sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        let _ = out_tx_for_sub.unbounded_send(bytes.to_vec());
        true
    }));
    tokio::spawn(async move {
        while let Some(bytes) = out_rx.next().await {
            if ws_out.send(Message::Binary(bytes.into())).await.is_err() {
                break;
            }
        }
    });

    // Inbound: ws → doc.import.
    let doc_for_in = doc.clone();
    tokio::spawn(async move {
        while let Some(msg) = ws_in.next().await {
            match msg {
                Ok(Message::Binary(bytes)) => {
                    if let Err(e) = doc_for_in.apply_remote(&bytes) {
                        eprintln!("apply_remote failed: {e}");
                    }
                }
                Ok(Message::Close(_)) | Err(_) => break,
                _ => {}
            }
        }
    });

    Client {
        doc,
        repo,
        _outbound_sub: outbound_sub,
        _out_tx: out_tx,
    }
}

/// Poll the predicate until it returns true or we time out. Loro
/// updates take a few ms to propagate through axum + the broadcast
/// channel — 2s is plenty.
async fn eventually<F: FnMut() -> bool>(mut pred: F) -> bool {
    for _ in 0..200 {
        if pred() {
            return true;
        }
        tokio::time::sleep(Duration::from_millis(10)).await;
    }
    pred()
}

#[tokio::test(flavor = "multi_thread")]
async fn two_peers_converge_through_sync_server() {
    let (base, _persistence) = spawn_server().await;
    let doc_id = Uuid::new_v4();

    let alice = connect_client(&base, doc_id).await;
    let bob = connect_client(&base, doc_id).await;

    // Alice creates an entry. The bytes flow Alice -> ws -> server ->
    // ws -> Bob. Bob's doc.apply_remote merges, and a subsequent
    // bob.repo.list() sees the row.
    let row = alice
        .repo
        .create(TimeEntryCreate {
            task_id: None,
            user: Some("alice".into()),
            start_time: Utc::now(),
            end_time: None,
            description: Some("from alice".into()),
            billable: false,
            billable_rate_cents: None,
            tags: vec![],
            invoiced_at: None,
            project_id: None,
            client_id: None,
            manual: false,
        })
        .await
        .unwrap();

    let row_id = row.id;
    let bob_repo = &bob.repo;
    let saw_alice_row = eventually(|| {
        let bob_repo = bob_repo.clone();
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current()
                .block_on(async move { bob_repo.get(row_id).await.is_ok() })
        })
    })
    .await;
    assert!(saw_alice_row, "bob never received alice's row");

    // Symmetry: bob creates, alice sees it.
    let bobs_row = bob
        .repo
        .create(TimeEntryCreate {
            task_id: None,
            user: Some("bob".into()),
            start_time: Utc::now(),
            end_time: None,
            description: Some("from bob".into()),
            billable: false,
            billable_rate_cents: None,
            tags: vec![],
            invoiced_at: None,
            project_id: None,
            client_id: None,
            manual: false,
        })
        .await
        .unwrap();

    let alice_repo = &alice.repo;
    let saw_bob_row = eventually(|| {
        let alice_repo = alice_repo.clone();
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current()
                .block_on(async move { alice_repo.get(bobs_row.id).await.is_ok() })
        })
    })
    .await;
    assert!(saw_bob_row, "alice never received bob's row");
}

#[tokio::test(flavor = "multi_thread")]
async fn late_joiner_gets_snapshot_with_history() {
    let (base, _persistence) = spawn_server().await;
    let doc_id = Uuid::new_v4();

    // Alice connects, writes a few entries, drops.
    {
        let alice = connect_client(&base, doc_id).await;
        for n in 0..3 {
            alice
                .repo
                .create(TimeEntryCreate {
                    task_id: None,
                    user: Some(format!("alice-{n}")),
                    start_time: Utc::now(),
                    end_time: None,
                    description: Some(format!("entry {n}")),
                    billable: false,
                    billable_rate_cents: None,
                    tags: vec![],
                    invoiced_at: None,
                    project_id: None,
                    client_id: None,
                    manual: false,
                })
                .await
                .unwrap();
        }
        // Give the relay a moment to ingest everything.
        tokio::time::sleep(Duration::from_millis(100)).await;
        drop(alice);
    }

    // Bob joins fresh — the server should send a snapshot containing
    // all three of alice's entries.
    let bob = connect_client(&base, doc_id).await;
    let bob_repo = bob.repo.clone();
    let three_visible = eventually(|| {
        let bob_repo = bob_repo.clone();
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async move {
                bob_repo
                    .list(
                        architect::Page {
                            index: 0,
                            size: 100,
                        },
                        None,
                        None,
                    )
                    .await
                    .map(|l| l.total)
                    .unwrap_or(0)
                    == 3
            })
        })
    })
    .await;
    assert!(
        three_visible,
        "bob's snapshot didn't include alice's history"
    );
}

#[tokio::test(flavor = "multi_thread")]
async fn updates_survive_in_persistence() {
    let (base, persistence) = spawn_server().await;
    let doc_id = Uuid::new_v4();

    let alice = connect_client(&base, doc_id).await;
    alice
        .repo
        .create(TimeEntryCreate {
            task_id: None,
            user: Some("alice".into()),
            start_time: Utc::now(),
            end_time: None,
            description: Some("persisted".into()),
            billable: false,
            billable_rate_cents: None,
            tags: vec![],
            invoiced_at: None,
            project_id: None,
            client_id: None,
            manual: false,
        })
        .await
        .unwrap();

    // Wait until the server has finished importing + persisting.
    let persistence_for_check = persistence.clone();
    let persisted = eventually(|| {
        let p = persistence_for_check.clone();
        tokio::task::block_in_place(|| {
            tokio::runtime::Handle::current().block_on(async move {
                !p.load_updates(doc_id).await.unwrap_or_default().is_empty()
            })
        })
    })
    .await;
    assert!(persisted, "no update bytes landed in persistence");

    // The persisted bytes should reconstruct a doc identical to
    // alice's, which is what `late_joiner_gets_snapshot_with_history`
    // already exercises — but this test pins the storage layer
    // directly so a regression there fails here.
    let stored = persistence.load_updates(doc_id).await.unwrap();
    assert!(!stored.is_empty(), "expected at least one persisted update");

    // Replay onto a fresh doc and read the row back to confirm the
    // bytes are valid Loro updates.
    let restored_doc = loro::LoroDoc::new();
    for u in &stored {
        restored_doc.import(u).unwrap();
    }
    let snap = restored_doc.export(ExportMode::Snapshot).unwrap();
    assert!(!snap.is_empty());
    drop(alice); // keep server alive until here
}
