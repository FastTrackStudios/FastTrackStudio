//! End-to-end CRDT-over-vox sync test.
//!
//! Two peers come online + receive a snapshot, disconnect, make
//! divergent local edits on different tasks (conflict-free at the
//! field level), reconnect, push their accumulated bytes via
//! `apply_update`, and observe each other's edits via a fresh
//! `subscribe` stream. Asserts both peers' local docs converge to
//! the same state.

use std::sync::{Arc, Mutex};
use std::time::Duration;

use crdt::CrdtDoc;
use crdt_seaorm::SeaOrmPersistence;
use project_crdt::TaskRepoLoro;
use project_proto::architect::Page;
use project_proto::{
    ProjectCreate, ProjectRepo, TaskCreate, TaskRepo, TaskUpdate, UpdateBytes, WorkspaceSyncClient,
};
use task_db::{default_database_url, open_and_migrate};
use task_server::{AppState, router};
use uuid::Uuid;
use vox::Rx;

/// One simulated peer. Owns its own LoroDoc and a buffer of
/// locally-committed update bytes captured via
/// `subscribe_local_update` — drained on reconnect.
struct Peer {
    doc: Arc<CrdtDoc>,
    pending: Arc<Mutex<Vec<Vec<u8>>>>,
}

impl Peer {
    fn new() -> Self {
        let doc = Arc::new(CrdtDoc::ephemeral());
        let pending: Arc<Mutex<Vec<Vec<u8>>>> = Arc::new(Mutex::new(Vec::new()));
        let pcb = pending.clone();
        let sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
            pcb.lock().unwrap().push(bytes.to_vec());
            true
        }));
        // Subscription handle must outlive the doc for the
        // callback to keep firing. The doc lives the whole test;
        // leaking is fine.
        std::mem::forget(sub);
        Self { doc, pending }
    }

    fn drain_pending(&self) -> Vec<Vec<u8>> {
        std::mem::take(&mut *self.pending.lock().unwrap())
    }
}

/// Spawn task-server on `127.0.0.1:0`, return its `ws://…/vox` URL
/// plus the seeded `(task_a_id, task_b_id)`.
async fn boot_server() -> eyre::Result<(String, Uuid, Uuid)> {
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let state = AppState::new(persistence).await?;

    let project = state
        .project_repo
        .create(ProjectCreate {
            name: "Test Project".into(),
        })
        .await
        .map_err(|e| eyre::eyre!("seed project: {e}"))?;
    let task_a = state
        .task_repo
        .create(TaskCreate {
            project_id: project.id,
            title: "Task A".into(),
            done: false,
        })
        .await
        .map_err(|e| eyre::eyre!("seed task A: {e}"))?;
    let task_b = state
        .task_repo
        .create(TaskCreate {
            project_id: project.id,
            title: "Task B".into(),
            done: false,
        })
        .await
        .map_err(|e| eyre::eyre!("seed task B: {e}"))?;

    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    let url = format!("ws://127.0.0.1:{port}/vox");

    Ok((url, task_a.id, task_b.id))
}

async fn open_subscribe(url: &str) -> eyre::Result<(tokio::task::JoinHandle<()>, Rx<UpdateBytes>)> {
    let client: WorkspaceSyncClient = vox::connect(url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("subscribe connect: {e:?}"))?;
    let (tx, rx) = vox::channel::<UpdateBytes>();
    let handle = tokio::spawn(async move {
        let _ = client.subscribe(tx).await;
    });
    Ok((handle, rx))
}

async fn open_apply_client(url: &str) -> eyre::Result<WorkspaceSyncClient> {
    vox::connect(url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply connect: {e:?}"))
}

async fn task_done(doc: &CrdtDoc, id: Uuid) -> Option<bool> {
    let task_repo = TaskRepoLoro::new(doc);
    let page = task_repo
        .list(
            Page {
                index: 0,
                size: 1000,
            },
            None,
            None,
        )
        .await
        .ok()?;
    page.items.into_iter().find(|t| t.id == id).map(|t| t.done)
}

async fn task_count(doc: &CrdtDoc) -> usize {
    let task_repo = TaskRepoLoro::new(doc);
    task_repo
        .list(
            Page {
                index: 0,
                size: 1000,
            },
            None,
            None,
        )
        .await
        .map(|p| p.items.len())
        .unwrap_or(0)
}

/// Drain chunks into `doc` until `cond(doc)` is true or `timeout`
/// elapses.
async fn wait_until<F, Fut>(
    doc: &CrdtDoc,
    rx: &mut Rx<UpdateBytes>,
    timeout: Duration,
    mut cond: F,
) -> eyre::Result<()>
where
    F: FnMut() -> Fut,
    Fut: std::future::Future<Output = bool>,
{
    let stop = tokio::time::Instant::now() + timeout;
    loop {
        if cond().await {
            return Ok(());
        }
        if tokio::time::Instant::now() >= stop {
            return Err(eyre::eyre!("wait_until: condition not met within timeout"));
        }
        match tokio::time::timeout(Duration::from_millis(100), rx.recv()).await {
            Ok(Ok(Some(msg))) => {
                doc.apply_remote(&msg.get().0)
                    .map_err(|e| eyre::eyre!("apply_remote: {e}"))?;
            }
            Ok(Ok(None)) => return Err(eyre::eyre!("wait_until: stream closed")),
            Ok(Err(e)) => return Err(eyre::eyre!("wait_until: rx.recv: {e:?}")),
            Err(_) => {}
        }
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn offline_divergent_edits_converge() -> eyre::Result<()> {
    let (url, task_a_id, task_b_id) = boot_server().await?;

    // ── Phase 1: both peers come online + receive the snapshot ────
    let peer_a = Peer::new();
    let peer_b = Peer::new();
    let (sub_a, mut rx_a) = open_subscribe(&url).await?;
    let (sub_b, mut rx_b) = open_subscribe(&url).await?;

    let doc_a = peer_a.doc.clone();
    wait_until(&peer_a.doc, &mut rx_a, Duration::from_secs(5), || {
        let d = doc_a.clone();
        async move { task_count(&d).await >= 2 }
    })
    .await?;
    let doc_b = peer_b.doc.clone();
    wait_until(&peer_b.doc, &mut rx_b, Duration::from_secs(5), || {
        let d = doc_b.clone();
        async move { task_count(&d).await >= 2 }
    })
    .await?;

    assert_eq!(task_done(&peer_a.doc, task_a_id).await, Some(false));
    assert_eq!(task_done(&peer_b.doc, task_b_id).await, Some(false));

    // ── Phase 2: GO OFFLINE — abort the subscribe streams ────────
    sub_a.abort();
    sub_b.abort();

    // ── Phase 3: divergent local edits ───────────────────────────
    TaskRepoLoro::new(&peer_a.doc)
        .update(
            task_a_id,
            TaskUpdate {
                done: Some(true),
                ..Default::default()
            },
        )
        .await
        .map_err(|e| eyre::eyre!("peer A edit: {e}"))?;
    TaskRepoLoro::new(&peer_b.doc)
        .update(
            task_b_id,
            TaskUpdate {
                done: Some(true),
                ..Default::default()
            },
        )
        .await
        .map_err(|e| eyre::eyre!("peer B edit: {e}"))?;

    // Local reads: each peer sees its own edit, NOT the other's.
    assert_eq!(task_done(&peer_a.doc, task_a_id).await, Some(true));
    assert_eq!(task_done(&peer_a.doc, task_b_id).await, Some(false));
    assert_eq!(task_done(&peer_b.doc, task_b_id).await, Some(true));
    assert_eq!(task_done(&peer_b.doc, task_a_id).await, Some(false));

    // ── Phase 4: RECONNECT — fresh subscribe streams + apply ──────
    let (sub_a2, mut rx_a2) = open_subscribe(&url).await?;
    let (sub_b2, mut rx_b2) = open_subscribe(&url).await?;
    let apply_a = open_apply_client(&url).await?;
    let apply_b = open_apply_client(&url).await?;

    for bytes in peer_a.drain_pending() {
        apply_a
            .apply_update(UpdateBytes(bytes))
            .await
            .map_err(|e| eyre::eyre!("apply A: {e:?}"))?;
    }
    for bytes in peer_b.drain_pending() {
        apply_b
            .apply_update(UpdateBytes(bytes))
            .await
            .map_err(|e| eyre::eyre!("apply B: {e:?}"))?;
    }

    // ── Phase 5: drain until each peer sees the OTHER's edit ─────
    let doc_a = peer_a.doc.clone();
    wait_until(&peer_a.doc, &mut rx_a2, Duration::from_secs(5), || {
        let d = doc_a.clone();
        async move { task_done(&d, task_b_id).await == Some(true) }
    })
    .await?;
    let doc_b = peer_b.doc.clone();
    wait_until(&peer_b.doc, &mut rx_b2, Duration::from_secs(5), || {
        let d = doc_b.clone();
        async move { task_done(&d, task_a_id).await == Some(true) }
    })
    .await?;

    // ── Phase 6: assert convergence ─────────────────────────────
    assert_eq!(task_done(&peer_a.doc, task_a_id).await, Some(true));
    assert_eq!(task_done(&peer_a.doc, task_b_id).await, Some(true));
    assert_eq!(task_done(&peer_b.doc, task_a_id).await, Some(true));
    assert_eq!(task_done(&peer_b.doc, task_b_id).await, Some(true));

    sub_a2.abort();
    sub_b2.abort();
    Ok(())
}
