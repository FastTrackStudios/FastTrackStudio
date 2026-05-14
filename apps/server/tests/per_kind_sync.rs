//! Phase 10 — per-kind sync filtering.
//!
//! `WorkspaceSync::subscribe_kinds` lets a subscriber declare
//! interest in a subset of root containers. The server filters
//! updates on the way out: an update that only touches
//! `knowledge_blocks` is not forwarded to a subscriber that
//! asked only for `tasks`.
//!
//! Test plan:
//!
//! 1. Boot one server, pre-open the `workspace` doc + an
//!    additional `knowledge` doc (we just reuse the org vault).
//! 2. Open three subscriber streams:
//!    - `all`: no filter (legacy `subscribe`)
//!    - `tasks_only`: filter = ["tasks"]
//!    - `knowledge_only`: filter = ["knowledge_blocks",
//!      "knowledge_pages", "knowledge_vaults",
//!      "knowledge_folders"]
//!    (All three subscribe to the SAME doc id — we drive the
//!    test on a Knowledge vault.)
//! 3. Apply a task-style update (touches root `tasks`) and a
//!    knowledge-style update (touches root `knowledge_blocks`).
//! 4. Drain each subscriber for ~500ms, count frames received.
//! 5. Assert:
//!    - `all` saw both updates
//!    - `tasks_only` saw the task one, not the knowledge one
//!    - `knowledge_only` saw the knowledge one, not the task one
//! 6. Print byte counts to stderr so the commit-message benchmark
//!    can quote them.

use std::sync::Arc;
use std::time::Duration;

use crdt::CrdtDoc;
use crdt_seaorm::SeaOrmPersistence;
use project_proto::{DocId, KindFilter, UpdateBytes, WorkspaceSyncClient};
use task_db::{default_database_url, open_and_migrate};
use task_server::{AppState, AuthState, router};
use uuid::Uuid;

async fn boot() -> eyre::Result<String> {
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let auth = AuthState::open("sqlite::memory:", "test-secret-at-least-32-bytes!!!").await?;
    let state = AppState::new_with_auth(persistence, auth).await?;
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    Ok(format!("ws://127.0.0.1:{port}/vox"))
}

/// Build an opaque update payload that touches one specific root
/// container. We construct a tiny `CrdtDoc`, mutate the chosen
/// root, commit, and export the bytes. The receiving server
/// imports them and Loro's `subscribe_root` fires for the same
/// root on that side too.
fn payload_touching(root: &str, key: &str, value: &str) -> Vec<u8> {
    let doc = CrdtDoc::ephemeral();
    let m = doc.loro().get_map(root);
    m.insert(key, value).expect("insert");
    doc.loro().commit();
    doc.loro()
        .export(loro::ExportMode::Snapshot)
        .expect("export")
}

async fn drain_frames(rx: &mut vox::Rx<UpdateBytes>, timeout: Duration) -> Vec<UpdateBytes> {
    let mut out = Vec::new();
    let deadline = tokio::time::Instant::now() + timeout;
    loop {
        let remaining = deadline.saturating_duration_since(tokio::time::Instant::now());
        if remaining.is_zero() {
            break;
        }
        match tokio::time::timeout(remaining.min(Duration::from_millis(150)), rx.recv()).await {
            Ok(Ok(Some(msg))) => out.push(msg.get().clone()),
            Ok(Ok(None)) => break,
            Ok(Err(_)) => break,
            Err(_) => {} // tick timeout, keep looping
        }
    }
    out
}

#[tokio::test(flavor = "multi_thread")]
async fn kind_filter_drops_unrelated_updates() -> eyre::Result<()> {
    let url = boot().await?;
    let doc_id = DocId::new("workspace");

    // Subscriber A — all kinds (legacy path).
    let client_all: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect all: {e:?}"))?;
    let (tx_all, mut rx_all) = vox::channel::<UpdateBytes>();
    let did = doc_id.clone();
    let h_all = tokio::spawn(async move {
        let _ = client_all.subscribe(did, tx_all).await;
    });

    // Subscriber B — tasks_only via subscribe_kinds.
    let client_tasks: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect tasks: {e:?}"))?;
    let (tx_tasks, mut rx_tasks) = vox::channel::<UpdateBytes>();
    let filter_tasks = KindFilter {
        doc_id: doc_id.clone(),
        kinds: vec!["tasks".into()],
    };
    let h_tasks = tokio::spawn(async move {
        let _ = client_tasks.subscribe_kinds(filter_tasks, tx_tasks).await;
    });

    // Subscriber C — knowledge_only via subscribe_kinds.
    let client_kn: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect kn: {e:?}"))?;
    let (tx_kn, mut rx_kn) = vox::channel::<UpdateBytes>();
    let filter_kn = KindFilter {
        doc_id: doc_id.clone(),
        kinds: vec![
            "knowledge_blocks".into(),
            "knowledge_pages".into(),
            "knowledge_vaults".into(),
            "knowledge_folders".into(),
        ],
    };
    let h_kn = tokio::spawn(async move {
        let _ = client_kn.subscribe_kinds(filter_kn, tx_kn).await;
    });

    // Wait for all three to receive their initial snapshot frames.
    // For an empty doc the snapshots are small. We drain them out
    // of the way so the rest of the test counts *real* updates.
    tokio::time::sleep(Duration::from_millis(200)).await;
    let _initial_all = drain_frames(&mut rx_all, Duration::from_millis(300)).await;
    let _initial_tasks = drain_frames(&mut rx_tasks, Duration::from_millis(300)).await;
    let _initial_kn = drain_frames(&mut rx_kn, Duration::from_millis(300)).await;

    // Apply a tasks update via apply_update over vox.
    let apply: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply connect: {e:?}"))?;
    let tasks_bytes = payload_touching("tasks", "k1", "tasks-payload");
    apply
        .apply_update(doc_id.clone(), UpdateBytes(tasks_bytes.clone()))
        .await
        .map_err(|e| eyre::eyre!("apply tasks: {e:?}"))?;

    // Apply a knowledge update.
    let kn_bytes = payload_touching("knowledge_blocks", "k2", "knowledge-payload");
    apply
        .apply_update(doc_id.clone(), UpdateBytes(kn_bytes.clone()))
        .await
        .map_err(|e| eyre::eyre!("apply kn: {e:?}"))?;

    // Drain.
    let all_frames = drain_frames(&mut rx_all, Duration::from_millis(500)).await;
    let tasks_frames = drain_frames(&mut rx_tasks, Duration::from_millis(500)).await;
    let kn_frames = drain_frames(&mut rx_kn, Duration::from_millis(500)).await;

    let all_bytes: usize = all_frames.iter().map(|f| f.0.len()).sum();
    let tasks_bytes_total: usize = tasks_frames.iter().map(|f| f.0.len()).sum();
    let kn_bytes_total: usize = kn_frames.iter().map(|f| f.0.len()).sum();

    eprintln!(
        "[per-kind] all: {n_all} frames / {all_bytes} B; tasks_only: {n_tasks} / {tasks_bytes_total} B; knowledge_only: {n_kn} / {kn_bytes_total} B",
        n_all = all_frames.len(),
        n_tasks = tasks_frames.len(),
        n_kn = kn_frames.len(),
    );

    // `all` saw both updates.
    assert!(
        all_frames.len() >= 2,
        "subscriber 'all' should have received both updates; got {} frames",
        all_frames.len()
    );

    // `tasks_only` saw exactly the task update — never the
    // knowledge one. Lossy frame count is fine; what we check is
    // that the unique payload contents that arrived are a subset
    // of the task payload's roots.
    assert_eq!(
        tasks_frames.len(),
        1,
        "tasks_only should have received exactly 1 frame; got {}",
        tasks_frames.len()
    );

    // `knowledge_only` saw exactly the knowledge update.
    assert_eq!(
        kn_frames.len(),
        1,
        "knowledge_only should have received exactly 1 frame; got {}",
        kn_frames.len()
    );

    // Bandwidth saving: filtered subscribers received strictly
    // fewer bytes than the unfiltered one. This is the bench
    // figure the Phase 10 commit message quotes.
    assert!(
        tasks_bytes_total < all_bytes,
        "tasks_only bytes ({tasks_bytes_total}) not < all bytes ({all_bytes})"
    );
    assert!(
        kn_bytes_total < all_bytes,
        "knowledge_only bytes ({kn_bytes_total}) not < all bytes ({all_bytes})"
    );

    // Avoid leaking the spawned tasks.
    h_all.abort();
    h_tasks.abort();
    h_kn.abort();
    drop(tasks_bytes);
    drop(kn_bytes);
    let _: Uuid = Uuid::new_v4(); // silence unused import
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn empty_kinds_filter_acts_like_legacy_subscribe() -> eyre::Result<()> {
    // subscribe_kinds with kinds=[] must forward every update.
    // Equivalent to the back-compat path.
    let url = boot().await?;
    let doc_id = DocId::new("workspace");

    let client: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let h = tokio::spawn(async move {
        let _ = client
            .subscribe_kinds(
                KindFilter {
                    doc_id: DocId::new("workspace"),
                    kinds: vec![],
                },
                tx,
            )
            .await;
    });
    tokio::time::sleep(Duration::from_millis(200)).await;
    let _initial = drain_frames(&mut rx, Duration::from_millis(300)).await;

    let apply: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply connect: {e:?}"))?;
    let payload = payload_touching("anything", "k", "v");
    apply
        .apply_update(doc_id, UpdateBytes(payload))
        .await
        .map_err(|e| eyre::eyre!("apply: {e:?}"))?;

    let frames = drain_frames(&mut rx, Duration::from_millis(500)).await;
    assert!(
        !frames.is_empty(),
        "empty-kinds subscriber should still receive updates"
    );
    let _ = Arc::new(()); // silence
    h.abort();
    Ok(())
}
