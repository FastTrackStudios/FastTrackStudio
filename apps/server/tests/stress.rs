//! Sync stress test.
//!
//! Spins up N peers concurrently, each holding its own `CrdtDoc`
//! and a continuously-running `WorkspaceSync` subscribe stream.
//! Each peer fires M random `done`-toggles in quick succession.
//! Every locally-committed update is pushed via `apply_update`.
//!
//! The test then waits for quiescence (no new bytes for a short
//! window across all peers) and asserts that every peer's local
//! doc is byte-identical for every task it knows about.
//!
//! This catches bugs the happy-path test misses:
//! - Race between snapshot export and broadcast subscribe on the
//!   server.
//! - Lost updates under burst (broadcast lag, dropped frames).
//! - Convergence failures when many peers commit concurrently.
//! - Slow-subscriber recovery via the lagged-snapshot fallback.

use std::collections::BTreeMap;
use std::sync::{Arc, Mutex};
use std::time::Duration;

use crdt::CrdtDoc;
use crdt_seaorm::SeaOrmPersistence;
use project_crdt::TaskRepoLoro;
use project_proto::architect::Page;
use project_proto::{
    DocId, ProjectCreate, ProjectRepo, TaskCreate, TaskRepo, TaskUpdate, UpdateBytes,
    WorkspaceSyncClient,
};

fn workspace_doc() -> DocId {
    DocId::new("workspace")
}
use task_db::{default_database_url, open_and_migrate};
use task_server::{AppState, router};
use uuid::Uuid;
use vox::Rx;

// Sized so the full suite runs in <60s on a laptop. Each peer's
// burst fans to (PEERS-1) subscribers, so total imports ≈
// PEERS² × EDITS_PER_PEER. Crank these if you're hunting a
// specific race; revert when you commit.
const PEERS: usize = 15;
const EDITS_PER_PEER: usize = 150;
const SEED_TASKS: usize = 30;
/// How long to wait, after the last edit, for the stream to settle
/// before declaring quiescence and asserting convergence.
const QUIESCE_MS: u64 = 3000;
/// Hard cap on total drain time per peer during quiescence wait.
const DRAIN_TIMEOUT: Duration = Duration::from_secs(120);

async fn boot_server() -> eyre::Result<(String, Vec<Uuid>)> {
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let state = AppState::new(persistence).await?;

    let project = state
        .project_repo
        .create(ProjectCreate {
            name: "Stress".into(),
        })
        .await
        .map_err(|e| eyre::eyre!("seed project: {e}"))?;

    let mut task_ids = Vec::with_capacity(SEED_TASKS);
    for i in 0..SEED_TASKS {
        let t = state
            .task_repo
            .create(TaskCreate {
                project_id: project.id,
                title: format!("Task {i}"),
                done: false,
            })
            .await
            .map_err(|e| eyre::eyre!("seed task: {e}"))?;
        task_ids.push(t.id);
    }

    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    let url = format!("ws://127.0.0.1:{port}/vox");
    Ok((url, task_ids))
}

/// One peer with both directions wired.
struct Peer {
    id: usize,
    doc: Arc<CrdtDoc>,
    apply: WorkspaceSyncClient,
    /// Counts every chunk we've imported. Used to detect quiescence.
    imports: Arc<Mutex<u64>>,
    /// Background task that pumps the subscribe stream + uploads.
    /// Aborted on test teardown.
    _sub_handle: tokio::task::JoinHandle<()>,
    _upload_handle: tokio::task::JoinHandle<()>,
}

async fn open_peer(id: usize, url: &str) -> eyre::Result<Peer> {
    let doc = Arc::new(CrdtDoc::ephemeral());

    // Wire local commits → mpsc → uploader.
    let (upload_tx, mut upload_rx) = futures::channel::mpsc::unbounded::<Vec<u8>>();
    let upload_tx_for_cb = upload_tx.clone();
    let local_sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        let _ = upload_tx_for_cb.unbounded_send(bytes.to_vec());
        true
    }));
    std::mem::forget(local_sub);
    drop(upload_tx);

    let apply: WorkspaceSyncClient = vox::connect(url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply connect: {e:?}"))?;
    let apply_for_uploader = apply.clone();
    let upload_handle = tokio::spawn(async move {
        use futures::StreamExt;
        while let Some(bytes) = upload_rx.next().await {
            if let Err(e) = apply_for_uploader
                .apply_update(workspace_doc(), UpdateBytes(bytes))
                .await
            {
                tracing::warn!(peer = id, ?e, "apply_update failed");
            }
        }
    });

    let sub_client: WorkspaceSyncClient = vox::connect(url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("sub connect: {e:?}"))?;
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    tokio::spawn(async move {
        let _ = sub_client.subscribe(workspace_doc(), tx).await;
    });

    let imports = Arc::new(Mutex::new(0u64));
    let doc_for_loop = doc.clone();
    let imports_for_loop = imports.clone();
    let sub_handle = tokio::spawn(async move {
        while let Ok(Some(msg)) = rx.recv().await {
            if let Err(e) = doc_for_loop.apply_remote(&msg.get().0) {
                tracing::warn!(peer = id, ?e, "apply_remote failed");
                continue;
            }
            *imports_for_loop.lock().unwrap() += 1;
        }
    });

    Ok(Peer {
        id,
        doc,
        apply,
        imports,
        _sub_handle: sub_handle,
        _upload_handle: upload_handle,
    })
}

async fn import_count(peer: &Peer) -> u64 {
    *peer.imports.lock().unwrap()
}

async fn task_done_map(doc: &CrdtDoc) -> BTreeMap<Uuid, bool> {
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
        .expect("list");
    page.items.into_iter().map(|t| (t.id, t.done)).collect()
}

/// Wait until none of the peers' import counters changed for
/// `QUIESCE_MS`. Returns after at most `DRAIN_TIMEOUT`.
async fn wait_for_quiescence(peers: &[Peer]) {
    let stop = tokio::time::Instant::now() + DRAIN_TIMEOUT;
    let mut last_counts: Vec<u64> = Vec::with_capacity(peers.len());
    for p in peers.iter() {
        last_counts.push(import_count(p).await);
    }
    let mut quiet_since = tokio::time::Instant::now();
    loop {
        tokio::time::sleep(Duration::from_millis(100)).await;
        let mut changed = false;
        for (i, p) in peers.iter().enumerate() {
            let c = import_count(p).await;
            if c != last_counts[i] {
                last_counts[i] = c;
                changed = true;
            }
        }
        if changed {
            quiet_since = tokio::time::Instant::now();
        }
        if quiet_since.elapsed() >= Duration::from_millis(QUIESCE_MS) {
            return;
        }
        if tokio::time::Instant::now() >= stop {
            tracing::warn!("wait_for_quiescence: hard timeout");
            return;
        }
    }
}

/// Seed of a tiny xorshift so the test is deterministic per peer
/// id. Avoids pulling in `rand` for one PRNG.
fn xorshift(seed: u64) -> impl FnMut() -> u64 {
    let mut s = seed | 1; // seed must be non-zero
    move || {
        s ^= s << 13;
        s ^= s >> 7;
        s ^= s << 17;
        s
    }
}

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn many_peers_burst_edits_converge() -> eyre::Result<()> {
    let (url, task_ids) = boot_server().await?;

    // Open all peers concurrently — exercises the snapshot-vs-
    // broadcast race more thoroughly than serializing them.
    let mut peers = Vec::with_capacity(PEERS);
    let opens = (0..PEERS).map(|i| open_peer(i, &url));
    let opened: Vec<eyre::Result<Peer>> = futures::future::join_all(opens).await;
    for r in opened {
        peers.push(r?);
    }

    // Wait for every peer's snapshot to land — at minimum 1 import
    // per peer.
    let stop = tokio::time::Instant::now() + Duration::from_secs(5);
    loop {
        let mut all_seen = true;
        for p in peers.iter() {
            if import_count(p).await == 0 {
                all_seen = false;
                break;
            }
        }
        if all_seen {
            break;
        }
        if tokio::time::Instant::now() >= stop {
            return Err(eyre::eyre!("snapshots didn't arrive within 5s"));
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    // Burst: each peer toggles random tasks in parallel.
    let edit_tasks: Vec<_> = peers
        .iter()
        .map(|p| {
            let doc = p.doc.clone();
            let id = p.id;
            let task_ids = task_ids.clone();
            tokio::spawn(async move {
                let mut prng = xorshift((id as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15));
                for k in 0..EDITS_PER_PEER {
                    let pick = (prng() as usize) % task_ids.len();
                    let task_id = task_ids[pick];
                    let new_done = ((prng() as u64) & 1) == 1;
                    let task_repo = TaskRepoLoro::new(&doc);
                    if let Err(e) = task_repo
                        .update(
                            task_id,
                            TaskUpdate {
                                done: Some(new_done),
                                ..Default::default()
                            },
                        )
                        .await
                    {
                        tracing::warn!(peer = id, edit = k, ?e, "local update failed");
                    }
                    // No jitter — fire as fast as the runtime
                    // schedules us. The stress test wants real
                    // burst behavior, not polite pacing.
                    tokio::task::yield_now().await;
                }
            })
        })
        .collect();
    for t in edit_tasks {
        t.await?;
    }

    wait_for_quiescence(&peers).await;

    // Convergence assertion. Compute the canonical map from
    // peer 0 and check every other peer + the server agree.
    let mut maps: Vec<BTreeMap<Uuid, bool>> = Vec::with_capacity(peers.len());
    for p in peers.iter() {
        maps.push(task_done_map(&p.doc).await);
    }

    let mut ok = true;
    for i in 1..maps.len() {
        if maps[i] != maps[0] {
            ok = false;
            // Find the diff for the panic message.
            let diff: Vec<_> = task_ids
                .iter()
                .filter(|id| maps[0].get(id) != maps[i].get(id))
                .map(|id| {
                    format!(
                        "  {}: peer0={:?} peer{}={:?}",
                        id,
                        maps[0].get(id),
                        i,
                        maps[i].get(id)
                    )
                })
                .collect();
            eprintln!("\nDIVERGENCE peer 0 vs peer {i}:\n{}", diff.join("\n"));
        }
    }

    // Also cross-check via a fresh subscribe — round-trips through
    // the server's snapshot, so it captures the server's authoritative
    // state and compares to peer 0.
    let probe = open_peer(999, &url).await?;
    // Wait for probe's snapshot.
    let stop = tokio::time::Instant::now() + Duration::from_secs(5);
    while import_count(&probe).await == 0 && tokio::time::Instant::now() < stop {
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    let server_view = task_done_map(&probe.doc).await;
    if server_view != maps[0] {
        ok = false;
        let diff: Vec<_> = task_ids
            .iter()
            .filter(|id| maps[0].get(id) != server_view.get(id))
            .map(|id| {
                format!(
                    "  {}: peer0={:?} server={:?}",
                    id,
                    maps[0].get(id),
                    server_view.get(id)
                )
            })
            .collect();
        eprintln!("\nDIVERGENCE peer 0 vs server probe:\n{}", diff.join("\n"));
    }

    // Keep `_apply` clones alive until end of test so their
    // background uploaders aren't dropped mid-flight.
    drop(peers);
    drop(probe);

    assert!(
        ok,
        "peers and/or server diverged after burst edits + quiescence"
    );
    Ok(())
}

/// Like `many_peers_burst_edits_converge` but each peer's
/// subscribe stream is killed + reopened halfway through the
/// burst. Exercises the snapshot-vs-broadcast race I fixed and
/// the "lagged subscriber → resnapshot" recovery path.
#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn churn_peers_resubscribe_mid_burst() -> eyre::Result<()> {
    let (url, task_ids) = boot_server().await?;

    let url_arc: Arc<String> = Arc::new(url);

    // Each peer is wrapped in a Mutex so the editor task and the
    // churner task can both touch its `_sub_handle` slot.
    struct ChurnPeer {
        peer: Peer,
    }
    let mut peers: Vec<Arc<Mutex<ChurnPeer>>> = Vec::with_capacity(PEERS);
    let opens = (0..PEERS).map(|i| {
        let url = url_arc.clone();
        async move { open_peer(i, &url).await }
    });
    for r in futures::future::join_all(opens).await {
        peers.push(Arc::new(Mutex::new(ChurnPeer { peer: r? })));
    }

    // Wait for snapshots.
    let stop = tokio::time::Instant::now() + Duration::from_secs(5);
    loop {
        let mut all_seen = true;
        for p in peers.iter() {
            if import_count(&p.lock().unwrap().peer).await == 0 {
                all_seen = false;
                break;
            }
        }
        if all_seen {
            break;
        }
        if tokio::time::Instant::now() >= stop {
            return Err(eyre::eyre!("snapshots didn't arrive within 5s"));
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    // Editor tasks fire EDITS_PER_PEER toggles, calling
    // `task_repo.update` directly via the doc Arc (not through
    // the peer mutex — the doc handle is independently Cloneable).
    let edit_tasks: Vec<_> = peers
        .iter()
        .enumerate()
        .map(|(id, p)| {
            let doc = p.lock().unwrap().peer.doc.clone();
            let task_ids = task_ids.clone();
            tokio::spawn(async move {
                let mut prng = xorshift((id as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15));
                for _ in 0..EDITS_PER_PEER {
                    let task_id = task_ids[(prng() as usize) % task_ids.len()];
                    let new_done = ((prng() as u64) & 1) == 1;
                    let task_repo = TaskRepoLoro::new(&doc);
                    let _ = task_repo
                        .update(
                            task_id,
                            TaskUpdate {
                                done: Some(new_done),
                                ..Default::default()
                            },
                        )
                        .await;
                    tokio::task::yield_now().await;
                }
            })
        })
        .collect();

    // Churner tasks: at random intervals, drop and re-open each
    // peer's subscribe stream. Runs for ~half the expected burst
    // duration.
    let churn_tasks: Vec<_> = peers
        .iter()
        .enumerate()
        .map(|(id, p)| {
            let p = p.clone();
            let url = url_arc.clone();
            tokio::spawn(async move {
                for round in 0..3 {
                    tokio::time::sleep(Duration::from_millis(50 + (id as u64) * 7)).await;
                    // Replace the subscribe handle. The old one's
                    // JoinHandle goes out of scope and the WS
                    // closes when its driver task is dropped.
                    let new_doc = p.lock().unwrap().peer.doc.clone();
                    let new_imports = p.lock().unwrap().peer.imports.clone();
                    let sub_client: WorkspaceSyncClient =
                        match vox::connect(url.as_str()).establish().await {
                            Ok(c) => c,
                            Err(e) => {
                                tracing::warn!(peer = id, round, ?e, "churn reconnect failed");
                                continue;
                            }
                        };
                    let (tx, mut rx) = vox::channel::<UpdateBytes>();
                    tokio::spawn(async move {
                        let _ = sub_client.subscribe(workspace_doc(), tx).await;
                    });
                    let new_sub = tokio::spawn(async move {
                        while let Ok(Some(msg)) = rx.recv().await {
                            if new_doc.apply_remote(&msg.get().0).is_ok() {
                                *new_imports.lock().unwrap() += 1;
                            }
                        }
                    });
                    {
                        let mut g = p.lock().unwrap();
                        let old = std::mem::replace(&mut g.peer._sub_handle, new_sub);
                        old.abort();
                    }
                }
            })
        })
        .collect();

    for t in edit_tasks {
        t.await?;
    }
    for t in churn_tasks {
        t.await?;
    }

    // Build a slice of references for wait_for_quiescence. The
    // helper only needs &Peer; reach through the Mutex.
    // Use the same pattern as wait_for_quiescence inline:
    {
        let stop = tokio::time::Instant::now() + DRAIN_TIMEOUT;
        let mut last_counts: Vec<u64> = Vec::with_capacity(peers.len());
        for p in peers.iter() {
            last_counts.push(import_count(&p.lock().unwrap().peer).await);
        }
        let mut quiet_since = tokio::time::Instant::now();
        loop {
            tokio::time::sleep(Duration::from_millis(100)).await;
            let mut changed = false;
            for (i, p) in peers.iter().enumerate() {
                let c = import_count(&p.lock().unwrap().peer).await;
                if c != last_counts[i] {
                    last_counts[i] = c;
                    changed = true;
                }
            }
            if changed {
                quiet_since = tokio::time::Instant::now();
            }
            if quiet_since.elapsed() >= Duration::from_millis(QUIESCE_MS) {
                break;
            }
            if tokio::time::Instant::now() >= stop {
                tracing::warn!("churn quiescence: hard timeout");
                break;
            }
        }
    }

    // Convergence check.
    let mut maps: Vec<BTreeMap<Uuid, bool>> = Vec::with_capacity(peers.len());
    for p in peers.iter() {
        maps.push(task_done_map(&p.lock().unwrap().peer.doc).await);
    }
    let mut ok = true;
    for i in 1..maps.len() {
        if maps[i] != maps[0] {
            ok = false;
            let diff: Vec<_> = task_ids
                .iter()
                .filter(|id| maps[0].get(id) != maps[i].get(id))
                .map(|id| {
                    format!(
                        "  {}: peer0={:?} peer{}={:?}",
                        id,
                        maps[0].get(id),
                        i,
                        maps[i].get(id)
                    )
                })
                .collect();
            eprintln!(
                "\n[churn] DIVERGENCE peer 0 vs peer {i}:\n{}",
                diff.join("\n")
            );
        }
    }

    let probe = open_peer(999, url_arc.as_str()).await?;
    let stop = tokio::time::Instant::now() + Duration::from_secs(5);
    while import_count(&probe).await == 0 && tokio::time::Instant::now() < stop {
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    let server_view = task_done_map(&probe.doc).await;
    if server_view != maps[0] {
        ok = false;
        let diff: Vec<_> = task_ids
            .iter()
            .filter(|id| maps[0].get(id) != server_view.get(id))
            .map(|id| {
                format!(
                    "  {}: peer0={:?} server={:?}",
                    id,
                    maps[0].get(id),
                    server_view.get(id)
                )
            })
            .collect();
        eprintln!(
            "\n[churn] DIVERGENCE peer 0 vs server:\n{}",
            diff.join("\n")
        );
    }

    drop(peers);
    drop(probe);

    assert!(ok, "[churn] divergence after re-subscribe under burst");
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────
// Hotspot: all peers hammer the same handful of tasks.
//
// Pathological case for CRDT merge: every peer's edit lands on the
// same scalar field of the same row. Loro's per-peer LWW ordering
// has to deterministically pick a winner and all peers have to
// agree on which one. Asserts convergence (NOT a specific value —
// the winner is observable but not predictable from the test's
// POV).
// ─────────────────────────────────────────────────────────────────────

const HOTSPOT_PEERS: usize = 10;
const HOTSPOT_EDITS_PER_PEER: usize = 100;
const HOTSPOT_TASK_COUNT: usize = 3;

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn hotspot_same_task_concurrent_edits_converge() -> eyre::Result<()> {
    let (url, mut task_ids) = boot_server().await?;
    task_ids.truncate(HOTSPOT_TASK_COUNT);
    assert_eq!(task_ids.len(), HOTSPOT_TASK_COUNT);

    let mut peers = Vec::with_capacity(HOTSPOT_PEERS);
    let opens = (0..HOTSPOT_PEERS).map(|i| open_peer(i, &url));
    for r in futures::future::join_all(opens).await {
        peers.push(r?);
    }

    // Wait for snapshots.
    let stop = tokio::time::Instant::now() + Duration::from_secs(10);
    loop {
        let mut all_seen = true;
        for p in peers.iter() {
            if import_count(p).await == 0 {
                all_seen = false;
                break;
            }
        }
        if all_seen {
            break;
        }
        if tokio::time::Instant::now() >= stop {
            return Err(eyre::eyre!("hotspot: snapshots didn't arrive in 10s"));
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    // Every peer cycles every task's `done` HOTSPOT_EDITS_PER_PEER
    // times. Worst-case same-cell contention.
    let edit_tasks: Vec<_> = peers
        .iter()
        .map(|p| {
            let doc = p.doc.clone();
            let id = p.id;
            let task_ids = task_ids.clone();
            tokio::spawn(async move {
                let mut prng = xorshift((id as u64).wrapping_mul(0x9E37_79B9_7F4A_7C15));
                for _ in 0..HOTSPOT_EDITS_PER_PEER {
                    let task_id = task_ids[(prng() as usize) % task_ids.len()];
                    let new_done = ((prng() as u64) & 1) == 1;
                    let task_repo = TaskRepoLoro::new(&doc);
                    let _ = task_repo
                        .update(
                            task_id,
                            TaskUpdate {
                                done: Some(new_done),
                                ..Default::default()
                            },
                        )
                        .await;
                    tokio::task::yield_now().await;
                }
            })
        })
        .collect();
    for t in edit_tasks {
        t.await?;
    }

    wait_for_quiescence(&peers).await;

    // Convergence check (NOT value check — Loro picks a winner).
    let maps: Vec<BTreeMap<Uuid, bool>> = {
        let mut v = Vec::with_capacity(peers.len());
        for p in peers.iter() {
            v.push(task_done_map(&p.doc).await);
        }
        v
    };
    let mut ok = true;
    for i in 1..maps.len() {
        if maps[i] != maps[0] {
            ok = false;
            let diff: Vec<_> = task_ids
                .iter()
                .filter(|id| maps[0].get(id) != maps[i].get(id))
                .map(|id| {
                    format!(
                        "  {}: peer0={:?} peer{}={:?}",
                        id,
                        maps[0].get(id),
                        i,
                        maps[i].get(id)
                    )
                })
                .collect();
            eprintln!(
                "\n[hotspot] DIVERGENCE peer 0 vs peer {i}:\n{}",
                diff.join("\n")
            );
        }
    }

    let probe = open_peer(999, &url).await?;
    let stop = tokio::time::Instant::now() + Duration::from_secs(10);
    while import_count(&probe).await == 0 && tokio::time::Instant::now() < stop {
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    let server_view = task_done_map(&probe.doc).await;
    if server_view != maps[0] {
        ok = false;
        eprintln!("\n[hotspot] server probe differs from peer 0");
    }

    drop(peers);
    drop(probe);
    assert!(
        ok,
        "[hotspot] CRDT merge failed to converge under same-cell burst"
    );
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────
// Create storm: many peers concurrently create many new tasks.
//
// Exercises the path where peers introduce ENTIRELY NEW keys into
// the workspace doc concurrently — different from edit conflicts.
// Every peer's local commit produces fresh LoroMap entries; the
// merge has to fold them all in without dropping any.
// ─────────────────────────────────────────────────────────────────────

const STORM_PEERS: usize = 8;
const STORM_TASKS_PER_PEER: usize = 25;

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn create_storm_no_losses() -> eyre::Result<()> {
    // Boot with one project; ignore the seeded tasks for this test.
    let (url, _) = boot_server().await?;
    let project_id = {
        // Pull project_id from a probe peer's snapshot — the seed
        // already wrote one.
        let probe = open_peer(0, &url).await?;
        let stop = tokio::time::Instant::now() + Duration::from_secs(10);
        while import_count(&probe).await == 0 && tokio::time::Instant::now() < stop {
            tokio::time::sleep(Duration::from_millis(50)).await;
        }
        let task_repo = TaskRepoLoro::new(&probe.doc);
        let page = task_repo
            .list(
                Page {
                    index: 0,
                    size: 1000,
                },
                None,
                None,
            )
            .await?;
        let pid = page
            .items
            .first()
            .map(|t| t.project_id)
            .ok_or_else(|| eyre::eyre!("seeded tasks unexpectedly missing"))?;
        drop(probe);
        pid
    };

    // Open a fresh set of peers (probe peer is gone).
    let mut peers = Vec::with_capacity(STORM_PEERS);
    let opens = (1..=STORM_PEERS).map(|i| open_peer(i, &url));
    for r in futures::future::join_all(opens).await {
        peers.push(r?);
    }

    // Wait for snapshots.
    let stop = tokio::time::Instant::now() + Duration::from_secs(10);
    loop {
        let mut all_seen = true;
        for p in peers.iter() {
            if import_count(p).await == 0 {
                all_seen = false;
                break;
            }
        }
        if all_seen {
            break;
        }
        if tokio::time::Instant::now() >= stop {
            return Err(eyre::eyre!("storm: snapshots didn't arrive in 10s"));
        }
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    let seeded_count = task_done_map(&peers[0].doc).await.len();

    // Each peer creates STORM_TASKS_PER_PEER tasks in parallel.
    let create_tasks: Vec<_> = peers
        .iter()
        .enumerate()
        .map(|(slot, p)| {
            let doc = p.doc.clone();
            tokio::spawn(async move {
                let task_repo = TaskRepoLoro::new(&doc);
                for k in 0..STORM_TASKS_PER_PEER {
                    let _ = task_repo
                        .create(TaskCreate {
                            project_id,
                            title: format!("peer{slot}/task{k}"),
                            done: false,
                        })
                        .await;
                    tokio::task::yield_now().await;
                }
            })
        })
        .collect();
    for t in create_tasks {
        t.await?;
    }

    wait_for_quiescence(&peers).await;

    let expected_total = seeded_count + STORM_PEERS * STORM_TASKS_PER_PEER;

    // Convergence + count check.
    let mut maps: Vec<BTreeMap<Uuid, bool>> = Vec::with_capacity(peers.len());
    for p in peers.iter() {
        maps.push(task_done_map(&p.doc).await);
    }
    let mut ok = true;
    for (i, m) in maps.iter().enumerate() {
        if m.len() != expected_total {
            ok = false;
            eprintln!(
                "[storm] peer {i} has {} tasks; expected {expected_total}",
                m.len()
            );
        }
    }
    for i in 1..maps.len() {
        if maps[i].keys().collect::<Vec<_>>() != maps[0].keys().collect::<Vec<_>>() {
            ok = false;
            eprintln!("[storm] peer 0 vs peer {i}: key sets differ");
        }
    }

    let probe = open_peer(999, &url).await?;
    let stop = tokio::time::Instant::now() + Duration::from_secs(10);
    while import_count(&probe).await == 0 && tokio::time::Instant::now() < stop {
        tokio::time::sleep(Duration::from_millis(50)).await;
    }
    let server_view = task_done_map(&probe.doc).await;
    if server_view.len() != expected_total {
        ok = false;
        eprintln!(
            "[storm] server probe has {} tasks; expected {expected_total}",
            server_view.len()
        );
    }

    drop(peers);
    drop(probe);
    assert!(
        ok,
        "[storm] task count or key set diverged after create burst"
    );
    Ok(())
}

// ─────────────────────────────────────────────────────────────────────
// Long offline + bulk replay.
//
// One peer goes offline, makes hundreds of local edits, comes
// back, and dumps them all at once. Server has to accept the
// flood, broadcast it to other peers, and have everyone converge.
// Stresses the apply_update path under burst from a single peer
// (no inter-peer interleaving while offline).
// ─────────────────────────────────────────────────────────────────────

const OFFLINE_EDITS: usize = 200;

#[tokio::test(flavor = "multi_thread", worker_threads = 4)]
async fn long_offline_bulk_replay() -> eyre::Result<()> {
    let (url, task_ids) = boot_server().await?;

    // One "online observer" peer that stays connected the whole
    // time, and one "offline editor" peer that disconnects after
    // its snapshot.
    let observer = open_peer(0, &url).await?;
    let editor = open_peer(1, &url).await?;
    let stop = tokio::time::Instant::now() + Duration::from_secs(5);
    while (import_count(&observer).await == 0 || import_count(&editor).await == 0)
        && tokio::time::Instant::now() < stop
    {
        tokio::time::sleep(Duration::from_millis(50)).await;
    }

    // Editor goes offline (kill its subscribe stream — the
    // upload mpsc still drains because its apply_client lives in
    // the peer struct; for a fully-offline simulation we drop
    // the whole peer except its doc, then rebuild later).
    drop(editor);

    // Recreate an offline doc with the same captured-bytes
    // pattern. (Simpler than scoping inside `Peer` to keep
    // upload independent of subscribe.)
    let offline_doc = Arc::new(CrdtDoc::ephemeral());
    let pending: Arc<Mutex<Vec<Vec<u8>>>> = Arc::new(Mutex::new(Vec::new()));
    let pcb = pending.clone();
    let sub = offline_doc
        .loro()
        .subscribe_local_update(Box::new(move |bytes| {
            pcb.lock().unwrap().push(bytes.to_vec());
            true
        }));
    std::mem::forget(sub);

    // Snapshot the observer's current state into the offline doc
    // so it starts from the same place. (Realistic — a real
    // offline client would have hydrated from a snapshot before
    // disconnecting.)
    let bootstrap_bytes = observer
        .doc
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("offline bootstrap export: {e}"))?;
    offline_doc
        .apply_remote(&bootstrap_bytes)
        .map_err(|e| eyre::eyre!("offline bootstrap import: {e}"))?;
    // Clear the bootstrap bytes from `pending` — those aren't
    // local commits.
    pending.lock().unwrap().clear();

    // Now: while offline, fire OFFLINE_EDITS commits.
    {
        let task_repo = TaskRepoLoro::new(&offline_doc);
        let mut prng = xorshift(0xCAFEBABE);
        for _ in 0..OFFLINE_EDITS {
            let task_id = task_ids[(prng() as usize) % task_ids.len()];
            let new_done = ((prng() as u64) & 1) == 1;
            task_repo
                .update(
                    task_id,
                    TaskUpdate {
                        done: Some(new_done),
                        ..Default::default()
                    },
                )
                .await
                .map_err(|e| eyre::eyre!("offline edit: {e}"))?;
        }
    }
    let pending_count = pending.lock().unwrap().len();
    eprintln!("[offline] captured {pending_count} update chunks while offline");
    assert!(
        pending_count > 0,
        "expected at least one captured local update"
    );

    // Reconnect: open an apply client, dump all pending bytes.
    let apply: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("reconnect apply: {e:?}"))?;
    for bytes in std::mem::take(&mut *pending.lock().unwrap()) {
        apply
            .apply_update(workspace_doc(), UpdateBytes(bytes))
            .await
            .map_err(|e| eyre::eyre!("bulk replay apply: {e:?}"))?;
    }

    // Observer should now see the offline editor's final state.
    // Wait for the offline doc's done-map to match the observer's.
    let target_map = task_done_map(&offline_doc).await;
    let stop = tokio::time::Instant::now() + Duration::from_secs(15);
    loop {
        let obs_map = task_done_map(&observer.doc).await;
        if obs_map == target_map {
            break;
        }
        if tokio::time::Instant::now() >= stop {
            let diff: Vec<_> = task_ids
                .iter()
                .filter(|id| obs_map.get(id) != target_map.get(id))
                .map(|id| {
                    format!(
                        "  {}: observer={:?} offline={:?}",
                        id,
                        obs_map.get(id),
                        target_map.get(id)
                    )
                })
                .collect();
            return Err(eyre::eyre!(
                "[offline] observer didn't catch up to offline editor's state:\n{}",
                diff.join("\n")
            ));
        }
        tokio::time::sleep(Duration::from_millis(100)).await;
    }

    drop(observer);
    Ok(())
}
