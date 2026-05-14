//! Multi-doc isolation test.
//!
//! Phase 1 (doc-id transport) deliverable: verify that an edit
//! to `DocId("project/X")` doesn't appear in the subscription
//! stream of `DocId("project/Y")`. Two peers, two distinct
//! doc-ids, separate state.
//!
//! The earlier `sync.rs` + `stress.rs` tests both target the
//! legacy `"workspace"` doc id. This test specifically exercises
//! the per-doc registry on the server.

use std::sync::{Arc, Mutex};
use std::time::Duration;

use crdt::CrdtDoc;
use crdt_seaorm::SeaOrmPersistence;
use project_proto::{DocId, UpdateBytes, WorkspaceSyncClient};
use task_db::{default_database_url, open_and_migrate};
use task_server::{AppState, router};
use uuid::Uuid;
use vox::Rx;

async fn boot_server() -> eyre::Result<String> {
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let state = AppState::new(persistence).await?;
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    Ok(format!("ws://127.0.0.1:{port}/vox"))
}

/// Open a fresh `CrdtDoc` + capture local commits into `pending`.
fn fresh_doc() -> (Arc<CrdtDoc>, Arc<Mutex<Vec<Vec<u8>>>>) {
    let doc = Arc::new(CrdtDoc::ephemeral());
    let pending: Arc<Mutex<Vec<Vec<u8>>>> = Arc::new(Mutex::new(Vec::new()));
    let pcb = pending.clone();
    let sub = doc.loro().subscribe_local_update(Box::new(move |bytes| {
        pcb.lock().unwrap().push(bytes.to_vec());
        true
    }));
    std::mem::forget(sub);
    (doc, pending)
}

async fn open_subscribe(
    url: &str,
    doc_id: DocId,
) -> eyre::Result<(tokio::task::JoinHandle<()>, Rx<UpdateBytes>)> {
    let client: WorkspaceSyncClient = vox::connect(url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
    let (tx, rx) = vox::channel::<UpdateBytes>();
    let handle = tokio::spawn(async move {
        let _ = client.subscribe(doc_id, tx).await;
    });
    Ok((handle, rx))
}

/// Drain at most `max` chunks from `rx` into `doc`, with a per-recv
/// timeout. Returns the number of chunks imported.
async fn drain_into(
    doc: &CrdtDoc,
    rx: &mut Rx<UpdateBytes>,
    max: usize,
    per_recv_timeout: Duration,
) -> usize {
    let mut count = 0;
    while count < max {
        match tokio::time::timeout(per_recv_timeout, rx.recv()).await {
            Ok(Ok(Some(msg))) => {
                doc.apply_remote(&msg.get().0).expect("apply_remote");
                count += 1;
            }
            _ => break,
        }
    }
    count
}

/// Count entries in a top-level LoroMap by name.
fn map_len(doc: &CrdtDoc, root: &str) -> usize {
    doc.loro().get_map(root).len()
}

#[tokio::test(flavor = "multi_thread")]
async fn distinct_docs_dont_cross_streams() -> eyre::Result<()> {
    let url = boot_server().await?;

    let doc_x = DocId::project(Uuid::parse_str("11111111-1111-1111-1111-111111111111").unwrap());
    let doc_y = DocId::project(Uuid::parse_str("22222222-2222-2222-2222-222222222222").unwrap());

    // Peer A subscribes to doc_x. Peer B subscribes to doc_y.
    let (peer_a_doc, _peer_a_pending) = fresh_doc();
    let (peer_b_doc, _peer_b_pending) = fresh_doc();
    let (sub_a, mut rx_a) = open_subscribe(&url, doc_x.clone()).await?;
    let (sub_b, mut rx_b) = open_subscribe(&url, doc_y.clone()).await?;

    // Drain the empty-doc snapshot each subscribe stream sends as its first frame.
    let _ = drain_into(&peer_a_doc, &mut rx_a, 1, Duration::from_secs(2)).await;
    let _ = drain_into(&peer_b_doc, &mut rx_b, 1, Duration::from_secs(2)).await;

    // Writer for doc_x — creates a unique key in the doc.
    let writer_doc_x = Arc::new(CrdtDoc::ephemeral());
    {
        let m = writer_doc_x.loro().get_map("test_marker");
        m.insert("from_x", "x-value").expect("insert x marker");
        writer_doc_x.loro().commit();
    }
    let writer_bytes_x = writer_doc_x
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("export x: {e}"))?;
    let apply_x: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply_x connect: {e:?}"))?;
    apply_x
        .apply_update(doc_x.clone(), UpdateBytes(writer_bytes_x))
        .await
        .map_err(|e| eyre::eyre!("apply doc_x: {e:?}"))?;

    // Writer for doc_y.
    let writer_doc_y = Arc::new(CrdtDoc::ephemeral());
    {
        let m = writer_doc_y.loro().get_map("test_marker");
        m.insert("from_y", "y-value").expect("insert y marker");
        writer_doc_y.loro().commit();
    }
    let writer_bytes_y = writer_doc_y
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("export y: {e}"))?;
    let apply_y: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply_y connect: {e:?}"))?;
    apply_y
        .apply_update(doc_y.clone(), UpdateBytes(writer_bytes_y))
        .await
        .map_err(|e| eyre::eyre!("apply doc_y: {e:?}"))?;

    // Pump each peer's subscribe stream. They should only receive
    // the bytes for THEIR doc.
    let n_a = drain_into(&peer_a_doc, &mut rx_a, 10, Duration::from_millis(500)).await;
    let n_b = drain_into(&peer_b_doc, &mut rx_b, 10, Duration::from_millis(500)).await;
    eprintln!("peer A imported {n_a} chunks; peer B imported {n_b} chunks");

    // Peer A's doc must contain the x marker, NOT the y marker.
    let a_map = peer_a_doc.loro().get_map("test_marker");
    assert!(
        a_map.get("from_x").is_some(),
        "peer A's doc should contain from_x marker"
    );
    assert!(
        a_map.get("from_y").is_none(),
        "peer A's doc must NOT contain from_y marker — doc isolation broken!"
    );

    // Peer B's doc must contain the y marker, NOT the x marker.
    let b_map = peer_b_doc.loro().get_map("test_marker");
    assert!(
        b_map.get("from_y").is_some(),
        "peer B's doc should contain from_y marker"
    );
    assert!(
        b_map.get("from_x").is_none(),
        "peer B's doc must NOT contain from_x marker — doc isolation broken!"
    );

    // Each peer's doc should have one marker entry, not two.
    assert_eq!(
        map_len(&peer_a_doc, "test_marker"),
        1,
        "peer A's test_marker map should have one entry"
    );
    assert_eq!(
        map_len(&peer_b_doc, "test_marker"),
        1,
        "peer B's test_marker map should have one entry"
    );

    sub_a.abort();
    sub_b.abort();
    Ok(())
}

/// Verify that a re-open of the same DocId returns a doc with prior
/// state (the server's registry caches the open doc).
#[tokio::test(flavor = "multi_thread")]
async fn same_doc_id_reopens_with_state() -> eyre::Result<()> {
    let url = boot_server().await?;
    let doc_id = DocId::project(Uuid::parse_str("33333333-3333-3333-3333-333333333333").unwrap());

    // First subscriber writes a marker.
    let writer = Arc::new(CrdtDoc::ephemeral());
    {
        let m = writer.loro().get_map("memo");
        m.insert("hello", "world").expect("insert memo");
        writer.loro().commit();
    }
    let writer_bytes = writer
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("export: {e}"))?;
    let apply: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply connect: {e:?}"))?;
    apply
        .apply_update(doc_id.clone(), UpdateBytes(writer_bytes))
        .await
        .map_err(|e| eyre::eyre!("apply: {e:?}"))?;

    // Drop the apply client. New subscriber connects fresh —
    // server should reuse the in-memory doc from the registry.
    drop(apply);
    tokio::time::sleep(Duration::from_millis(200)).await;

    let (probe_doc, _) = fresh_doc();
    let (sub, mut rx) = open_subscribe(&url, doc_id.clone()).await?;
    let n = drain_into(&probe_doc, &mut rx, 5, Duration::from_secs(2)).await;
    assert!(
        n >= 1,
        "probe should have received at least the snapshot frame"
    );
    let memo = probe_doc.loro().get_map("memo");
    assert!(
        memo.get("hello").is_some(),
        "re-subscribed probe should see the prior writer's marker"
    );

    sub.abort();
    Ok(())
}
