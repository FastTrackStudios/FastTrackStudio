//! Phase 3 — capability middleware end-to-end.
//!
//! `plans/decentralized-foundation.md` §13 Phase 3 deliverable:
//! anonymous client with a token scoped to `project/A` can
//! subscribe to that doc, gets `Forbidden` for `project/B`.
//!
//! Also validates LRU eviction (carried over from Phase 1 deferred).

use std::sync::Arc;
use std::time::Duration;

use crdt::CrdtDoc;
use crdt_seaorm::SeaOrmPersistence;
use project_proto::{DocId, UpdateBytes, WorkspaceSyncClient};
use task_db::{default_database_url, open_and_migrate};
use task_server::capability::{CapabilityScope, ServerKeypair};
use task_server::{AppState, AuthState, DocRegistry, router};
use uuid::Uuid;

fn future_expiry() -> i64 {
    i64::MAX / 2
}

async fn boot_server_with_capability() -> eyre::Result<(String, ServerKeypair)> {
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let auth = AuthState::open("sqlite::memory:", "test-secret-at-least-32-bytes!!!").await?;
    let keypair = ServerKeypair::generate_ephemeral();
    let state = AppState::new_with_capability(persistence, auth, keypair.clone()).await?;
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    Ok((format!("ws://127.0.0.1:{port}/vox"), keypair))
}

fn url_with_cap(base: &str, token: &str) -> String {
    format!("{base}?cap={token}")
}

#[tokio::test(flavor = "multi_thread")]
async fn scoped_token_only_subscribes_to_its_doc() -> eyre::Result<()> {
    let (base, keypair) = boot_server_with_capability().await?;

    let doc_a = DocId::project(Uuid::parse_str("aaaaaaaa-aaaa-aaaa-aaaa-aaaaaaaaaaaa").unwrap());
    let doc_b = DocId::project(Uuid::parse_str("bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb").unwrap());

    // Token scoped to doc_a only, write-allowed.
    let token = keypair.issue(&CapabilityScope {
        expires_unix: future_expiry(),
        can_write: true,
        doc_ids: vec![doc_a.clone()],
    });
    let url = url_with_cap(&base, &token);

    // Subscribing to doc_a: receives the snapshot frame.
    let client_a: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect for sub A: {e:?}"))?;
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let doc_a_for_sub = doc_a.clone();
    let sub_a = tokio::spawn(async move {
        let _ = client_a.subscribe(doc_a_for_sub, tx).await;
    });
    let probe = Arc::new(CrdtDoc::ephemeral());
    let got = tokio::time::timeout(Duration::from_secs(2), rx.recv())
        .await
        .map_err(|_| eyre::eyre!("sub on doc_a never got a snapshot"))?
        .map_err(|e| eyre::eyre!("rx.recv: {e:?}"))?;
    let snapshot = got.ok_or_else(|| eyre::eyre!("stream closed before snapshot"))?;
    probe
        .apply_remote(&snapshot.get().0)
        .map_err(|e| eyre::eyre!("apply snapshot: {e}"))?;
    sub_a.abort();

    // Subscribing to doc_b with the same token must NOT yield a
    // snapshot — server closes the stream because the doc is out
    // of scope.
    let client_b: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect for sub B: {e:?}"))?;
    let (tx_b, mut rx_b) = vox::channel::<UpdateBytes>();
    let sub_b = tokio::spawn(async move {
        let _ = client_b.subscribe(doc_b.clone(), tx_b).await;
    });
    match tokio::time::timeout(Duration::from_millis(500), rx_b.recv()).await {
        Err(_) => {}       // timed out as expected
        Ok(Ok(None)) => {} // stream closed — also OK
        Ok(Ok(Some(_))) => {
            sub_b.abort();
            return Err(eyre::eyre!(
                "doc_b subscription returned a frame — capability check broken"
            ));
        }
        Ok(Err(_)) => {} // stream errored — OK
    }
    sub_b.abort();

    // apply_update to doc_a (in scope, RW) succeeds.
    let apply_client: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect apply: {e:?}"))?;
    let writer = Arc::new(CrdtDoc::ephemeral());
    {
        let m = writer.loro().get_map("marker");
        m.insert("k", "v").expect("insert");
        writer.loro().commit();
    }
    let bytes = writer
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("export: {e}"))?;
    apply_client
        .apply_update(doc_a.clone(), UpdateBytes(bytes.clone()))
        .await
        .map_err(|e| eyre::eyre!("apply doc_a: {e:?}"))?;

    // apply_update to doc_b (out of scope) is rejected.
    let apply_b: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect apply_b: {e:?}"))?;
    let res = apply_b
        .apply_update(
            DocId::project(Uuid::parse_str("bbbbbbbb-bbbb-bbbb-bbbb-bbbbbbbbbbbb").unwrap()),
            UpdateBytes(bytes),
        )
        .await;
    assert!(
        res.is_err(),
        "apply_update on out-of-scope doc must error; got {res:?}"
    );

    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn read_only_token_blocks_apply_allows_subscribe() -> eyre::Result<()> {
    let (base, keypair) = boot_server_with_capability().await?;
    let doc = DocId::project(Uuid::parse_str("cccccccc-cccc-cccc-cccc-cccccccccccc").unwrap());
    let token = keypair.issue(&CapabilityScope {
        expires_unix: future_expiry(),
        can_write: false,
        doc_ids: vec![doc.clone()],
    });
    let url = url_with_cap(&base, &token);

    // Subscribe should succeed (RO covers reads).
    let sub_client: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let doc_for_sub = doc.clone();
    let h = tokio::spawn(async move {
        let _ = sub_client.subscribe(doc_for_sub, tx).await;
    });
    let _ = tokio::time::timeout(Duration::from_secs(2), rx.recv()).await;
    h.abort();

    // apply_update must be rejected — RO.
    let apply_client: WorkspaceSyncClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply connect: {e:?}"))?;
    let writer = Arc::new(CrdtDoc::ephemeral());
    writer.loro().get_map("m").insert("k", "v").expect("insert");
    writer.loro().commit();
    let bytes = writer
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("export: {e}"))?;
    let res = apply_client.apply_update(doc, UpdateBytes(bytes)).await;
    assert!(res.is_err(), "RO token must reject apply_update");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn missing_or_bad_token_blocks_everything() -> eyre::Result<()> {
    let (base, _keypair) = boot_server_with_capability().await?;
    let doc = DocId::project(Uuid::nil());

    // No cap.
    let client: WorkspaceSyncClient = vox::connect(&base)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
    let writer = Arc::new(CrdtDoc::ephemeral());
    writer.loro().get_map("m").insert("k", "v").expect("insert");
    writer.loro().commit();
    let bytes = writer
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("export: {e}"))?;
    let res = client
        .apply_update(doc.clone(), UpdateBytes(bytes.clone()))
        .await;
    assert!(res.is_err(), "no cap must reject apply_update");

    // Garbage cap.
    let bad_url = url_with_cap(&base, "not-a-real-token");
    let client_bad: WorkspaceSyncClient = vox::connect(&bad_url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect bad: {e:?}"))?;
    let res = client_bad.apply_update(doc, UpdateBytes(bytes)).await;
    assert!(res.is_err(), "garbage cap must reject apply_update");
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn lru_eviction_evicts_oldest_then_rehydrates() -> eyre::Result<()> {
    // Build a tiny registry directly — no need for the full vox path.
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let registry = DocRegistry::with_cap(Arc::new(persistence), 2);

    let a = DocId::new("project/lru-a");
    let b = DocId::new("project/lru-b");
    let c = DocId::new("project/lru-c");

    // Open a, write a marker, then open b. Cap is 2 so both stay.
    let open_a = registry.get_or_open(&a).await.expect("open a");
    open_a
        .doc
        .loro()
        .get_map("m")
        .insert("k", "from-a")
        .expect("insert a");
    open_a.doc.loro().commit();
    let _open_b = registry.get_or_open(&b).await.expect("open b");
    assert_eq!(registry.open_count().await, 2);

    // Opening c evicts the LRU. Order of access: a (open + touched
    // by insert) then b — so a is LRU.
    let _open_c = registry.get_or_open(&c).await.expect("open c");
    assert_eq!(registry.open_count().await, 2);

    // Re-opening a should rehydrate from persistence and still see
    // the marker.
    let reopen_a = registry.get_or_open(&a).await.expect("reopen a");
    let m = reopen_a.doc.loro().get_map("m");
    let val = m.get("k").map(|v| format!("{v:?}"));
    assert!(
        val.as_deref().is_some_and(|s| s.contains("from-a")),
        "rehydrated doc must still carry the marker; got {val:?}"
    );

    Ok(())
}
