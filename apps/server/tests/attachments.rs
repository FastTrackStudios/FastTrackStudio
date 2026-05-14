//! Phase 7 — end-to-end attachment flow.
//!
//! Three scenarios:
//!
//! 1. `upload_then_download_roundtrip` — initiate via vox, PUT
//!    bytes to the signed URL, request a download URL, GET bytes
//!    back, verify byte-equality.
//! 2. `attachments_only_token_blocks_sync` — a capability token
//!    with `attachments_only: true` cannot subscribe to its doc.
//! 3. `bogus_download_token_rejected` — tampered token fails the
//!    HTTP GET.
//!
//! The tests bind to `127.0.0.1:0`, build the AppState with
//! enforcement on, and route HTTP requests through `reqwest` so
//! they hit the same axum handlers the real client would.

use std::sync::Arc;
use std::time::Duration;

use attachments_proto::{AttachmentServiceClient, CompleteUpload, ContentHashArg, InitiateUpload};
use crdt::CrdtDoc;
use crdt_seaorm::SeaOrmPersistence;
use project_proto::{DocId, UpdateBytes, WorkspaceSyncClient};
use task_db::{default_database_url, open_and_migrate};
use task_server::attachments::{LocalFsStore, ObjectStore};
use task_server::capability::{CapabilityScope, ServerKeypair};
use task_server::{AppState, AuthState, router};

async fn boot_server_with_blob_store_at(
    tmpdir: &std::path::Path,
) -> eyre::Result<(String, ServerKeypair)> {
    // Override the XDG path so the test's blob store points at the
    // tempdir. `AppState::new_with_capability` uses
    // `default_blob_root` — we redirect via env.
    unsafe {
        std::env::set_var("XDG_DATA_HOME", tmpdir);
    }
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let auth = AuthState::open("sqlite::memory:", "test-secret-at-least-32-bytes!!!").await?;
    let keypair = ServerKeypair::generate_ephemeral();
    let state = AppState::new_with_capability(persistence, auth, keypair.clone()).await?;

    // Also wire the public_base_url onto the attachment service —
    // it was constructed without the bind address at boot, so we
    // patch the service to emit absolute URLs once we know the port.
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let base_url = format!("http://127.0.0.1:{port}");
    // Rebuild the AttachmentServiceImpl with the right base URL.
    // We're swapping out the Arc on the state we just built.
    let blob_root = tmpdir.join("task-server").join("blobs");
    let _ = std::fs::create_dir_all(&blob_root);
    let store: Arc<dyn ObjectStore> = Arc::new(LocalFsStore::new(blob_root));
    let svc = Arc::new(task_server::attachments::AttachmentServiceImpl::new(
        keypair.clone(),
        store,
        base_url.clone(),
    ));
    let mut state = state;
    state.attachments = svc;

    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    Ok((base_url, keypair))
}

fn admin_cap(keypair: &ServerKeypair, doc_id: &str) -> String {
    let scope = CapabilityScope {
        expires_unix: i64::MAX / 2,
        can_write: true,
        doc_ids: vec![DocId::new(doc_id)],
        ..Default::default()
    };
    keypair.issue(&scope)
}

fn attachments_only_cap(keypair: &ServerKeypair, doc_id: &str) -> String {
    let scope = CapabilityScope {
        expires_unix: i64::MAX / 2,
        can_write: false,
        doc_ids: vec![DocId::new(doc_id)],
        attachments_only: true,
        ..Default::default()
    };
    keypair.issue(&scope)
}

#[tokio::test(flavor = "multi_thread")]
async fn upload_then_download_roundtrip() -> eyre::Result<()> {
    let tmp = tempfile::tempdir().expect("tempdir");
    let (base_url, keypair) = boot_server_with_blob_store_at(tmp.path()).await?;
    let doc_id = "project/attach-test";
    let cap = admin_cap(&keypair, doc_id);

    // 1. Initiate over vox.
    let vox_url = format!("{}/vox?cap={}", base_url.replace("http", "ws"), cap);
    let client: AttachmentServiceClient = vox::connect(&vox_url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
    let payload = b"hello attachments\n".to_vec();
    let ticket = client
        .initiate_upload(InitiateUpload {
            doc_id: doc_id.into(),
            filename: "hello.txt".into(),
            mime_type: "text/plain".into(),
            size_bytes: payload.len() as u64,
        })
        .await
        .map_err(|e| eyre::eyre!("initiate: {e:?}"))?;
    assert_eq!(ticket.method, "PUT");
    assert!(ticket.upload_url.starts_with(&base_url));

    // 2. PUT the bytes via reqwest.
    let http = reqwest::Client::new();
    let put_resp = http
        .put(&ticket.upload_url)
        .body(payload.clone())
        .send()
        .await?;
    assert!(
        put_resp.status().is_success(),
        "PUT failed: {}",
        put_resp.status()
    );
    let returned_hash = put_resp.text().await?;
    // sha256 of "hello attachments\n"
    let expected_hash = sha256_hex(&payload);
    assert_eq!(returned_hash, expected_hash);

    // 3. Complete on vox (registers the catalog row).
    let meta = client
        .complete_upload(CompleteUpload {
            upload_id: ticket.upload_id,
            content_hash: returned_hash.clone(),
        })
        .await
        .map_err(|e| eyre::eyre!("complete: {e:?}"))?;
    assert_eq!(meta.content_hash, returned_hash);
    assert_eq!(meta.filename, "hello.txt");
    assert_eq!(meta.doc_id, doc_id);

    // 4. Get a download URL (could be the same client OR a fresh
    // one; here we reuse). Then GET the bytes.
    let dl = client
        .get_download_url(ContentHashArg {
            content_hash: returned_hash.clone(),
        })
        .await
        .map_err(|e| eyre::eyre!("download url: {e:?}"))?;
    let got = http.get(&dl.url).send().await?;
    assert!(got.status().is_success());
    let body = got.bytes().await?.to_vec();
    assert_eq!(body, payload);

    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn attachments_only_token_blocks_sync() -> eyre::Result<()> {
    let tmp = tempfile::tempdir().expect("tempdir");
    let (base_url, keypair) = boot_server_with_blob_store_at(tmp.path()).await?;
    let doc_id = "project/aonly-test";
    let cap = attachments_only_cap(&keypair, doc_id);

    let vox_url = format!("{}/vox?cap={}", base_url.replace("http", "ws"), cap);

    // Attachment vox calls SHOULD succeed (initiate_upload works
    // even on attachments_only tokens).
    let client: AttachmentServiceClient = vox::connect(&vox_url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
    let _ = client
        .initiate_upload(InitiateUpload {
            doc_id: doc_id.into(),
            filename: "ok.bin".into(),
            mime_type: "application/octet-stream".into(),
            size_bytes: 0,
        })
        .await
        .map_err(|e| eyre::eyre!("initiate on attachments_only: {e:?}"))?;

    // But subscribing on the same token must time out / close —
    // attachments_only blocks sync.
    let sub_client: WorkspaceSyncClient = vox::connect(&vox_url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("sub connect: {e:?}"))?;
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let doc = DocId::new(doc_id);
    let doc_for_sub = doc.clone();
    let h = tokio::spawn(async move {
        let _ = sub_client.subscribe(doc_for_sub, tx).await;
    });
    match tokio::time::timeout(Duration::from_millis(500), rx.recv()).await {
        Err(_) => {}       // timed out — server closed without sending
        Ok(Ok(None)) => {} // closed cleanly
        Ok(Ok(Some(_))) => {
            h.abort();
            return Err(eyre::eyre!(
                "attachments_only token still received a sync frame"
            ));
        }
        Ok(Err(_)) => {}
    }
    h.abort();

    // And apply_update must error.
    let writer = Arc::new(CrdtDoc::ephemeral());
    writer.loro().get_map("m").insert("k", "v").expect("insert");
    writer.loro().commit();
    let bytes = writer
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("export: {e}"))?;
    let apply: WorkspaceSyncClient = vox::connect(&vox_url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("apply connect: {e:?}"))?;
    let res = apply.apply_update(doc, UpdateBytes(bytes)).await;
    assert!(
        res.is_err(),
        "attachments_only must reject apply_update; got {res:?}"
    );

    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn bogus_download_token_rejected() -> eyre::Result<()> {
    let tmp = tempfile::tempdir().expect("tempdir");
    let (base_url, _kp) = boot_server_with_blob_store_at(tmp.path()).await?;
    let bad_url = format!("{base_url}/blobs/download/somehash?token=not-a-real-token");
    let resp = reqwest::get(&bad_url).await?;
    assert_eq!(resp.status(), reqwest::StatusCode::UNAUTHORIZED);
    Ok(())
}

fn sha256_hex(bytes: &[u8]) -> String {
    use sha2::{Digest, Sha256};
    let mut h = Sha256::new();
    h.update(bytes);
    let out = h.finalize();
    let mut s = String::with_capacity(out.len() * 2);
    for b in out {
        s.push_str(&format!("{b:02x}"));
    }
    s
}
