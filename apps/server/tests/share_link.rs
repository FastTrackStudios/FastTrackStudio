//! Phase 4 — share-link e2e.
//!
//! Per `plans/decentralized-foundation.md` §13 Phase 4: admin
//! creates a share link scoped to `project/X` read-only, an
//! anonymous client redeems it (presents `?cap=<token>`) and can
//! subscribe; cannot `apply_update`. Admin revokes; the same
//! token's next subscribe fails (empty scope = closed stream).

use std::sync::Arc;
use std::time::Duration;

use crdt::CrdtDoc;
use crdt_seaorm::SeaOrmPersistence;
use project_proto::{
    DocId, DocIdArg, ShareLinkScope, ShareServiceClient, TokenIdArg, UpdateBytes,
    WorkspaceSyncClient,
};
use task_db::{default_database_url, open_and_migrate};
use task_server::capability::ServerKeypair;
use task_server::{AppState, AuthState, router};

async fn boot_capable_server() -> eyre::Result<(String, ServerKeypair)> {
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

/// An admin client connection needs SOME valid capability to reach
/// the ShareService dispatcher under enforcement. The admin uses a
/// long-lived RW token that covers every doc — bootstrap is out of
/// scope for Phase 4.
async fn admin_client(base: &str, keypair: &ServerKeypair) -> eyre::Result<ShareServiceClient> {
    let admin_scope = task_server::capability::CapabilityScope {
        expires_unix: i64::MAX / 2,
        can_write: true,
        // ShareService isn't enforced by doc-id (it's an admin
        // surface, not a per-doc data path). The connection-level
        // verify just needs a parseable token; the share methods
        // ignore the scope's doc_ids list.
        doc_ids: vec![],
        ..Default::default()
    };
    let token = keypair.issue(&admin_scope);
    let url = format!("{base}?cap={token}");
    vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("admin connect: {e:?}"))
}

#[tokio::test(flavor = "multi_thread")]
async fn admin_creates_link_anon_subscribes_admin_revokes() -> eyre::Result<()> {
    let (base, keypair) = boot_capable_server().await?;
    let doc_id = "project/share-test";

    // Admin: mint a read-only token scoped to project/share-test.
    let admin = admin_client(&base, &keypair).await?;
    let created = admin
        .create(ShareLinkScope {
            doc_id: doc_id.into(),
            can_write: false,
            ttl_seconds: 3600,
            label: Some("acme review".into()),
        })
        .await
        .map_err(|e| eyre::eyre!("create: {e:?}"))?;
    assert!(!created.token.is_empty());

    // Admin: list — the new link is there, not revoked.
    let listed = admin
        .list(DocIdArg {
            doc_id: doc_id.into(),
        })
        .await
        .map_err(|e| eyre::eyre!("list: {e:?}"))?;
    assert_eq!(listed.items.len(), 1);
    assert_eq!(listed.items[0].token_id, created.token_id);
    assert!(!listed.items[0].revoked);

    // Anonymous client redeems by passing the token in ?cap=.
    let anon_url = format!("{base}?cap={}", created.token);
    let anon: WorkspaceSyncClient = vox::connect(&anon_url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("anon connect: {e:?}"))?;
    let (tx, mut rx) = vox::channel::<UpdateBytes>();
    let doc = DocId::new(doc_id);
    let doc_for_sub = doc.clone();
    let h = tokio::spawn(async move {
        let _ = anon.subscribe(doc_for_sub, tx).await;
    });
    // Anon should get the empty-doc snapshot frame.
    let frame = tokio::time::timeout(Duration::from_secs(2), rx.recv())
        .await
        .map_err(|_| eyre::eyre!("anon subscribe got no frame"))?
        .map_err(|e| eyre::eyre!("rx.recv: {e:?}"))?
        .ok_or_else(|| eyre::eyre!("anon stream closed before snapshot"))?;
    let probe = Arc::new(CrdtDoc::ephemeral());
    probe
        .apply_remote(&frame.get().0)
        .map_err(|e| eyre::eyre!("apply: {e}"))?;
    h.abort();

    // Anon must NOT be able to apply_update (RO token).
    let anon_apply: WorkspaceSyncClient = vox::connect(&anon_url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("anon apply connect: {e:?}"))?;
    let writer = Arc::new(CrdtDoc::ephemeral());
    writer.loro().get_map("m").insert("k", "v").expect("insert");
    writer.loro().commit();
    let bytes = writer
        .loro()
        .export(loro::ExportMode::Snapshot)
        .map_err(|e| eyre::eyre!("export: {e}"))?;
    let res = anon_apply
        .apply_update(doc.clone(), UpdateBytes(bytes))
        .await;
    assert!(res.is_err(), "RO share-link must reject apply_update");

    // Admin revokes the token.
    admin
        .revoke(TokenIdArg {
            token_id: created.token_id,
        })
        .await
        .map_err(|e| eyre::eyre!("revoke: {e:?}"))?;

    // List again: revoked = true.
    let listed2 = admin
        .list(DocIdArg {
            doc_id: doc_id.into(),
        })
        .await
        .map_err(|e| eyre::eyre!("list 2: {e:?}"))?;
    assert!(listed2.items[0].revoked);

    // Anonymous reuse of the revoked token: server treats as
    // empty-scope. The stream closes without a snapshot frame.
    let anon2: WorkspaceSyncClient = vox::connect(&anon_url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("anon2 connect: {e:?}"))?;
    let (tx2, mut rx2) = vox::channel::<UpdateBytes>();
    let doc_for_sub2 = doc.clone();
    let h2 = tokio::spawn(async move {
        let _ = anon2.subscribe(doc_for_sub2, tx2).await;
    });
    match tokio::time::timeout(Duration::from_millis(500), rx2.recv()).await {
        Err(_) => {}       // timed out → expected (empty scope)
        Ok(Ok(None)) => {} // stream closed → expected
        Ok(Ok(Some(_))) => {
            h2.abort();
            return Err(eyre::eyre!(
                "revoked token still produced a frame — revocation broken"
            ));
        }
        Ok(Err(_)) => {}
    }
    h2.abort();

    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn revoking_unknown_token_returns_not_found() -> eyre::Result<()> {
    let (base, keypair) = boot_capable_server().await?;
    let admin = admin_client(&base, &keypair).await?;
    let res = admin
        .revoke(TokenIdArg {
            token_id: uuid::Uuid::new_v4(),
        })
        .await;
    assert!(res.is_err(), "revoking unknown token must error");
    Ok(())
}
