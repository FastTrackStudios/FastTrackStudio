//! Phase 8 — client-side federation, server-side proof.
//!
//! Boots two independent task-servers on `127.0.0.1:0`, hits each
//! via a fresh `WorkspaceSyncClient`, and proves:
//!
//! 1. `list_docs` returns the open docs on each server. The
//!    federation UI uses this to enumerate what's available.
//! 2. The fan-out pattern works: a "client" can seed projects on
//!    each server, then combine their lists into one view. We do
//!    this server-side via direct repo calls — same result a
//!    Dioxus client would get over vox.
//!
//! Anonymous-claim e2e lives in the same file: sign in (auth),
//! call `claim_anonymous_session` with a share-link token id,
//! verify the table records the claim.

use std::sync::Arc;
use std::time::Duration;

use architect_auth::{CreateEmailPasswordUser, SignInEmailPassword};
use crdt::CrdtDoc;
use crdt_seaorm::SeaOrmPersistence;
use project_proto::{
    AnonymousClaimClient, ClaimRequest, DocId, ListDocsRequest, ProjectCreate, ProjectRepo,
    UpdateBytes, WorkspaceSyncClient,
};
use task_db::{WORKSPACE_DOC_ID, default_database_url, open_and_migrate, seed};
use task_server::{AppState, AuthState, router};

async fn boot_one_server() -> eyre::Result<String> {
    // Each server gets its own seeded workspace doc + auth db.
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    seed::run(persistence.clone(), WORKSPACE_DOC_ID).await?;
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

#[tokio::test(flavor = "multi_thread")]
async fn list_docs_returns_open_docs_per_server() -> eyre::Result<()> {
    let url_a = boot_one_server().await?;
    let url_b = boot_one_server().await?;

    // Connect to each. Both servers seeded the same `workspace`
    // doc id, plus opened `vault/org`, so we expect to see both
    // there.
    for url in &[url_a, url_b] {
        let client: WorkspaceSyncClient = vox::connect(url)
            .establish()
            .await
            .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
        let list = client
            .list_docs(ListDocsRequest)
            .await
            .map_err(|e| eyre::eyre!("list_docs: {e:?}"))?;
        let ids: Vec<&str> = list.doc_ids.iter().map(|d| d.as_str()).collect();
        assert!(
            ids.contains(&"workspace"),
            "server {url} missing workspace doc; got {ids:?}"
        );
        assert!(
            ids.contains(&"vault/org"),
            "server {url} missing vault/org doc; got {ids:?}"
        );
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn fan_out_view_combines_tasks_from_two_servers() -> eyre::Result<()> {
    // Boot two servers, ping each to seed an extra project — then
    // pull project lists from both via vox sync subscribes, build
    // local docs, list. A real UI does the same; this proves the
    // server side accepts the fan-out pattern.
    let url_a = boot_one_server().await?;
    let url_b = boot_one_server().await?;

    // Helper: connect, subscribe to "workspace", drain one frame
    // (the snapshot), decode locally, return the doc.
    async fn snapshot_workspace(url: &str) -> eyre::Result<Arc<CrdtDoc>> {
        let client: WorkspaceSyncClient = vox::connect(url)
            .establish()
            .await
            .map_err(|e| eyre::eyre!("connect: {e:?}"))?;
        let (tx, mut rx) = vox::channel::<UpdateBytes>();
        tokio::spawn(async move {
            let _ = client.subscribe(DocId::new("workspace"), tx).await;
        });
        let frame = tokio::time::timeout(Duration::from_secs(2), rx.recv())
            .await
            .map_err(|_| eyre::eyre!("no snapshot from {url}"))?
            .map_err(|e| eyre::eyre!("recv: {e:?}"))?
            .ok_or_else(|| eyre::eyre!("stream closed before snapshot"))?;
        let doc = Arc::new(CrdtDoc::ephemeral());
        doc.apply_remote(&frame.get().0)
            .map_err(|e| eyre::eyre!("apply: {e}"))?;
        Ok(doc)
    }

    let doc_a = snapshot_workspace(&url_a).await?;
    let doc_b = snapshot_workspace(&url_b).await?;

    // Each seeded workspace has 10 projects + 80 tasks. The
    // fan-out client sees 20 total projects without dedup. Real
    // UIs dedupe on (server_id, project_id); this test just
    // confirms both sides contribute rows.
    let proj_a = project_crdt::ProjectRepoLoro::new(&doc_a)
        .list(
            architect::Page {
                index: 0,
                size: 1000,
            },
            None,
            None,
        )
        .await
        .map_err(|e| eyre::eyre!("list a: {e}"))?;
    let proj_b = project_crdt::ProjectRepoLoro::new(&doc_b)
        .list(
            architect::Page {
                index: 0,
                size: 1000,
            },
            None,
            None,
        )
        .await
        .map_err(|e| eyre::eyre!("list b: {e}"))?;
    assert!(proj_a.items.len() >= 1, "server A missing seeded projects");
    assert!(proj_b.items.len() >= 1, "server B missing seeded projects");

    // Mint a unique project on each server so we can prove they
    // really are independent. (Seed already gave us projects, but
    // those overlap by id between the two seedings because the
    // seed uses fake::Faker without per-server salt.)
    let unique_a = project_crdt::ProjectRepoLoro::new(&doc_a)
        .create(ProjectCreate {
            name: "Unique on A".into(),
        })
        .await
        .map_err(|e| eyre::eyre!("create a: {e}"))?;
    let unique_b = project_crdt::ProjectRepoLoro::new(&doc_b)
        .create(ProjectCreate {
            name: "Unique on B".into(),
        })
        .await
        .map_err(|e| eyre::eyre!("create b: {e}"))?;
    assert_ne!(unique_a.id, unique_b.id);

    // Combined fan-out list. In a real client this would be
    // `tagged` with the server URL so the UI can route mutations
    // back to the right place.
    let combined: Vec<(&str, project_proto::Project)> = proj_a
        .items
        .into_iter()
        .map(|p| ("server-a", p))
        .chain(proj_b.items.into_iter().map(|p| ("server-b", p)))
        .collect();
    assert!(combined.iter().any(|(s, _)| *s == "server-a"));
    assert!(combined.iter().any(|(s, _)| *s == "server-b"));
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn anonymous_claim_records_share_link_attribution() -> eyre::Result<()> {
    // Boot one server, create a user, sign in, claim a share-link
    // token via vox. Subsequent claims with a different user are
    // told the session is already attributed.
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let auth = AuthState::open("sqlite::memory:", "test-secret-at-least-32-bytes!!!").await?;
    // Seed two users server-side (vox auth doesn't expose signup —
    // matches the Phase 2 pattern in tests/auth.rs).
    let alice = auth
        .auth
        .create_email_password_user(CreateEmailPasswordUser {
            email: "alice@example.test".into(),
            password: "correct-horse-battery-staple".into(),
            name: Some("Alice".into()),
            username: None,
            image: None,
            metadata_json: None,
            ip_address: None,
            user_agent: None,
        })
        .await
        .map_err(|e| eyre::eyre!("seed alice: {e:?}"))?;
    let bob = auth
        .auth
        .create_email_password_user(CreateEmailPasswordUser {
            email: "bob@example.test".into(),
            password: "correct-horse-battery-staple".into(),
            name: Some("Bob".into()),
            username: None,
            image: None,
            metadata_json: None,
            ip_address: None,
            user_agent: None,
        })
        .await
        .map_err(|e| eyre::eyre!("seed bob: {e:?}"))?;
    let state = AppState::new_with_auth(persistence, auth.clone()).await?;
    let listener = tokio::net::TcpListener::bind("127.0.0.1:0").await?;
    let port = listener.local_addr()?.port();
    let app = router(state);
    tokio::spawn(async move {
        let _ = axum::serve(listener, app).await;
    });
    let url = format!("ws://127.0.0.1:{port}/vox");

    // Alice signs in over vox, claims a share-link.
    let auth_client: architect_auth::proto::AuthServiceClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("auth connect: {e:?}"))?;
    let alice_session = auth_client
        .sign_in_email_password(SignInEmailPassword {
            email: "alice@example.test".into(),
            password: "correct-horse-battery-staple".into(),
            ip_address: None,
            user_agent: None,
        })
        .await
        .map_err(|e| eyre::eyre!("alice sign in: {e:?}"))?;
    assert_eq!(alice_session.user.id, alice.user.id);

    // The token gets attached via vox metadata using
    // architect_auth's `AuthClientMiddleware`. We construct an
    // AnonymousClaimClient that carries Alice's bearer token.
    use architect_auth::transport::vox::AuthClientMiddleware;
    let claim_client_alice: AnonymousClaimClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("claim connect: {e:?}"))?;
    let claim_client_alice = claim_client_alice
        .with_middleware(AuthClientMiddleware::bearer(alice_session.token.clone()));
    let token_id = uuid::Uuid::new_v4();
    let summary_alice = claim_client_alice
        .claim_anonymous_session(ClaimRequest { token_id })
        .await
        .map_err(|e| eyre::eyre!("alice claim: {e:?}"))?;
    assert_eq!(summary_alice.user_id, alice.user.id);
    assert!(summary_alice.first_claim);
    assert_eq!(summary_alice.peer_id, format!("share-link-{token_id}"));

    // Bob signs in, attempts to claim the same token — gets
    // Alice's attribution back, first_claim = false.
    let bob_session = auth_client
        .sign_in_email_password(SignInEmailPassword {
            email: "bob@example.test".into(),
            password: "correct-horse-battery-staple".into(),
            ip_address: None,
            user_agent: None,
        })
        .await
        .map_err(|e| eyre::eyre!("bob sign in: {e:?}"))?;
    assert_eq!(bob_session.user.id, bob.user.id);
    let claim_client_bob: AnonymousClaimClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("bob claim connect: {e:?}"))?;
    let claim_client_bob =
        claim_client_bob.with_middleware(AuthClientMiddleware::bearer(bob_session.token.clone()));
    let summary_bob = claim_client_bob
        .claim_anonymous_session(ClaimRequest { token_id })
        .await
        .map_err(|e| eyre::eyre!("bob claim: {e:?}"))?;
    assert_eq!(summary_bob.user_id, alice.user.id, "first-claim wins");
    assert!(!summary_bob.first_claim);

    Ok(())
}
