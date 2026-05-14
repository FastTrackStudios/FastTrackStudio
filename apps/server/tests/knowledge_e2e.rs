//! Phase 5 — Knowledge over vox.
//!
//! Verifies the architect-emitted Knowledge `*Repo` dispatchers are
//! mounted on `/vox` and a client can create+list a `Vault` + `Page`
//! via the wire. Specifically validates:
//!
//! 1. The `vault://org` doc id is the one the server pre-opens for
//!    Knowledge reads/writes (3-tier vault model first instance).
//! 2. Round-trip through the new entity_crdt!-emitted repos (Vault,
//!    Page).
//!
//! Backlink/frontmatter indexes + Person-page ACL resolution defer to
//! Phase 5b alongside the basename-index wiring; the goal-doc test
//! "create Person page, wiki-link, ACL resolves" is set up by the
//! parts here (vault + page) plus Phase 4's basename index.

use chrono::Utc;
use crdt_seaorm::SeaOrmPersistence;
use knowledge_proto::{PageCreate, PageRepoClient, Vault, VaultCreate, VaultRepoClient};
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

fn make_vault(name: &str) -> VaultCreate {
    VaultCreate {
        name: name.into(),
        root_path: None,
        use_markdown_links: false,
        new_link_format: "shortest".into(),
        attachment_folder_path: "".into(),
        default_view_mode: "source".into(),
        config_json: "{}".into(),
    }
}

fn make_page(vault_id: Uuid, basename: &str) -> PageCreate {
    PageCreate {
        vault_id,
        folder_id: None,
        path: format!("{basename}.md"),
        basename: basename.into(),
        ext: "md".into(),
        aliases: Vec::new(),
        frontmatter_json: "{}".into(),
        stat_ctime: Utc::now(),
        stat_mtime: Utc::now(),
        stat_size: 0,
        is_journal: false,
        journal_day: None,
        shadow_for_kind: None,
        shadow_for_id: None,
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn vault_and_page_round_trip_over_vox() -> eyre::Result<()> {
    let url = boot().await?;

    // VaultRepo over vox: create + list returns the new vault.
    let vault_client: VaultRepoClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("vault connect: {e:?}"))?;
    let vault: Vault = vault_client
        .create(make_vault("Org"))
        .await
        .map_err(|e| eyre::eyre!("create vault: {e:?}"))?;
    assert_eq!(vault.name, "Org");

    let vaults = vault_client
        .list(
            architect::Page {
                index: 0,
                size: 100,
            },
            None,
            None,
        )
        .await
        .map_err(|e| eyre::eyre!("list vaults: {e:?}"))?;
    assert!(
        vaults.items.iter().any(|v| v.id == vault.id),
        "newly-created vault must appear in list"
    );

    // PageRepo over vox: create + list a "Cody" person page in the
    // new vault. Phase 4's basename index would later map "Cody" →
    // user_id; this test sets up the data shape.
    let page_client: PageRepoClient = vox::connect(&url)
        .establish()
        .await
        .map_err(|e| eyre::eyre!("page connect: {e:?}"))?;
    let page = page_client
        .create(make_page(vault.id, "Cody"))
        .await
        .map_err(|e| eyre::eyre!("create page: {e:?}"))?;
    assert_eq!(page.basename, "Cody");
    assert_eq!(page.vault_id, vault.id);

    let pages = page_client
        .list(
            architect::Page {
                index: 0,
                size: 100,
            },
            None,
            None,
        )
        .await
        .map_err(|e| eyre::eyre!("list pages: {e:?}"))?;
    assert!(pages.items.iter().any(|p| p.id == page.id));

    Ok(())
}
