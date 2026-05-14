//! Phase 5b — server-side Knowledge indexes.
//!
//! Validates the three correlated indexes that the `KnowledgeIndexer`
//! maintains over the org vault:
//!
//! 1. **Basename index** — a Page whose frontmatter carries
//!    `auth_user_id` is reachable from `basename_index.resolve()`
//!    (this is the seam the Phase 4 ACL resolver consumes).
//! 2. **Backlink index** — a Block whose `content` contains
//!    `[[Target]]` is listed under `backlinks.referrers_of("Target")`.
//! 3. **Frontmatter index** — a Page with `kind: task` is reachable
//!    via `frontmatter.pages_with("kind", "task")`.

use chrono::Utc;
use crdt_seaorm::SeaOrmPersistence;
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::{BlockCreate, BlockRepo, PageCreate, PageRepo, VaultCreate, VaultRepo};
use task_db::{default_database_url, open_and_migrate};
use task_server::basename_index::BasenameIndex;
use task_server::{AppState, AuthState};
use uuid::Uuid;

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

fn make_page(vault_id: Uuid, basename: &str, frontmatter_json: &str) -> PageCreate {
    PageCreate {
        vault_id,
        folder_id: None,
        path: format!("{basename}.md"),
        basename: basename.into(),
        ext: "md".into(),
        aliases: Vec::new(),
        frontmatter_json: frontmatter_json.into(),
        stat_ctime: Utc::now(),
        stat_mtime: Utc::now(),
        stat_size: 0,
        is_journal: false,
        journal_day: None,
        shadow_for_kind: None,
        shadow_for_id: None,
    }
}

fn make_block(vault_id: Uuid, page_id: Uuid, content: &str) -> BlockCreate {
    BlockCreate {
        vault_id,
        page_id,
        parent_block_id: None,
        sort_key: "a0".into(),
        kind: "paragraph".into(),
        content: content.into(),
        heading_level: None,
        list_ordered: false,
        list_task: None,
        code_lang: None,
        callout_kind: None,
        callout_foldable: false,
        properties_json: "{}".into(),
        obsidian_block_id: None,
        collapsed: false,
        refs_json: "[]".into(),
        canvas_node_json: None,
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn indexer_resolves_person_pages_backlinks_and_frontmatter() -> eyre::Result<()> {
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let auth = AuthState::open("sqlite::memory:", "test-secret-at-least-32-bytes!!!").await?;
    let state = AppState::new_with_auth(persistence, auth).await?;

    // Drive the in-process repos directly (no need for the vox path
    // here — we're testing the indexer, not the wire).
    let vault_repo: VaultRepoLoro = (*state.vault_repo).clone();
    let page_repo: PageRepoLoro = (*state.page_repo).clone();
    let block_repo: BlockRepoLoro = (*state.block_repo).clone();

    let vault = vault_repo
        .create(make_vault("Org"))
        .await
        .map_err(|e| eyre::eyre!("create vault: {e}"))?;

    let cody_user_id = Uuid::new_v4();
    let cody_fm = serde_json::json!({
        "kind": "person",
        "auth_user_id": cody_user_id.to_string()
    })
    .to_string();
    let cody_page = page_repo
        .create(make_page(vault.id, "Cody", &cody_fm))
        .await
        .map_err(|e| eyre::eyre!("create cody: {e}"))?;

    // A task page that references Cody via wiki-link inside a block.
    let task_fm = serde_json::json!({"kind": "task", "status": "in_progress"}).to_string();
    let task_page = page_repo
        .create(make_page(vault.id, "Buy capacitors", &task_fm))
        .await
        .map_err(|e| eyre::eyre!("create task: {e}"))?;
    let _ref_block = block_repo
        .create(make_block(
            vault.id,
            task_page.id,
            "assigned to [[Cody]] — pick up at the [[Acme/store|store]] tomorrow",
        ))
        .await
        .map_err(|e| eyre::eyre!("create block: {e}"))?;

    // The indexer runs as a background task on every commit. Give it
    // a tick to drain.
    tokio::time::sleep(std::time::Duration::from_millis(200)).await;
    // Belt-and-suspenders: force an explicit rebuild so the test
    // can't flake on a slow runner.
    state
        .indexer
        .rebuild()
        .await
        .map_err(|e| eyre::eyre!("rebuild: {e}"))?;

    // ── Basename index: [[Cody]] → cody_user_id ────────────────
    assert_eq!(
        state.basename_index.resolve("Cody"),
        Some(cody_user_id),
        "Cody's page must map basename → auth_user_id"
    );

    // ── Frontmatter index: kind=task surfaces the task page ────
    let tasks = state.indexer.frontmatter.pages_with("kind", "task");
    assert!(
        tasks.contains(&task_page.id),
        "kind=task should surface task_page.id ({}); got {tasks:?}",
        task_page.id
    );
    let people = state.indexer.frontmatter.pages_with("kind", "person");
    assert!(people.contains(&cody_page.id));

    // ── Backlink index: [[Cody]] referrers includes task page ──
    let referrers = state.indexer.backlinks.referrers_of("Cody");
    assert!(
        referrers.contains(&task_page.id),
        "Cody's referrers must include task_page; got {referrers:?}"
    );
    // The "Acme/store" form gets the first segment ("Acme") as the
    // target — wiki-link parsing splits on `|` and `#`, not `/`.
    let acme_refs = state.indexer.backlinks.referrers_of("Acme/store");
    assert!(
        acme_refs.contains(&task_page.id),
        "the aliased link's bare target must also be indexed"
    );

    Ok(())
}
