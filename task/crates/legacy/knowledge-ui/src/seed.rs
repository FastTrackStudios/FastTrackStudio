//! Offline-first demo seed. Populates an empty `CrdtDoc` with a
//! small set of pages + blocks that exercise every Logseq-flavored
//! feature the live outliner supports — block refs, page embeds,
//! tasks, properties, planning timestamps, queries, namespaces.
//!
//! Wired from `KnowledgeLive`: on cold start the route counts
//! pages; when zero, this seeder runs so the desktop binary
//! comes up with content even without a server connection.

use std::sync::Arc;

use chrono::Utc;
use crdt::CrdtDoc;
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::{
    BlockCreate, BlockRepo, PageCreate, PageRepo, VaultCreate, VaultRepo, architect::RepoError,
};
use uuid::Uuid;

/// Returns true when the doc already has at least one page. Used
/// by `KnowledgeLive` to skip seeding on warm starts.
pub async fn doc_has_pages(doc: &CrdtDoc) -> bool {
    let page_repo = PageRepoLoro::new(doc);
    let page = knowledge_proto::architect::Page { index: 0, size: 1 };
    match page_repo.list(page, None, None).await {
        Ok(list) => !list.items.is_empty(),
        Err(_) => false,
    }
}

/// Seed the doc with sample content. Idempotent in the sense that
/// callers should gate it on `doc_has_pages` returning false — this
/// fn doesn't deduplicate against existing data.
pub async fn seed_demo(doc: Arc<CrdtDoc>) -> Result<(), RepoError> {
    let now = Utc::now();
    let vault_repo = VaultRepoLoro::new(&doc);
    let page_repo = PageRepoLoro::new(&doc);
    let block_repo = BlockRepoLoro::new(&doc);

    // Vault lives at `~/Task/` by default — Logseq-style local
    // graph. graph_writer mirrors every commit back to
    // `~/Task/pages/*.md` and `~/Task/journals/*.md`, so the
    // user (or another editor) can read the canonical source on
    // disk. We create the directory tree up front so the first
    // write doesn't race.
    let default_root = default_vault_root();
    if let Some(root) = &default_root {
        let _ = std::fs::create_dir_all(root.join("pages"));
        let _ = std::fs::create_dir_all(root.join("journals"));
        let _ = std::fs::create_dir_all(root.join("assets"));
    }
    let vault = vault_repo
        .create(VaultCreate {
            name: "Task".into(),
            root_path: default_root.as_ref().map(|p| p.to_string_lossy().into()),
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: String::new(),
            default_view_mode: "live-preview".into(),
            config_json: "{}".into(),
        })
        .await?;

    let home = mk_page(&page_repo, vault.id, "Home", None, now).await?;
    let welcome = mk_page(&page_repo, vault.id, "Welcome", None, now).await?;
    let tasks = mk_page(&page_repo, vault.id, "Tasks", None, now).await?;
    let projects_alpha = mk_page(&page_repo, vault.id, "Projects/Alpha", None, now).await?;
    let projects_beta = mk_page(&page_repo, vault.id, "Projects/Beta", None, now).await?;
    let today_journal = mk_page(
        &page_repo,
        vault.id,
        &now.format("%Y-%m-%d").to_string(),
        Some(now.format("%Y-%m-%d").to_string()),
        now,
    )
    .await?;

    // ── Home page ─ scratch-pad, auto-opens on cold start ─
    mk_block(
        &block_repo,
        vault.id,
        home,
        "a",
        "heading",
        "Home",
        Some(1),
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        home,
        "b",
        "paragraph",
        "Welcome to your Task vault. This is your scratch-pad — anything you write here lives in `~/Task/pages/Home.md`.",
        None,
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        home,
        "c",
        "paragraph",
        "Try a wikilink: [[Welcome]] · A tag: #demo · A query: {{query #demo}}",
        None,
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        home,
        "d",
        "paragraph",
        "Press ⌘K to search. The sidebar (Cards / Tasks / Graph / Settings) lives on the left.",
        None,
        "{}",
    )
    .await?;

    // ── Welcome page ─────────────────────────────────────
    let welcome_intro = mk_block(
        &block_repo,
        vault.id,
        welcome,
        "a",
        "heading",
        "Welcome to Task",
        Some(1),
        "{}",
    )
    .await?;
    let _ = welcome_intro;
    mk_block(
        &block_repo,
        vault.id,
        welcome,
        "b",
        "paragraph",
        "This is a Logseq-style outliner. Try `[[Tasks]]` to navigate.",
        None,
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        welcome,
        "c",
        "paragraph",
        "Tag a block with `#demo` and run `{{query #demo}}` elsewhere to see it pulled in.",
        None,
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        welcome,
        "d",
        "paragraph",
        "Embed an entire page with `![[Tasks]]`, or reference one block by id.",
        None,
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        welcome,
        "e",
        "paragraph",
        "Press `i` to enter Insert mode (vim). `Esc` returns to Normal. `:wq` etc. work.",
        None,
        "{}",
    )
    .await?;

    // ── Tasks page — text-keyword tasks + planning + props.
    mk_block(
        &block_repo,
        vault.id,
        tasks,
        "a",
        "heading",
        "Open tasks",
        Some(2),
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        tasks,
        "b",
        "paragraph",
        "TODO Buy groceries #demo",
        None,
        r#"{"priority":"low"}"#,
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        tasks,
        "c",
        "paragraph",
        "DOING Ship Logseq parity demo\nSCHEDULED: <2026-05-20>\nDEADLINE: <2026-05-22>",
        None,
        r#"{"priority":"high","area":"engineering"}"#,
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        tasks,
        "d",
        "paragraph",
        "DONE Wire publish-core into the live outliner",
        None,
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        tasks,
        "e",
        "paragraph",
        "WAITING Review PR from @bot #demo",
        None,
        "{}",
    )
    .await?;

    // ── Projects/Alpha + Projects/Beta — namespace demo.
    mk_block(
        &block_repo,
        vault.id,
        projects_alpha,
        "a",
        "heading",
        "Project Alpha",
        Some(1),
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        projects_alpha,
        "b",
        "paragraph",
        "Notes on the alpha rollout. See [[Welcome]] for context.",
        None,
        r#"{"status":"active","owner":"cody"}"#,
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        projects_beta,
        "a",
        "heading",
        "Project Beta",
        Some(1),
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        projects_beta,
        "b",
        "paragraph",
        "Children of `Projects/`: {{namespace Projects}}",
        None,
        "{}",
    )
    .await?;

    // ── Today's journal.
    mk_block(
        &block_repo,
        vault.id,
        today_journal,
        "a",
        "paragraph",
        "Journal entry for today.",
        None,
        "{}",
    )
    .await?;
    mk_block(
        &block_repo,
        vault.id,
        today_journal,
        "b",
        "paragraph",
        "Tagged demo blocks: {{query #demo}}",
        None,
        "{}",
    )
    .await?;

    Ok(())
}

/// Resolve the default vault root: `~/Task/`. Returns None when
/// the home directory can't be looked up (very rare) or when
/// we're building for wasm where filesystem access is sandboxed.
pub fn default_vault_root() -> Option<std::path::PathBuf> {
    #[cfg(target_arch = "wasm32")]
    {
        None
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let home = std::env::var_os("HOME").map(std::path::PathBuf::from)?;
        Some(home.join("Task"))
    }
}

async fn mk_page(
    repo: &PageRepoLoro,
    vault_id: Uuid,
    basename: &str,
    journal_day: Option<String>,
    now: chrono::DateTime<Utc>,
) -> Result<Uuid, RepoError> {
    let is_journal = journal_day.is_some();
    let p = repo
        .create(PageCreate {
            vault_id,
            folder_id: None,
            path: format!("{basename}.md"),
            basename: basename.into(),
            ext: "md".into(),
            aliases: Vec::new(),
            frontmatter_json: "{}".into(),
            stat_ctime: now,
            stat_mtime: now,
            stat_size: 0,
            is_journal,
            journal_day,
            shadow_for_kind: None,
            shadow_for_id: None,
        })
        .await?;
    Ok(p.id)
}

#[allow(clippy::too_many_arguments)]
async fn mk_block(
    repo: &BlockRepoLoro,
    vault_id: Uuid,
    page_id: Uuid,
    sort_key: &str,
    kind: &str,
    content: &str,
    heading_level: Option<i32>,
    properties_json: &str,
) -> Result<Uuid, RepoError> {
    let b = repo
        .create(BlockCreate {
            vault_id,
            page_id,
            parent_block_id: None,
            sort_key: sort_key.into(),
            kind: kind.into(),
            content: content.into(),
            heading_level,
            list_ordered: false,
            list_task: None,
            code_lang: None,
            callout_kind: None,
            callout_foldable: false,
            properties_json: properties_json.into(),
            obsidian_block_id: None,
            collapsed: false,
            refs_json: "[]".into(),
            canvas_node_json: None,
        })
        .await?;
    Ok(b.id)
}
