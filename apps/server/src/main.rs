//! Thin shell — env-driven config, then hands off to `task_server::router`.

use std::net::SocketAddr;

use eyre::WrapErr;
use task_db::{WORKSPACE_DOC_ID, default_database_url, open_and_migrate, seed};
use task_server::{AppState, router};
use tracing::info;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "task_server=info,task_db=info,tower_http=info".into()),
        )
        .init();

    let database_url = default_database_url();
    let bind: SocketAddr = std::env::var("TASK_SERVER_BIND")
        .unwrap_or_else(|_| "0.0.0.0:9090".into())
        .parse()
        .wrap_err("invalid TASK_SERVER_BIND")?;
    let seed_on_start = env_truthy("TASK_SERVER_SEED");

    info!(%database_url, "connecting");
    let persistence = open_and_migrate(&database_url).await?;

    if seed_on_start {
        info!("TASK_SERVER_SEED=1 — seeding workspace doc before listening");
        seed::run(persistence.clone(), WORKSPACE_DOC_ID).await?;
    }

    let state = AppState::new(persistence).await?;

    if seed_on_start {
        seed_knowledge_org_vault(&state).await?;
    }

    // Formatting-demo pages run on every startup (idempotent —
    // skips if Welcome page exists in the vault). Decoupled from
    // `TASK_SERVER_SEED` so the demo content stays in sync with
    // renderer changes without requiring users to remember the
    // env var.
    if let Err(e) = seed_formatting_demo_idempotent(&state).await {
        tracing::warn!(?e, "formatting-demo seed failed (non-fatal)");
    }

    let app = router(state);

    info!(%bind, "listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

/// Ensure the org vault doc has an "Org" vault row. Idempotent.
/// Phase 5c demo seeding so the Knowledge route renders something
/// on first paint without needing the CLI.
/// Find the first vault and seed the formatting-demo pages
/// into it. No-op when no vault exists yet (in that case the
/// formatting demo will run from inside `seed_knowledge_org_vault`
/// when `TASK_SERVER_SEED=1`).
async fn seed_formatting_demo_idempotent(state: &AppState) -> eyre::Result<()> {
    use knowledge_proto::VaultRepo;
    let big = project_proto::architect::Page {
        index: 0,
        size: 100,
    };
    let vaults = state
        .vault_repo
        .list(big, None, None)
        .await
        .map_err(|e| eyre::eyre!("list vaults: {e}"))?;
    let Some(v) = vaults.items.first() else {
        info!("no vault yet, skipping formatting-demo seed");
        return Ok(());
    };
    seed_formatting_demo(state, v.id, chrono::Utc::now()).await
}

async fn seed_knowledge_org_vault(state: &AppState) -> eyre::Result<()> {
    use chrono::Utc;
    use knowledge_proto::{PageCreate, PageRepo, VaultCreate, VaultRepo};
    let big = project_proto::architect::Page {
        index: 0,
        size: 1000,
    };
    let vaults = state
        .vault_repo
        .list(big, None, None)
        .await
        .map_err(|e| eyre::eyre!("seed vault list: {e}"))?;
    // Vault-creation gate. If the org vault already exists we
    // skip the project/task seeding (that's one-shot demo data
    // the user has already customized) but still run the
    // formatting-demo seed below — that one is idempotent
    // (presence-checked by page basename) and exists to keep
    // the showcase pages in sync as we add new renderers.
    let vault_id = if let Some(existing) = vaults.items.first() {
        info!(count = vaults.items.len(), "org vault already seeded");
        seed_formatting_demo(state, existing.id, chrono::Utc::now()).await?;
        return Ok(());
    } else {
        uuid::Uuid::nil() // placeholder, replaced below
    };
    let _ = vault_id;
    let v = state
        .vault_repo
        .create(VaultCreate {
            name: "Org".into(),
            root_path: None,
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: "".into(),
            default_view_mode: "source".into(),
            config_json: "{}".into(),
        })
        .await
        .map_err(|e| eyre::eyre!("seed vault create: {e}"))?;
    info!(vault_id = %v.id, "seeded Org vault");

    // Seed two `kind: project` pages + a handful of `kind: task`
    // pages linked to them. Powers the new /projects route which
    // reads from the Knowledge layer (the legacy `Task` entity in
    // project-proto is still seeded for back-compat by
    // task_db::seed).
    let now = Utc::now();
    let projects = [
        ("Website Redesign", "active"),
        ("Album Release", "planning"),
    ];
    for (name, state_val) in projects {
        let fm = serde_json::json!({
            "kind": "project",
            "state": state_val,
        });
        let _ = state
            .page_repo
            .create(PageCreate {
                vault_id: v.id,
                folder_id: None,
                path: format!("{name}.md"),
                basename: name.into(),
                ext: "md".into(),
                aliases: Vec::new(),
                frontmatter_json: fm.to_string(),
                stat_ctime: now,
                stat_mtime: now,
                stat_size: 0,
                is_journal: false,
                journal_day: None,
                shadow_for_kind: None,
                shadow_for_id: None,
            })
            .await
            .map_err(|e| eyre::eyre!("seed project page {name}: {e}"))?;
    }

    let tasks = [
        (
            "Design new color palette",
            "done",
            "high",
            "Website Redesign",
        ),
        (
            "Build component library",
            "in_progress",
            "high",
            "Website Redesign",
        ),
        ("Deploy to staging", "todo", "low", "Website Redesign"),
        ("Master all tracks", "todo", "high", "Album Release"),
        (
            "Design album artwork",
            "in_progress",
            "normal",
            "Album Release",
        ),
    ];
    for (title, status, priority, project) in tasks {
        // `up` lifts the page into the project's subtree in the
        // `up`-tree sidebar (Obsidian VirtFolder convention).
        let fm = serde_json::json!({
            "kind": "task",
            "title": title,
            "status": status,
            "priority": priority,
            "projects": [project],
            "up": [format!("[[{project}]]")],
        });
        let _ = state
            .page_repo
            .create(PageCreate {
                vault_id: v.id,
                folder_id: None,
                path: format!("{title}.md"),
                basename: title.into(),
                ext: "md".into(),
                aliases: Vec::new(),
                frontmatter_json: fm.to_string(),
                stat_ctime: now,
                stat_mtime: now,
                stat_size: 0,
                is_journal: false,
                journal_day: None,
                shadow_for_kind: None,
                shadow_for_id: None,
            })
            .await
            .map_err(|e| eyre::eyre!("seed task page {title}: {e}"))?;
    }
    info!("seeded 2 kind:project + 5 kind:task pages");

    // ── Formatting showcase pages ─────────────────────────────
    //
    // A small set of demo pages exercising every renderer we've
    // shipped. Useful for visual QA, onboarding tours, and as a
    // smoke test that nothing regressed in the BlockView render
    // path. Seeded only on a fresh vault (gated by the early-
    // return above).
    seed_formatting_demo(state, v.id, now).await?;
    Ok(())
}

/// Add seven "Welcome / showcase" pages that exercise every
/// formatting feature in the renderer.
///
/// **Idempotent**: presence-checks by page basename. Safe to
/// re-run on every server startup so the demo content stays in
/// sync with renderer changes without wiping user data.
async fn seed_formatting_demo(
    state: &AppState,
    vault_id: uuid::Uuid,
    now: chrono::DateTime<chrono::Utc>,
) -> eyre::Result<()> {
    use knowledge_proto::{BlockCreate, BlockRepo, PageCreate, PageRepo};

    // Bail when the showcase already exists. The "Welcome" page
    // is our canary — its presence implies the full set was
    // seeded in a previous run.
    let big = project_proto::architect::Page {
        index: 0,
        size: 10_000,
    };
    let existing = state
        .page_repo
        .list(big, None, None)
        .await
        .map_err(|e| eyre::eyre!("seed: list pages: {e}"))?;
    let has_welcome = existing
        .items
        .iter()
        .any(|p| p.vault_id == vault_id && p.basename == "Welcome");
    if has_welcome {
        info!("formatting-demo pages already present, skipping");
        return Ok(());
    }
    info!("seeding 7 formatting-demo pages");

    // Helper to mint one page + return its id.
    async fn mk_page(
        state: &AppState,
        vault_id: uuid::Uuid,
        now: chrono::DateTime<chrono::Utc>,
        name: &str,
    ) -> eyre::Result<uuid::Uuid> {
        let p = state
            .page_repo
            .create(PageCreate {
                vault_id,
                folder_id: None,
                path: format!("{name}.md"),
                basename: name.into(),
                ext: "md".into(),
                aliases: Vec::new(),
                frontmatter_json: "{}".into(),
                stat_ctime: now,
                stat_mtime: now,
                stat_size: 0,
                is_journal: false,
                journal_day: None,
                shadow_for_kind: None,
                shadow_for_id: None,
            })
            .await
            .map_err(|e| eyre::eyre!("seed page {name}: {e}"))?;
        Ok(p.id)
    }

    // Block builder closure — keeps the per-page blob short.
    let mk_block = |vault_id: uuid::Uuid,
                    page_id: uuid::Uuid,
                    sort_idx: usize,
                    kind: &str,
                    content: &str,
                    heading: Option<i32>,
                    code_lang: Option<String>,
                    list_task: Option<String>| BlockCreate {
        vault_id,
        page_id,
        parent_block_id: None,
        sort_key: format!("a{sort_idx:04}"),
        kind: kind.into(),
        content: content.into(),
        heading_level: heading,
        list_ordered: false,
        list_task,
        code_lang,
        callout_kind: None,
        callout_foldable: false,
        properties_json: "{}".into(),
        obsidian_block_id: None,
        collapsed: false,
        refs_json: "[]".into(),
        canvas_node_json: None,
    };

    // ── Page 1: Welcome ───────────────────────────────────────
    let welcome = mk_page(state, vault_id, now, "Welcome").await?;
    let welcome_blocks: Vec<BlockCreate> = vec![
        mk_block(
            vault_id,
            welcome,
            0,
            "heading",
            "Welcome to Task Architect",
            Some(1),
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            1,
            "paragraph",
            "This vault is a tour of every formatting feature the editor ships with. Click any block to start editing — content is local-first and persists to IndexedDB, so refresh is safe.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            2,
            "paragraph",
            "Jump straight in:",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            3,
            "list_item",
            "[[Inline formatting]] — bold, italic, strikethrough, highlight, code, links",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            4,
            "list_item",
            "[[Headings and structure]] — h1-h6, lists, task checkboxes",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            5,
            "list_item",
            "[[Code blocks]] — syntax highlighting via syntect",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            6,
            "list_item",
            "[[Callouts]] — Obsidian-style alert boxes",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            7,
            "list_item",
            "[[Tables]] — GFM-style with column alignment",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            8,
            "list_item",
            "[[Footnotes]] — inline refs + block-level defs",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            welcome,
            9,
            "paragraph",
            "Type `/` in any block to open the command palette. Press `Esc` to enter Normal mode for vim navigation.",
            None,
            None,
            None,
        ),
    ];

    // ── Page 2: Inline formatting ─────────────────────────────
    let inline = mk_page(state, vault_id, now, "Inline formatting").await?;
    let inline_blocks: Vec<BlockCreate> = vec![
        mk_block(
            vault_id,
            inline,
            0,
            "heading",
            "Inline formatting",
            Some(1),
            None,
            None,
        ),
        mk_block(
            vault_id,
            inline,
            1,
            "paragraph",
            "**Bold text** and *italic text* and ***bold italic***.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            inline,
            2,
            "paragraph",
            "Inline `code spans` use backticks. ~~Strikethrough~~ for removed text. ==Highlights== for emphasis (Obsidian-flavored).",
            None,
            None,
            None,
        ),
        mk_block(vault_id, inline, 3, "heading", "Links", Some(2), None, None),
        mk_block(
            vault_id,
            inline,
            4,
            "paragraph",
            "Internal: [[Welcome]] or aliased [[Welcome|the home page]]. Broken: [[Nonexistent Page]] renders as a broken link.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            inline,
            5,
            "paragraph",
            "External: [GitHub](https://github.com) opens in a new tab.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            inline,
            6,
            "heading",
            "Tags + footnote refs",
            Some(2),
            None,
            None,
        ),
        mk_block(
            vault_id,
            inline,
            7,
            "paragraph",
            "Hashtags like #demo and #area/work get tag pages auto-generated. Footnote refs[^1] anchor to definitions below.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            inline,
            8,
            "paragraph",
            "[^1]: This is a footnote definition. Body text re-runs through the inline parser so **emphasis** and [[wikilinks]] nest.",
            None,
            None,
            None,
        ),
    ];

    // ── Page 3: Headings and structure ────────────────────────
    let structure = mk_page(state, vault_id, now, "Headings and structure").await?;
    let structure_blocks: Vec<BlockCreate> = vec![
        mk_block(
            vault_id,
            structure,
            0,
            "heading",
            "Headings and structure",
            Some(1),
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            1,
            "heading",
            "Heading 2",
            Some(2),
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            2,
            "heading",
            "Heading 3",
            Some(3),
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            3,
            "heading",
            "Heading 4",
            Some(4),
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            4,
            "heading",
            "Heading 5",
            Some(5),
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            5,
            "heading",
            "Heading 6",
            Some(6),
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            6,
            "heading",
            "Lists",
            Some(2),
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            7,
            "list_item",
            "A plain bullet item",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            8,
            "list_item",
            "Another one with **bold** and `code`",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            9,
            "heading",
            "Tasks",
            Some(2),
            None,
            None,
        ),
        mk_block(
            vault_id,
            structure,
            10,
            "list_item",
            "Unchecked task",
            None,
            None,
            Some(" ".into()),
        ),
        mk_block(
            vault_id,
            structure,
            11,
            "list_item",
            "In-progress task",
            None,
            None,
            Some("/".into()),
        ),
        mk_block(
            vault_id,
            structure,
            12,
            "list_item",
            "Completed task",
            None,
            None,
            Some("x".into()),
        ),
    ];

    // ── Page 4: Code blocks ───────────────────────────────────
    let code = mk_page(state, vault_id, now, "Code blocks").await?;
    let rust_sample = "fn main() {\n    let greeting = \"Hello, world!\";\n    println!(\"{greeting}\");\n    for i in 0..5 {\n        println!(\"  iteration {i}\");\n    }\n}\n";
    let ts_sample = "interface User {\n  id: string;\n  name: string;\n}\n\nasync function loadUser(id: string): Promise<User> {\n  const res = await fetch(`/api/users/${id}`);\n  return res.json();\n}\n";
    let py_sample = "from dataclasses import dataclass\n\n@dataclass\nclass Note:\n    title: str\n    body: str\n    tags: list[str]\n\nnote = Note(title=\"Hi\", body=\"world\", tags=[\"demo\"])\nprint(note)\n";
    let code_blocks: Vec<BlockCreate> = vec![
        mk_block(
            vault_id,
            code,
            0,
            "heading",
            "Code blocks",
            Some(1),
            None,
            None,
        ),
        mk_block(
            vault_id,
            code,
            1,
            "paragraph",
            "Server-rendered syntax highlighting via syntect. The language label appears in the header chip.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            code,
            2,
            "code",
            rust_sample,
            None,
            Some("rust".into()),
            None,
        ),
        mk_block(
            vault_id,
            code,
            3,
            "code",
            ts_sample,
            None,
            Some("typescript".into()),
            None,
        ),
        mk_block(
            vault_id,
            code,
            4,
            "code",
            py_sample,
            None,
            Some("python".into()),
            None,
        ),
        mk_block(
            vault_id,
            code,
            5,
            "paragraph",
            "Plain text code (no `code_lang` set):",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            code,
            6,
            "code",
            "just a plain block of text\nwith multiple lines\n",
            None,
            None,
            None,
        ),
    ];

    // ── Page 5: Callouts ──────────────────────────────────────
    let callouts = mk_page(state, vault_id, now, "Callouts").await?;
    let callout_blocks: Vec<BlockCreate> = vec![
        mk_block(
            vault_id,
            callouts,
            0,
            "heading",
            "Callouts",
            Some(1),
            None,
            None,
        ),
        mk_block(
            vault_id,
            callouts,
            1,
            "paragraph",
            "Obsidian-style. Each callout renders as a colored card with a kind-aware icon. Body lines re-parse through the inline renderer so **emphasis** and [[wikilinks]] nest.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            callouts,
            2,
            "paragraph",
            "> [!note] Note\n> A note callout for general background info. Subtle blue accent.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            callouts,
            3,
            "paragraph",
            "> [!tip] Tip\n> A tip — use these for **best practices** and shortcuts.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            callouts,
            4,
            "paragraph",
            "> [!warning] Heads up\n> A warning. Use sparingly so it keeps its weight.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            callouts,
            5,
            "paragraph",
            "> [!danger] Danger\n> The strongest alert. Reserve for irreversible actions.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            callouts,
            6,
            "paragraph",
            "> [!success] Success\n> Confirmation that something completed.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            callouts,
            7,
            "paragraph",
            "> [!question] FAQ\n> What does this look like with a question?",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            callouts,
            8,
            "paragraph",
            "> [!quote] Quote\n> A quote/citation block. Renders italic, muted.",
            None,
            None,
            None,
        ),
    ];

    // ── Page 6: Tables ────────────────────────────────────────
    let tables = mk_page(state, vault_id, now, "Tables").await?;
    let table_md = "| Feature      | Status     | Notes                                    |\n|:-------------|:----------:|-----------------------------------------:|\n| Headings     | shipped    | h1 through h6                            |\n| Lists        | shipped    | plain + task with `[ ]` / `[/]` / `[x]`  |\n| Wikilinks    | shipped    | `[[Page]]` + `[[Page\\|Alias]]`          |\n| Callouts     | shipped    | 11 kinds + synonyms                      |\n| Tables       | shipped    | with column alignment via `:--`          |\n| Footnotes    | shipped    | inline ref + block-level def             |\n";
    let table_blocks: Vec<BlockCreate> = vec![
        mk_block(
            vault_id,
            tables,
            0,
            "heading",
            "Tables",
            Some(1),
            None,
            None,
        ),
        mk_block(
            vault_id,
            tables,
            1,
            "paragraph",
            "GFM-style tables. The divider row controls per-column alignment — `:--` left, `:--:` center, `--:` right.",
            None,
            None,
            None,
        ),
        mk_block(vault_id, tables, 2, "paragraph", table_md, None, None, None),
    ];

    // ── Page 7: Footnotes ─────────────────────────────────────
    let footnotes = mk_page(state, vault_id, now, "Footnotes").await?;
    let footnote_blocks: Vec<BlockCreate> = vec![
        mk_block(
            vault_id,
            footnotes,
            0,
            "heading",
            "Footnotes",
            Some(1),
            None,
            None,
        ),
        mk_block(
            vault_id,
            footnotes,
            1,
            "paragraph",
            "Inline reference syntax: `[^id]`. The renderer turns it into a superscript link[^claim] that jumps to the matching definition block below.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            footnotes,
            2,
            "paragraph",
            "You can have multiple refs[^one][^two] in a row, or scatter them through the text[^one] and reuse the same id.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            footnotes,
            3,
            "heading",
            "Definitions",
            Some(2),
            None,
            None,
        ),
        mk_block(
            vault_id,
            footnotes,
            4,
            "paragraph",
            "[^claim]: A footnote definition block. The leading `[^id]:` marks it as a definition; the body text supports the full inline syntax.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            footnotes,
            5,
            "paragraph",
            "[^one]: First definition.",
            None,
            None,
            None,
        ),
        mk_block(
            vault_id,
            footnotes,
            6,
            "paragraph",
            "[^two]: Second definition.",
            None,
            None,
            None,
        ),
    ];

    let all_blocks = welcome_blocks
        .into_iter()
        .chain(inline_blocks)
        .chain(structure_blocks)
        .chain(code_blocks)
        .chain(callout_blocks)
        .chain(table_blocks)
        .chain(footnote_blocks);
    let mut count = 0usize;
    for b in all_blocks {
        state
            .block_repo
            .create(b)
            .await
            .map_err(|e| eyre::eyre!("seed block: {e}"))?;
        count += 1;
    }
    info!(blocks = count, "seeded 7 formatting-showcase pages");
    Ok(())
}

fn env_truthy(key: &str) -> bool {
    matches!(
        std::env::var(key).ok().as_deref(),
        Some("1") | Some("true") | Some("TRUE") | Some("yes")
    )
}
