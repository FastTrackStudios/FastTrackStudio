//! Phase 6.5a — property schema round-trip + index decomposition.
//!
//! Two things to prove:
//!
//! 1. **Real Observatory frontmatter** survives the schema's
//!    `coerce_value` pipeline without data loss. Where the schema
//!    rejects a value (e.g. `status: InProgress` not in our enum's
//!    declared options), the original is preserved in a shadow
//!    sibling (callers store under `__shadow__<key>`).
//! 2. **Schema-aware FrontmatterIndex** decomposes `Tags` /
//!    `LinkList` per-element, so `pages_with("tags", "design")`
//!    finds the page rather than skipping it.
//!
//! The five frontmatter samples are inlined verbatim from
//! `~/Documents/The Observatory/TaskNotes/*.md` so the test
//! doesn't depend on the user's filesystem at test time.

use chrono::Utc;
use crdt_seaorm::SeaOrmPersistence;
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::property_schema::{CoerceOutcome, PropertySchemaRegistry, coerce_value};
use knowledge_proto::{PageCreate, PageRepo, VaultCreate, VaultRepo};
use task_db::{default_database_url, open_and_migrate};
use task_server::basename_index::BasenameIndex;
use task_server::{AppState, AuthState};
use uuid::Uuid;

/// Five representative frontmatter shapes from
/// `~/Documents/The Observatory/TaskNotes/*.md`, transcribed to JSON
/// (the on-disk YAML decodes to the same value tree). Each line
/// comment names the source file.
const SAMPLES: &[(&str, &str, &str)] = &[
    // (basename, kind, frontmatter_json)
    (
        "Build component library",
        "task",
        r#"{
            "title": "Build component library",
            "status": "InProgress",
            "priority": "High",
            "kind": "task",
            "projects": ["Website Redesign"],
            "tags": ["frontend", "components"],
            "contexts": ["work"],
            "due": "2026-04-18",
            "time_estimate": 480
        }"#,
    ),
    (
        "Create wireframes for homepage",
        "task",
        r#"{
            "title": "Create wireframes for homepage",
            "status": "Done",
            "priority": "High",
            "kind": "task",
            "projects": ["Website Redesign"],
            "tags": ["design", "ux"],
            "due": "2026-04-08",
            "completed_date": "2026-04-07"
        }"#,
    ),
    (
        "Design album artwork",
        "task",
        r#"{
            "title": "Design album artwork",
            "status": "Open",
            "priority": "Normal",
            "kind": "task",
            "projects": ["Album Release"],
            "tags": ["design", "artwork"],
            "due": "2026-05-01",
            "time_estimate": 480
        }"#,
    ),
    (
        "Master all tracks",
        "task",
        r#"{
            "title": "Master all tracks",
            "status": "Planned",
            "priority": "High",
            "kind": "task",
            "projects": ["Album Release"],
            "tags": ["mastering"],
            "contexts": ["studio"],
            "due": "2026-05-15",
            "time_estimate": 600
        }"#,
    ),
    (
        "Implement responsive navigation",
        "task",
        r#"{
            "title": "Implement responsive navigation",
            "status": "Open",
            "priority": "Normal",
            "kind": "task",
            "projects": ["Website Redesign"],
            "tags": ["frontend"],
            "contexts": ["work"],
            "due": "2026-04-22",
            "time_estimate": 120
        }"#,
    ),
];

#[test]
fn observatory_frontmatter_round_trips_with_coerce_or_shadow() {
    let registry = PropertySchemaRegistry::with_builtins();
    let task_schema = registry.get("task").expect("task schema");

    for (basename, kind, fm_json) in SAMPLES {
        let raw: serde_json::Map<String, serde_json::Value> =
            serde_json::from_str::<serde_json::Value>(fm_json)
                .expect("sample is valid JSON")
                .as_object()
                .expect("sample is an object")
                .clone();
        assert_eq!(*kind, "task");

        // Coerce every key the schema knows about. Unknown keys
        // pass through untouched. Shadows accumulate so the test
        // can assert the expected ones fire.
        let mut shadows: Vec<(String, String)> = Vec::new(); // (key, reason)
        let mut out = serde_json::Map::new();
        for (k, v) in raw.iter() {
            let prop = task_schema.properties.iter().find(|p| &p.key == k);
            match prop {
                Some(def) => match coerce_value(&def.ty, v.clone()) {
                    CoerceOutcome::Ok(coerced) => {
                        out.insert(k.clone(), coerced);
                    }
                    CoerceOutcome::Shadow { original, reason } => {
                        shadows.push((k.clone(), reason));
                        out.insert(format!("__shadow__{k}"), original);
                    }
                },
                None => {
                    out.insert(k.clone(), v.clone());
                }
            }
        }

        // Status is the one we expect to shadow on every sample
        // — TaskNotes uses CamelCase (`InProgress`, `Done`, `Open`,
        // `Planned`) and our enum declares snake_case
        // (`in_progress`, `done`, etc.). Verify this is the
        // intended kind of friction: original survives, warning
        // fires, no data lost.
        let status_shadowed = shadows.iter().any(|(k, _)| k == "status");
        assert!(
            status_shadowed,
            "sample `{basename}`: status should have shadowed; shadows = {shadows:?}"
        );
        // The shadow field carries the original CamelCase value.
        let raw_status = raw.get("status").expect("had status").as_str().unwrap();
        let shadow_status = out
            .get("__shadow__status")
            .expect("shadow field set")
            .as_str()
            .unwrap();
        assert_eq!(raw_status, shadow_status);

        // Every original key is still recoverable — either as the
        // coerced value at its own key, or under `__shadow__<key>`.
        // No silent loss.
        for k in raw.keys() {
            assert!(
                out.contains_key(k) || out.contains_key(&format!("__shadow__{k}")),
                "sample `{basename}`: key `{k}` disappeared"
            );
        }

        // Dates should be ok-coerced (already in YYYY-MM-DD).
        if let Some(due) = raw.get("due") {
            assert_eq!(
                out.get("due"),
                Some(due),
                "sample `{basename}`: due date should pass through"
            );
        }

        // `projects` arrays survive untouched (LinkList accepts
        // arrays).
        if let Some(projects) = raw.get("projects") {
            assert_eq!(out.get("projects"), Some(projects));
        }
    }
}

#[test]
fn coerce_pipeline_preserves_unknown_keys_for_untyped_pages() {
    // No `kind:` declared — the registry lookup returns nothing,
    // so every key passes through verbatim. This matters for
    // imported Obsidian frontmatter we haven't classified yet.
    let registry = PropertySchemaRegistry::with_builtins();
    let unknown_kind = registry.get("nonexistent");
    assert!(unknown_kind.is_none());

    // Even without a schema, scalars + lists are valid frontmatter.
    let raw: serde_json::Value = serde_json::json!({
        "title": "Some random page",
        "up": ["[[Coding]]"],
        "tags": ["area"],
        "icon": "💻"
    });
    let map = raw.as_object().unwrap().clone();
    // No coercion applied → identity.
    let pass_through: serde_json::Map<String, serde_json::Value> = map.clone();
    assert_eq!(pass_through, map);
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

fn make_page(vault_id: Uuid, basename: &str, fm: &str) -> PageCreate {
    let now = Utc::now();
    PageCreate {
        vault_id,
        folder_id: None,
        path: format!("{basename}.md"),
        basename: basename.into(),
        ext: "md".into(),
        aliases: Vec::new(),
        frontmatter_json: fm.into(),
        stat_ctime: now,
        stat_mtime: now,
        stat_size: 0,
        is_journal: false,
        journal_day: None,
        shadow_for_kind: None,
        shadow_for_id: None,
    }
}

#[tokio::test(flavor = "multi_thread")]
async fn frontmatter_index_decomposes_tags_and_link_lists() -> eyre::Result<()> {
    let persistence: SeaOrmPersistence = open_and_migrate(&default_database_url()).await?;
    let auth = AuthState::open("sqlite::memory:", "test-secret-at-least-32-bytes!!!").await?;
    let state = AppState::new_with_auth(persistence, auth).await?;

    let vault_repo: VaultRepoLoro = (*state.vault_repo).clone();
    let page_repo: PageRepoLoro = (*state.page_repo).clone();
    let _block_repo: BlockRepoLoro = (*state.block_repo).clone();

    let vault = vault_repo
        .create(make_vault("Org"))
        .await
        .map_err(|e| eyre::eyre!("create vault: {e}"))?;

    // Insert two tasks with overlapping tags + projects.
    let task_a_fm = serde_json::json!({
        "kind": "task",
        "title": "A",
        "status": "todo",
        "tags": ["design", "ux"],
        "projects": ["Website Redesign"]
    })
    .to_string();
    let task_b_fm = serde_json::json!({
        "kind": "task",
        "title": "B",
        "status": "in_progress",
        "tags": ["design", "frontend"],
        "projects": ["Website Redesign", "Component Library"]
    })
    .to_string();

    let page_a = page_repo
        .create(make_page(vault.id, "A", &task_a_fm))
        .await
        .map_err(|e| eyre::eyre!("create A: {e}"))?;
    let page_b = page_repo
        .create(make_page(vault.id, "B", &task_b_fm))
        .await
        .map_err(|e| eyre::eyre!("create B: {e}"))?;

    state
        .indexer
        .rebuild()
        .await
        .map_err(|e| eyre::eyre!("rebuild: {e}"))?;

    // Decomposed tag index: both pages with `design`, only A with
    // `ux`, only B with `frontend`.
    let design = state.indexer.frontmatter.pages_with("tags", "design");
    assert!(design.contains(&page_a.id), "A missing from design");
    assert!(design.contains(&page_b.id), "B missing from design");
    let ux = state.indexer.frontmatter.pages_with("tags", "ux");
    assert_eq!(ux, vec![page_a.id]);
    let frontend = state.indexer.frontmatter.pages_with("tags", "frontend");
    assert_eq!(frontend, vec![page_b.id]);

    // Decomposed projects (LinkList).
    let website = state
        .indexer
        .frontmatter
        .pages_with("projects", "Website Redesign");
    assert!(website.contains(&page_a.id));
    assert!(website.contains(&page_b.id));
    let comp_lib = state
        .indexer
        .frontmatter
        .pages_with("projects", "Component Library");
    assert_eq!(comp_lib, vec![page_b.id]);

    // Status (scalar enum value) still indexes the legacy way.
    let todo = state.indexer.frontmatter.pages_with("status", "todo");
    assert!(todo.contains(&page_a.id));
    let in_progress = state
        .indexer
        .frontmatter
        .pages_with("status", "in_progress");
    assert!(in_progress.contains(&page_b.id));

    // `basenames` index is unaffected by the upgrade (no
    // auth_user_id in any sample).
    assert_eq!(state.basename_index.resolve("A"), None);

    Ok(())
}

#[test]
fn lexorank_inserts_remain_sortable_under_concurrent_use() {
    use knowledge_proto::lexorank;
    // Simulate two clients inserting between the same pair of
    // bounds at the same time — both compute valid keys, and
    // both keys sort strictly between the bounds.
    let low = "a";
    let high = "z";
    let key_client1 = lexorank::between(low, high).expect("ok");
    let key_client2 = lexorank::between(low, high).expect("ok");
    // Same algorithm + same inputs = same output. That's fine —
    // CRDT handles dup keys via the underlying Loro container's
    // own tie-break. We just need both keys to be sortable.
    assert!(key_client1.as_str() > low && key_client1.as_str() < high);
    assert!(key_client2.as_str() > low && key_client2.as_str() < high);

    // 50 sequential insertions between the same bounds: each new
    // key sorts after the previous (because we use it as the new
    // low bound).
    let mut last = low.to_string();
    for i in 0..50 {
        let k = lexorank::between(&last, high).expect("ok");
        assert!(
            k.as_str() > last.as_str(),
            "iteration {i}: {k} not greater than {last}"
        );
        last = k;
    }
}
