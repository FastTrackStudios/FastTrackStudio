//! Tier 2: PagePropEdge integration tests. Verifies that
//! `IndexedPageRepo` keeps the materialized page-property index
//! in sync with `Page.frontmatter_json`.

use chrono::Utc;
use crdt::CrdtDoc;
use knowledge_crdt::{IndexedPageRepo, VaultRepoLoro, reindex::find_pages_with_prop};
use knowledge_proto::{PageCreate, PageRepo, PageUpdate, VaultCreate, VaultRepo};
use uuid::Uuid;

async fn seed_vault(doc: &CrdtDoc) -> Uuid {
    let v = VaultRepoLoro::new(doc)
        .create(VaultCreate {
            name: "test".into(),
            root_path: None,
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: String::new(),
            default_view_mode: "live-preview".into(),
            config_json: "{}".into(),
        })
        .await
        .expect("vault");
    v.id
}

fn make_page(vault_id: Uuid, basename: &str, frontmatter_json: &str) -> PageCreate {
    let now = Utc::now();
    PageCreate {
        vault_id,
        folder_id: None,
        path: format!("{basename}.md"),
        basename: basename.into(),
        ext: "md".into(),
        aliases: Vec::new(),
        frontmatter_json: frontmatter_json.into(),
        stat_ctime: now,
        stat_mtime: now,
        stat_size: 0,
        is_journal: false,
        journal_day: None,
        shadow_for_kind: None,
        shadow_for_id: None,
    }
}

#[tokio::test(flavor = "current_thread")]
async fn page_prop_edges_index_kind() {
    let doc = CrdtDoc::ephemeral();
    let vault_id = seed_vault(&doc).await;
    let pages = IndexedPageRepo::new(&doc);

    pages
        .create(make_page(
            vault_id,
            "Project A",
            r#"{"kind":"project","state":"active"}"#,
        ))
        .await
        .expect("create");
    pages
        .create(make_page(
            vault_id,
            "Task 1",
            r#"{"kind":"task","status":"todo"}"#,
        ))
        .await
        .expect("create");
    pages
        .create(make_page(
            vault_id,
            "Task 2",
            r#"{"kind":"task","status":"in_progress"}"#,
        ))
        .await
        .expect("create");

    let projects = find_pages_with_prop(&doc, "kind", Some("\"project\""))
        .await
        .expect("project query");
    assert_eq!(projects.len(), 1);

    let tasks = find_pages_with_prop(&doc, "kind", Some("\"task\""))
        .await
        .expect("task query");
    assert_eq!(tasks.len(), 2);

    let in_progress = find_pages_with_prop(&doc, "status", Some("\"in_progress\""))
        .await
        .expect("status query");
    assert_eq!(in_progress.len(), 1);
}

#[tokio::test(flavor = "current_thread")]
async fn page_prop_edges_track_updates() {
    let doc = CrdtDoc::ephemeral();
    let vault_id = seed_vault(&doc).await;
    let pages = IndexedPageRepo::new(&doc);
    let p = pages
        .create(make_page(
            vault_id,
            "T",
            r#"{"kind":"task","status":"todo"}"#,
        ))
        .await
        .expect("create");

    pages
        .update(
            p.id,
            PageUpdate {
                frontmatter_json: Some(r#"{"kind":"task","status":"done"}"#.into()),
                ..Default::default()
            },
        )
        .await
        .expect("update");

    assert!(
        find_pages_with_prop(&doc, "status", Some("\"todo\""))
            .await
            .unwrap()
            .is_empty()
    );
    assert_eq!(
        find_pages_with_prop(&doc, "status", Some("\"done\""))
            .await
            .unwrap()
            .len(),
        1
    );
}

#[tokio::test(flavor = "current_thread")]
async fn page_delete_cascades_edges() {
    let doc = CrdtDoc::ephemeral();
    let vault_id = seed_vault(&doc).await;
    let pages = IndexedPageRepo::new(&doc);
    let p = pages
        .create(make_page(vault_id, "Goner", r#"{"kind":"task"}"#))
        .await
        .expect("create");
    assert_eq!(
        find_pages_with_prop(&doc, "kind", Some("\"task\""))
            .await
            .unwrap()
            .len(),
        1
    );
    pages.delete(p.id).await.expect("delete");
    assert!(
        find_pages_with_prop(&doc, "kind", None)
            .await
            .unwrap()
            .is_empty()
    );
}

#[tokio::test(flavor = "current_thread")]
async fn declared_props_carry_value_type() {
    // `kind: task`'s `priority` property is declared as
    // EnumWithMetadata in the builtin registry — the edge should
    // record its canonical type tag.
    let doc = CrdtDoc::ephemeral();
    let vault_id = seed_vault(&doc).await;
    let pages = IndexedPageRepo::new(&doc);
    pages
        .create(make_page(
            vault_id,
            "Typed",
            r#"{"kind":"task","priority":"high","ad_hoc":42}"#,
        ))
        .await
        .expect("create");

    let priority = find_pages_with_prop(&doc, "priority", None)
        .await
        .expect("priority");
    assert_eq!(priority.len(), 1);
    assert_eq!(priority[0].value_type, "enum_with_metadata");

    // Undeclared property → empty type tag.
    let ad_hoc = find_pages_with_prop(&doc, "ad_hoc", None).await.unwrap();
    assert_eq!(ad_hoc.len(), 1);
    assert_eq!(ad_hoc[0].value_type, "");
}
