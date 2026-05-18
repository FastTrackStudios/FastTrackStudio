//! Tier 3 — first-class ref entities. Verifies that:
//! - Renaming a page cascades to every BlockRefEdge that linked to it.
//! - Creating a page that was previously linked-to (but missing)
//!   auto-resolves the dangling refs.
//! - Deleting a page leaves the refs intact but unresolves them
//!   (broken-link state).
//! - `find_broken_links` returns the unresolved set.

use chrono::Utc;
use crdt::CrdtDoc;
use knowledge_crdt::{
    BlockRepoLoro, IndexedPageRepo, VaultRepoLoro,
    reindex::{find_backlinks, find_broken_links},
};
use knowledge_proto::{
    BlockCreate, BlockRepo, PageCreate, PageRepo, PageUpdate, Ref, VaultCreate, VaultRepo,
    refs::LinkRef,
};
use uuid::Uuid;

fn refs_json(refs: &[Ref]) -> String {
    serde_json::to_string(refs).expect("encode refs")
}

fn link(target: &str) -> Ref {
    Ref::Link(LinkRef {
        target_linkpath: target.into(),
        heading: None,
        block_id: None,
        alias: None,
        original: format!("[[{target}]]"),
    })
}

async fn seed(doc: &CrdtDoc) -> (Uuid, Uuid, Uuid) {
    let v = VaultRepoLoro::new(doc)
        .create(VaultCreate {
            name: "t".into(),
            root_path: None,
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: String::new(),
            default_view_mode: "live-preview".into(),
            config_json: "{}".into(),
        })
        .await
        .unwrap();
    let pages = IndexedPageRepo::new(doc);
    let now = Utc::now();
    let mk = |basename: &'static str| PageCreate {
        vault_id: v.id,
        folder_id: None,
        path: format!("{basename}.md"),
        basename: basename.into(),
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
    };
    let foo = pages.create(mk("Foo")).await.unwrap();
    let source = pages.create(mk("Source")).await.unwrap();
    (v.id, foo.id, source.id)
}

fn block_with_link(vault_id: Uuid, page_id: Uuid, target: &str) -> BlockCreate {
    BlockCreate {
        vault_id,
        page_id,
        parent_block_id: None,
        sort_key: "m".into(),
        kind: "paragraph".into(),
        content: format!("see [[{target}]]"),
        heading_level: None,
        list_ordered: false,
        list_task: None,
        code_lang: None,
        callout_kind: None,
        callout_foldable: false,
        properties_json: "{}".into(),
        obsidian_block_id: None,
        collapsed: false,
        refs_json: refs_json(&[link(target)]),
        canvas_node_json: None,
    }
}

#[tokio::test(flavor = "current_thread")]
async fn rename_cascades_to_existing_refs() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, foo_id, source_id) = seed(&doc).await;
    BlockRepoLoro::new(&doc)
        .create(block_with_link(vault_id, source_id, "Foo"))
        .await
        .unwrap();

    // Sanity: backlink exists for "Foo".
    assert_eq!(find_backlinks(&doc, "Foo").await.unwrap().len(), 1);

    // Rename Foo -> NewFoo.
    let pages = IndexedPageRepo::new(&doc);
    pages
        .update(
            foo_id,
            PageUpdate {
                basename: Some("NewFoo".into()),
                ..Default::default()
            },
        )
        .await
        .unwrap();

    // Old basename has no backlinks; new basename does.
    assert!(find_backlinks(&doc, "Foo").await.unwrap().is_empty());
    let after = find_backlinks(&doc, "NewFoo").await.unwrap();
    assert_eq!(after.len(), 1);
    assert_eq!(after[0].target_uuid, Some(foo_id));
}

#[tokio::test(flavor = "current_thread")]
async fn page_create_resolves_dangling_refs() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, _foo_id, source_id) = seed(&doc).await;
    // Create a block that references "Bar" (page doesn't exist yet).
    BlockRepoLoro::new(&doc)
        .create(block_with_link(vault_id, source_id, "Bar"))
        .await
        .unwrap();

    let broken = find_broken_links(&doc).await.unwrap();
    assert_eq!(broken.len(), 1);
    assert_eq!(broken[0].target_str, "Bar");
    assert!(broken[0].target_uuid.is_none());

    // Now create Bar.
    let pages = IndexedPageRepo::new(&doc);
    let now = Utc::now();
    let bar = pages
        .create(PageCreate {
            vault_id,
            folder_id: None,
            path: "Bar.md".into(),
            basename: "Bar".into(),
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
        .unwrap();

    // Broken set is empty; the edge now resolves to bar.id.
    assert!(find_broken_links(&doc).await.unwrap().is_empty());
    let resolved = find_backlinks(&doc, "Bar").await.unwrap();
    assert_eq!(resolved.len(), 1);
    assert_eq!(resolved[0].target_uuid, Some(bar.id));
}

#[tokio::test(flavor = "current_thread")]
async fn aliases_resolve_dangling_refs() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, _foo_id, source_id) = seed(&doc).await;
    // Block references "Old Name" — page doesn't exist yet.
    BlockRepoLoro::new(&doc)
        .create(block_with_link(vault_id, source_id, "Old Name"))
        .await
        .unwrap();
    assert_eq!(find_broken_links(&doc).await.unwrap().len(), 1);

    // Create a page whose basename is different but whose
    // aliases include "Old Name" — should auto-resolve.
    let pages = IndexedPageRepo::new(&doc);
    let now = Utc::now();
    let renamed = pages
        .create(PageCreate {
            vault_id,
            folder_id: None,
            path: "Renamed.md".into(),
            basename: "Renamed".into(),
            ext: "md".into(),
            aliases: vec!["Old Name".into()],
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
        .unwrap();

    assert!(find_broken_links(&doc).await.unwrap().is_empty());
    let resolved = find_backlinks(&doc, "Old Name").await.unwrap();
    assert_eq!(resolved.len(), 1);
    assert_eq!(resolved[0].target_uuid, Some(renamed.id));
}

#[tokio::test(flavor = "current_thread")]
async fn alias_added_via_update_resolves_refs() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, foo_id, source_id) = seed(&doc).await;
    // Block references "Nickname" — page doesn't exist.
    BlockRepoLoro::new(&doc)
        .create(block_with_link(vault_id, source_id, "Nickname"))
        .await
        .unwrap();
    assert_eq!(find_broken_links(&doc).await.unwrap().len(), 1);

    // Add Nickname as an alias to Foo via update.
    IndexedPageRepo::new(&doc)
        .update(
            foo_id,
            knowledge_proto::PageUpdate {
                aliases: Some(vec!["Nickname".into()]),
                ..Default::default()
            },
        )
        .await
        .unwrap();

    assert!(find_broken_links(&doc).await.unwrap().is_empty());
    let resolved = find_backlinks(&doc, "Nickname").await.unwrap();
    assert_eq!(resolved[0].target_uuid, Some(foo_id));
}

#[tokio::test(flavor = "current_thread")]
async fn delete_unresolves_refs() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, foo_id, source_id) = seed(&doc).await;
    BlockRepoLoro::new(&doc)
        .create(block_with_link(vault_id, source_id, "Foo"))
        .await
        .unwrap();

    let before = find_backlinks(&doc, "Foo").await.unwrap();
    assert_eq!(before[0].target_uuid, Some(foo_id));

    IndexedPageRepo::new(&doc).delete(foo_id).await.unwrap();

    let broken = find_broken_links(&doc).await.unwrap();
    assert_eq!(broken.len(), 1);
    assert_eq!(broken[0].target_str.to_lowercase(), "foo");
    // Edge survives, just unresolved.
    let after = find_backlinks(&doc, "Foo").await.unwrap();
    assert_eq!(after.len(), 1);
    assert!(after[0].target_uuid.is_none());
}
