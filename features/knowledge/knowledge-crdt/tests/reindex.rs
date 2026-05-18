//! Integration tests for the BlockRefEdge / BlockPropEdge
//! materialized-view indices. Spins up an ephemeral `CrdtDoc`,
//! pokes blocks through the standard repo API, and verifies the
//! edges trail the source-of-truth automatically.

use chrono::Utc;
use crdt::CrdtDoc;
use knowledge_crdt::{
    BlockRepoLoro, PageRepoLoro, VaultRepoLoro,
    reindex::{cascade_delete_block_edges, find_backlinks, find_blocks_with_prop, reindex_block},
};
use knowledge_proto::{
    BlockCreate, BlockRefEdgeRepo, BlockRepo, BlockUpdate, PageCreate, PageRepo, Ref, TagRef,
    VaultCreate, VaultRepo, architect, refs::LinkRef,
};
use uuid::Uuid;

fn refs_json(refs: &[Ref]) -> String {
    serde_json::to_string(refs).expect("encode refs")
}

fn link_ref(target: &str) -> Ref {
    Ref::Link(LinkRef {
        target_linkpath: target.into(),
        heading: None,
        block_id: None,
        alias: None,
        original: format!("[[{target}]]"),
    })
}

async fn seed_vault_and_page(doc: &CrdtDoc) -> (Uuid, Uuid) {
    let vault_repo = VaultRepoLoro::new(doc);
    let v = vault_repo
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
    let page_repo = PageRepoLoro::new(doc);
    let now = Utc::now();
    let p = page_repo
        .create(PageCreate {
            vault_id: v.id,
            folder_id: None,
            path: "Foo.md".into(),
            basename: "Foo".into(),
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
        .expect("page");
    (v.id, p.id)
}

fn block_create(
    vault_id: Uuid,
    page_id: Uuid,
    content: &str,
    refs: &[Ref],
    props_json: &str,
) -> BlockCreate {
    BlockCreate {
        vault_id,
        page_id,
        parent_block_id: None,
        sort_key: "m".into(),
        kind: "paragraph".into(),
        content: content.into(),
        heading_level: None,
        list_ordered: false,
        list_task: None,
        code_lang: None,
        callout_kind: None,
        callout_foldable: false,
        properties_json: props_json.into(),
        obsidian_block_id: None,
        collapsed: false,
        refs_json: refs_json(refs),
        canvas_node_json: None,
    }
}

#[tokio::test(flavor = "current_thread")]
async fn ref_edge_created_on_block_create() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, page_id) = seed_vault_and_page(&doc).await;
    let block_repo = BlockRepoLoro::new(&doc);
    let block = block_repo
        .create(block_create(
            vault_id,
            page_id,
            "see [[Bar]]",
            &[link_ref("Bar")],
            "{}",
        ))
        .await
        .expect("create block");

    let backlinks = find_backlinks(&doc, "Bar").await.expect("backlinks");
    assert_eq!(backlinks.len(), 1);
    assert_eq!(backlinks[0].source_block_id, block.id);
    assert_eq!(backlinks[0].target_kind, "page");
    assert_eq!(backlinks[0].target_str, "Bar");
}

#[tokio::test(flavor = "current_thread")]
async fn ref_edge_updates_when_content_changes() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, page_id) = seed_vault_and_page(&doc).await;
    let block_repo = BlockRepoLoro::new(&doc);
    let block = block_repo
        .create(block_create(
            vault_id,
            page_id,
            "[[Bar]]",
            &[link_ref("Bar")],
            "{}",
        ))
        .await
        .expect("create");

    // Change ref to [[Baz]].
    block_repo
        .update(
            block.id,
            BlockUpdate {
                content: Some("[[Baz]]".into()),
                refs_json: Some(refs_json(&[link_ref("Baz")])),
                ..Default::default()
            },
        )
        .await
        .expect("update");

    assert!(find_backlinks(&doc, "Bar").await.unwrap().is_empty());
    let baz = find_backlinks(&doc, "Baz").await.unwrap();
    assert_eq!(baz.len(), 1);
}

#[tokio::test(flavor = "current_thread")]
async fn cascade_delete_removes_edges() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, page_id) = seed_vault_and_page(&doc).await;
    let block_repo = BlockRepoLoro::new(&doc);
    let block = block_repo
        .create(block_create(
            vault_id,
            page_id,
            "[[Bar]] [[Baz]]",
            &[link_ref("Bar"), link_ref("Baz")],
            "{}",
        ))
        .await
        .expect("create");
    assert_eq!(find_backlinks(&doc, "Bar").await.unwrap().len(), 1);
    assert_eq!(find_backlinks(&doc, "Baz").await.unwrap().len(), 1);

    block_repo.delete(block.id).await.expect("delete");
    assert!(find_backlinks(&doc, "Bar").await.unwrap().is_empty());
    assert!(find_backlinks(&doc, "Baz").await.unwrap().is_empty());
}

#[tokio::test(flavor = "current_thread")]
async fn tags_become_ref_edges() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, page_id) = seed_vault_and_page(&doc).await;
    let block_repo = BlockRepoLoro::new(&doc);
    let tag = Ref::Tag(TagRef {
        path: vec!["area".into(), "work".into()],
        original: "#area/work".into(),
    });
    let _ = block_repo
        .create(block_create(
            vault_id,
            page_id,
            "hi #area/work",
            &[tag],
            "{}",
        ))
        .await
        .expect("create");

    use knowledge_crdt::BlockRefEdgeRepoLoro;
    let repo = BlockRefEdgeRepoLoro::new(&doc);
    let list = repo
        .list(
            architect::Page {
                index: 0,
                size: 100_000,
            },
            None,
            None,
        )
        .await
        .expect("list");
    let tag_edge = list
        .items
        .iter()
        .find(|e| e.target_kind == "tag")
        .expect("tag edge present");
    assert_eq!(tag_edge.target_str, "area/work");
}

#[tokio::test(flavor = "current_thread")]
async fn prop_edge_indexed_and_queryable() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, page_id) = seed_vault_and_page(&doc).await;
    let block_repo = BlockRepoLoro::new(&doc);
    let block = block_repo
        .create(block_create(
            vault_id,
            page_id,
            "task content",
            &[],
            r#"{"priority":"high","done":false}"#,
        ))
        .await
        .expect("create");

    let high = find_blocks_with_prop(&doc, "priority", Some("\"high\""))
        .await
        .expect("priority");
    assert_eq!(high.len(), 1);
    assert_eq!(high[0].block_id, block.id);

    let any_done = find_blocks_with_prop(&doc, "done", None)
        .await
        .expect("done");
    assert_eq!(any_done.len(), 1);
    assert_eq!(any_done[0].value_json, "false");

    // Update -> edge follows.
    block_repo
        .update(
            block.id,
            BlockUpdate {
                properties_json: Some(r#"{"priority":"low"}"#.into()),
                ..Default::default()
            },
        )
        .await
        .expect("update");
    let low = find_blocks_with_prop(&doc, "priority", Some("\"low\""))
        .await
        .expect("low");
    assert_eq!(low.len(), 1);
    assert!(
        find_blocks_with_prop(&doc, "priority", Some("\"high\""))
            .await
            .unwrap()
            .is_empty()
    );
    // `done` was dropped from the new properties — edge should
    // be cascaded out by the diff.
    assert!(
        find_blocks_with_prop(&doc, "done", None)
            .await
            .unwrap()
            .is_empty()
    );
}

#[tokio::test(flavor = "current_thread")]
async fn manual_reindex_is_idempotent() {
    let doc = CrdtDoc::ephemeral();
    let (vault_id, page_id) = seed_vault_and_page(&doc).await;
    let block_repo = BlockRepoLoro::new(&doc);
    let block = block_repo
        .create(block_create(
            vault_id,
            page_id,
            "[[Bar]]",
            &[link_ref("Bar")],
            "{}",
        ))
        .await
        .expect("create");

    let before = find_backlinks(&doc, "Bar").await.unwrap().len();
    reindex_block(&doc, block.id).await.expect("reindex");
    reindex_block(&doc, block.id).await.expect("reindex again");
    let after = find_backlinks(&doc, "Bar").await.unwrap().len();
    assert_eq!(before, after);
    assert_eq!(after, 1);

    // Cascade-delete is also idempotent.
    cascade_delete_block_edges(&doc, block.id)
        .await
        .expect("cascade");
    cascade_delete_block_edges(&doc, block.id)
        .await
        .expect("cascade2");
    assert!(find_backlinks(&doc, "Bar").await.unwrap().is_empty());
}
