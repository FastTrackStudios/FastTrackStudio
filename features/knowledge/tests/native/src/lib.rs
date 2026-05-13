//! Native integration tests for the `knowledge` feature. One
//! ephemeral `CrdtDoc` hosts every entity type and proves they
//! coexist + round-trip through the Loro codec.

#![cfg(test)]

use architect::Page as PageWindow;
use chrono::Utc;
use knowledge_crdt::{
    BaseRepoLoro, BlockRepoLoro, CrdtDoc, FolderRepoLoro, KnowledgeTagRepoLoro, PageRepoLoro,
    VaultRepoLoro,
};
use knowledge_proto::{
    BaseCreate, BaseRepo, BlockCreate, BlockRepo, FolderCreate, FolderRepo, KnowledgeTagCreate,
    KnowledgeTagRepo, PageCreate, PageRepo, VaultCreate, VaultRepo,
};
use uuid::Uuid;

fn page() -> PageWindow {
    PageWindow {
        index: 0,
        size: 100,
    }
}

#[tokio::test]
async fn vault_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let repo = VaultRepoLoro::new(&doc);
    let v = repo
        .create(VaultCreate {
            name: "My Vault".into(),
            root_path: Some("/tmp/vault".into()),
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: "".into(),
            default_view_mode: "live-preview".into(),
            config_json: "{}".into(),
        })
        .await
        .unwrap();
    let got = repo.get(v.id).await.unwrap();
    assert_eq!(got.name, "My Vault");
    assert_eq!(got.new_link_format, "shortest");
    assert_eq!(got.root_path.as_deref(), Some("/tmp/vault"));
}

#[tokio::test]
async fn folder_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let folders = FolderRepoLoro::new(&doc);
    let vault_id = Uuid::new_v4();
    let f = folders
        .create(FolderCreate {
            vault_id,
            path: "notes/journal".into(),
            parent_id: None,
        })
        .await
        .unwrap();
    let got = folders.get(f.id).await.unwrap();
    assert_eq!(got.path, "notes/journal");
    assert_eq!(got.vault_id, vault_id);
}

#[tokio::test]
async fn page_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let pages = PageRepoLoro::new(&doc);
    let vault_id = Uuid::new_v4();
    let now = Utc::now();
    let p = pages
        .create(PageCreate {
            vault_id,
            folder_id: None,
            path: "Hello.md".into(),
            basename: "Hello".into(),
            ext: "md".into(),
            aliases: vec!["Greeting".into()],
            frontmatter_json: "[]".into(),
            stat_ctime: now,
            stat_mtime: now,
            stat_size: 12,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: Some("task".into()),
            shadow_for_id: Some(Uuid::new_v4()),
        })
        .await
        .unwrap();
    let got = pages.get(p.id).await.unwrap();
    assert_eq!(got.basename, "Hello");
    assert_eq!(got.aliases, vec!["Greeting".to_string()]);
    assert_eq!(got.shadow_for_kind.as_deref(), Some("task"));
}

#[tokio::test]
async fn block_round_trip_and_list() {
    let doc = CrdtDoc::ephemeral();
    let blocks = BlockRepoLoro::new(&doc);
    let vault_id = Uuid::new_v4();
    let page_id = Uuid::new_v4();
    for (i, key) in ["a0", "a1", "a2"].iter().enumerate() {
        blocks
            .create(BlockCreate {
                vault_id,
                page_id,
                parent_block_id: None,
                sort_key: (*key).into(),
                kind: "paragraph".into(),
                content: format!("block {i}"),
                heading_level: None,
                list_ordered: false,
                list_task: None,
                code_lang: None,
                callout_kind: None,
                callout_foldable: false,
                properties_json: "[]".into(),
                obsidian_block_id: None,
                collapsed: false,
                refs_json: "[]".into(),
            })
            .await
            .unwrap();
    }
    let list = blocks
        .list(
            page(),
            Some(architect::Sort {
                field: "sort_key".into(),
                order: architect::SortOrder::Asc,
            }),
            None,
        )
        .await
        .unwrap();
    assert_eq!(list.items.len(), 3);
    assert_eq!(list.items[0].content, "block 0");
    assert_eq!(list.items[2].content, "block 2");
}

#[tokio::test]
async fn knowledge_tag_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let tags = KnowledgeTagRepoLoro::new(&doc);
    let vault_id = Uuid::new_v4();
    let t = tags
        .create(KnowledgeTagCreate {
            vault_id,
            tag: "projects/alpha".into(),
            color: Some("#ff00aa".into()),
            description: Some("alpha project".into()),
        })
        .await
        .unwrap();
    let got = tags.get(t.id).await.unwrap();
    assert_eq!(got.tag, "projects/alpha");
    assert_eq!(got.color.as_deref(), Some("#ff00aa"));
}

#[tokio::test]
async fn base_round_trip() {
    let doc = CrdtDoc::ephemeral();
    let bases = BaseRepoLoro::new(&doc);
    let vault_id = Uuid::new_v4();
    let b = bases
        .create(BaseCreate {
            vault_id,
            page_id: None,
            name: "All Projects".into(),
            definition_yaml: "filters:\n  tags: [projects]\n".into(),
            parsed_filter_json: "{}".into(),
            parsed_views_json: "[]".into(),
        })
        .await
        .unwrap();
    let got = bases.get(b.id).await.unwrap();
    assert_eq!(got.name, "All Projects");
    assert!(got.definition_yaml.contains("projects"));
}
