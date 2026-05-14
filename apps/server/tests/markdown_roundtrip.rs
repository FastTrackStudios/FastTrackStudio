//! Phase 9 — markdown export/import round-trip stability test.
//!
//! End-to-end through the actual Loro repos (not just the pure
//! `knowledge_proto::export` functions):
//!
//! 1. Seed a `CrdtDoc` with one vault, a folder, two pages, three
//!    blocks each. Use the same `*RepoLoro` types `/knowledge`
//!    uses at runtime.
//! 2. Read everything back, render via `export_vault`, write to
//!    a temp dir.
//! 3. Walk the temp dir, build `ImportFile`s, call
//!    `import_vault`, push the result into a FRESH `CrdtDoc` via
//!    the same repos.
//! 4. Re-export from the second doc.
//! 5. Byte-equal the two exports (one entry per page, deterministic
//!    sort by path).

use std::sync::Arc;

use chrono::Utc;
use crdt::CrdtDoc;
use knowledge_crdt::{BlockRepoLoro, FolderRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::export::{ImportFile, export_vault, import_vault};
use knowledge_proto::{
    BlockCreate, BlockRepo, FolderCreate, FolderRepo, PageCreate, PageRepo, VaultCreate, VaultRepo,
};
use project_proto::architect::Page;

fn big() -> Page {
    Page {
        index: 0,
        size: 10_000,
    }
}

async fn seed_doc(doc: &Arc<CrdtDoc>) -> eyre::Result<()> {
    let vault_repo = VaultRepoLoro::new(doc);
    let folder_repo = FolderRepoLoro::new(doc);
    let page_repo = PageRepoLoro::new(doc);
    let block_repo = BlockRepoLoro::new(doc);

    let vault = vault_repo
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
        .map_err(|e| eyre::eyre!("vault create: {e}"))?;
    let folder = folder_repo
        .create(FolderCreate {
            vault_id: vault.id,
            path: "notes".into(),
            parent_id: None,
        })
        .await
        .map_err(|e| eyre::eyre!("folder create: {e}"))?;

    let now = Utc::now();
    for (basename, folder_id) in [("Hello", None), ("Deep", Some(folder.id))] {
        let page = page_repo
            .create(PageCreate {
                vault_id: vault.id,
                folder_id,
                path: match folder_id {
                    Some(_) => format!("notes/{basename}.md"),
                    None => format!("{basename}.md"),
                },
                basename: basename.into(),
                ext: "md".into(),
                aliases: vec![],
                frontmatter_json: "[]".into(),
                stat_ctime: now,
                stat_mtime: now,
                stat_size: 0,
                is_journal: false,
                journal_day: None,
                shadow_for_kind: None,
                shadow_for_id: None,
            })
            .await
            .map_err(|e| eyre::eyre!("page create: {e}"))?;
        for (i, body) in ["first paragraph", "second paragraph", "third paragraph"]
            .iter()
            .enumerate()
        {
            block_repo
                .create(BlockCreate {
                    vault_id: vault.id,
                    page_id: page.id,
                    parent_block_id: None,
                    sort_key: format!("a{i}"),
                    kind: "paragraph".into(),
                    content: (*body).into(),
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
                    canvas_node_json: None,
                })
                .await
                .map_err(|e| eyre::eyre!("block create: {e}"))?;
        }
    }
    Ok(())
}

async fn dump_doc(
    doc: &Arc<CrdtDoc>,
) -> eyre::Result<(
    Vec<knowledge_proto::Vault>,
    Vec<knowledge_proto::Folder>,
    Vec<knowledge_proto::Page>,
    Vec<knowledge_proto::Block>,
)> {
    let vault_repo = VaultRepoLoro::new(doc);
    let folder_repo = FolderRepoLoro::new(doc);
    let page_repo = PageRepoLoro::new(doc);
    let block_repo = BlockRepoLoro::new(doc);
    let v = vault_repo
        .list(big(), None, None)
        .await
        .map_err(|e| eyre::eyre!("v: {e}"))?
        .items;
    let f = folder_repo
        .list(big(), None, None)
        .await
        .map_err(|e| eyre::eyre!("f: {e}"))?
        .items;
    let p = page_repo
        .list(big(), None, None)
        .await
        .map_err(|e| eyre::eyre!("p: {e}"))?
        .items;
    let b = block_repo
        .list(big(), None, None)
        .await
        .map_err(|e| eyre::eyre!("b: {e}"))?
        .items;
    Ok((v, f, p, b))
}

async fn push_into(
    doc: &Arc<CrdtDoc>,
    imported: knowledge_proto::export::ImportResult,
) -> eyre::Result<()> {
    use knowledge_proto::VaultCreate as VC;
    let vault_repo = VaultRepoLoro::new(doc);
    let folder_repo = FolderRepoLoro::new(doc);
    let page_repo = PageRepoLoro::new(doc);
    let block_repo = BlockRepoLoro::new(doc);

    // Create the vault. We reuse the imported vault's fields but
    // let the repo mint a fresh id (the round-trip cares about
    // bytes, not ids).
    let new_vault = vault_repo
        .create(VC {
            name: imported.vault.name.clone(),
            root_path: imported.vault.root_path.clone(),
            use_markdown_links: imported.vault.use_markdown_links,
            new_link_format: imported.vault.new_link_format.clone(),
            attachment_folder_path: imported.vault.attachment_folder_path.clone(),
            default_view_mode: imported.vault.default_view_mode.clone(),
            config_json: imported.vault.config_json.clone(),
        })
        .await
        .map_err(|e| eyre::eyre!("create vault: {e}"))?;

    // Sort folders parent-first so each create has its parent_id
    // pointing at an already-inserted row.
    let mut folders = imported.folders.clone();
    folders.sort_by_key(|f| f.path.matches('/').count());
    let mut folder_id_remap: std::collections::HashMap<uuid::Uuid, uuid::Uuid> =
        std::collections::HashMap::new();
    for f in folders {
        let parent_id = f
            .parent_id
            .and_then(|pid| folder_id_remap.get(&pid).copied());
        let new = folder_repo
            .create(FolderCreate {
                vault_id: new_vault.id,
                path: f.path.clone(),
                parent_id,
            })
            .await
            .map_err(|e| eyre::eyre!("folder: {e}"))?;
        folder_id_remap.insert(f.id, new.id);
    }

    // Pages.
    let mut page_id_remap: std::collections::HashMap<uuid::Uuid, uuid::Uuid> =
        std::collections::HashMap::new();
    for p in imported.pages {
        let folder_id = p
            .folder_id
            .and_then(|fid| folder_id_remap.get(&fid).copied());
        let new = page_repo
            .create(PageCreate {
                vault_id: new_vault.id,
                folder_id,
                path: p.path.clone(),
                basename: p.basename.clone(),
                ext: p.ext.clone(),
                aliases: p.aliases.clone(),
                frontmatter_json: p.frontmatter_json.clone(),
                stat_ctime: p.stat_ctime,
                stat_mtime: p.stat_mtime,
                stat_size: p.stat_size,
                is_journal: p.is_journal,
                journal_day: p.journal_day.clone(),
                shadow_for_kind: p.shadow_for_kind.clone(),
                shadow_for_id: p.shadow_for_id,
            })
            .await
            .map_err(|e| eyre::eyre!("page: {e}"))?;
        page_id_remap.insert(p.id, new.id);
    }

    // Blocks.
    for b in imported.blocks {
        let page_id = page_id_remap
            .get(&b.page_id)
            .copied()
            .ok_or_else(|| eyre::eyre!("block points at unknown page"))?;
        block_repo
            .create(BlockCreate {
                vault_id: new_vault.id,
                page_id,
                parent_block_id: None,
                sort_key: b.sort_key.clone(),
                kind: b.kind.clone(),
                content: b.content.clone(),
                heading_level: b.heading_level,
                list_ordered: b.list_ordered,
                list_task: b.list_task.clone(),
                code_lang: b.code_lang.clone(),
                callout_kind: b.callout_kind.clone(),
                callout_foldable: b.callout_foldable,
                properties_json: b.properties_json.clone(),
                obsidian_block_id: b.obsidian_block_id.clone(),
                collapsed: b.collapsed,
                refs_json: b.refs_json.clone(),
                canvas_node_json: b.canvas_node_json.clone(),
            })
            .await
            .map_err(|e| eyre::eyre!("block: {e}"))?;
    }
    Ok(())
}

#[tokio::test(flavor = "multi_thread")]
async fn export_import_export_bytes_match() -> eyre::Result<()> {
    // Source doc → export.
    let doc_a = Arc::new(CrdtDoc::ephemeral());
    seed_doc(&doc_a).await?;
    let (vaults_a, folders_a, pages_a, blocks_a) = dump_doc(&doc_a).await?;
    assert_eq!(vaults_a.len(), 1);
    let plan_a = export_vault(&vaults_a[0], &folders_a, &pages_a, &blocks_a);
    assert_eq!(plan_a.files.len(), 2);

    // Write to a tempdir + read back as ImportFiles. Proves the
    // disk format actually round-trips, not just in-memory.
    let tmp = tempfile::tempdir()?;
    for f in &plan_a.files {
        let dest = tmp.path().join(&f.relative_path);
        if let Some(parent) = dest.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&dest, &f.contents)?;
    }
    let mut import_files: Vec<ImportFile> = Vec::new();
    for entry in walkdir::WalkDir::new(tmp.path()) {
        let entry = entry?;
        if !entry.file_type().is_file() {
            continue;
        }
        let rel = entry
            .path()
            .strip_prefix(tmp.path())?
            .to_string_lossy()
            .to_string();
        let contents = std::fs::read_to_string(entry.path())?;
        import_files.push(ImportFile {
            relative_path: rel,
            contents,
        });
    }

    // Import → fresh doc.
    let imported = import_vault("Org-Reimported", &import_files)?;
    let doc_b = Arc::new(CrdtDoc::ephemeral());
    push_into(&doc_b, imported).await?;

    // Re-export from doc_b.
    let (vaults_b, folders_b, pages_b, blocks_b) = dump_doc(&doc_b).await?;
    let plan_b = export_vault(&vaults_b[0], &folders_b, &pages_b, &blocks_b);

    // Compare. Sort by path for determinism.
    let mut a: Vec<(String, String)> = plan_a
        .files
        .iter()
        .map(|f| (f.relative_path.clone(), f.contents.clone()))
        .collect();
    let mut b: Vec<(String, String)> = plan_b
        .files
        .iter()
        .map(|f| (f.relative_path.clone(), f.contents.clone()))
        .collect();
    a.sort();
    b.sort();
    assert_eq!(a.len(), b.len(), "different number of files");
    for (pa, pb) in a.iter().zip(b.iter()) {
        assert_eq!(pa.0, pb.0, "path drift");
        assert_eq!(
            pa.1, pb.1,
            "contents drift for {}\n--- A ---\n{}\n--- B ---\n{}",
            pa.0, pa.1, pb.1
        );
    }
    Ok(())
}
