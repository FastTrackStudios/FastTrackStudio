//! Logseq-graph writer — the inverse of `graph_loader`.
//!
//! Serializes our CRDT state back to `.md` files in the
//! Logseq-shaped layout (`<root>/pages/*.md`,
//! `<root>/journals/*.md`) so users can co-edit with Logseq or
//! keep their notes portable on disk. Activates only for vaults
//! whose `root_path` is set; ephemeral vaults stay in-memory.
//!
//! Background loop: subscribes to local Loro commits and writes
//! affected pages back to disk after a quiet 500ms window.
//! Whole-vault rebuilds are cheap at typical graph sizes.

use std::path::{Path, PathBuf};
use std::sync::Arc;

use crdt::CrdtDoc;
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::{Block, BlockRepo, Page, PageRepo, VaultRepo, architect::Page as ListPage};

#[derive(Debug, thiserror::Error)]
pub enum WriteError {
    #[error("io: {0}")]
    Io(String),
    #[error("repo: {0}")]
    Repo(String),
}

/// Snapshot the doc + write every page in every vault that has
/// a `root_path` set. Pages with no `root_path` are skipped.
/// Returns the count of files written.
pub async fn write_all_pages(doc: &Arc<CrdtDoc>) -> Result<usize, WriteError> {
    let vault_repo = VaultRepoLoro::new(doc);
    let page_repo = PageRepoLoro::new(doc);
    let block_repo = BlockRepoLoro::new(doc);
    let big = ListPage {
        index: 0,
        size: 100_000,
    };

    let vaults = vault_repo
        .list(big.clone(), None, None)
        .await
        .map_err(|e| WriteError::Repo(format!("vault list: {e}")))?
        .items;
    let all_pages = page_repo
        .list(big.clone(), None, None)
        .await
        .map_err(|e| WriteError::Repo(format!("page list: {e}")))?
        .items;
    let all_blocks = block_repo
        .list(big, None, None)
        .await
        .map_err(|e| WriteError::Repo(format!("block list: {e}")))?
        .items;

    let mut written = 0usize;
    for v in &vaults {
        let Some(root) = v.root_path.as_ref() else {
            continue;
        };
        let root = PathBuf::from(root);
        for p in all_pages.iter().filter(|p| p.vault_id == v.id) {
            let page_blocks: Vec<Block> = all_blocks
                .iter()
                .filter(|b| b.page_id == p.id)
                .cloned()
                .collect();
            write_one_page(&root, p, &page_blocks)?;
            written += 1;
        }
    }
    Ok(written)
}

/// Write one page to disk under `<root>/pages/<basename>.md` or
/// `<root>/journals/<basename>.md` based on the journal flag.
fn write_one_page(root: &Path, page: &Page, blocks: &[Block]) -> Result<(), WriteError> {
    let subdir = if page.is_journal { "journals" } else { "pages" };
    let dir = root.join(subdir);
    std::fs::create_dir_all(&dir).map_err(|e| WriteError::Io(e.to_string()))?;
    let safe_name = sanitize_basename(&page.basename);
    let file = dir.join(format!("{safe_name}.md"));
    let body = export_page_markdown(page, blocks);
    std::fs::write(&file, body).map_err(|e| WriteError::Io(e.to_string()))?;
    Ok(())
}

/// Make a Logseq-safe filename from a page basename. Replaces
/// path separators and other problematic chars with `_`.
fn sanitize_basename(s: &str) -> String {
    s.chars()
        .map(|c| match c {
            '/' | '\\' | ':' | '*' | '?' | '"' | '<' | '>' | '|' | '\0' => '_',
            c => c,
        })
        .collect()
}

/// Serialize one page's full content as a Logseq-format `.md`
/// file. Frontmatter properties come first, then the block tree
/// as `- ` bullets with 2-space-per-level indent, with `id::`
/// and any other block properties emitted under each bullet.
pub fn export_page_markdown(page: &Page, blocks: &[Block]) -> String {
    let mut out = String::new();
    // Frontmatter (page properties).
    if let Ok(serde_json::Value::Object(m)) =
        serde_json::from_str::<serde_json::Value>(&page.frontmatter_json)
    {
        // `title::` is implicit (it's the filename), skip if
        // present to avoid double-writing. Aliases are emitted
        // from page.aliases instead of the frontmatter blob so
        // they stay in sync.
        let mut emitted_alias = false;
        if !page.aliases.is_empty() {
            out.push_str(&format!("alias:: {}\n", page.aliases.join(", ")));
            emitted_alias = true;
        }
        // Stable order: BTreeMap.
        let sorted: std::collections::BTreeMap<_, _> = m.iter().collect();
        for (k, v) in sorted {
            if k == "title" || (emitted_alias && (k == "alias" || k == "aliases")) {
                continue;
            }
            let value = match v {
                serde_json::Value::String(s) => s.clone(),
                _ => v.to_string(),
            };
            out.push_str(&format!("{k}:: {value}\n"));
        }
        if !out.is_empty() {
            out.push('\n');
        }
    }

    // Block tree.
    let mut sorted = blocks.to_vec();
    sorted.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    let tree = build_tree(&sorted);
    for node in tree {
        write_block_md(&node, 0, &mut out);
    }
    out
}

#[derive(Clone)]
struct TreeNode {
    block: Block,
    children: Vec<TreeNode>,
}

fn build_tree(blocks: &[Block]) -> Vec<TreeNode> {
    use std::collections::HashMap;
    let mut by_parent: HashMap<Option<uuid::Uuid>, Vec<Block>> = HashMap::new();
    for b in blocks {
        by_parent
            .entry(b.parent_block_id)
            .or_default()
            .push(b.clone());
    }
    for v in by_parent.values_mut() {
        v.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }
    fn recur(
        parent: Option<uuid::Uuid>,
        map: &std::collections::HashMap<Option<uuid::Uuid>, Vec<Block>>,
    ) -> Vec<TreeNode> {
        map.get(&parent)
            .cloned()
            .unwrap_or_default()
            .into_iter()
            .map(|b| {
                let id = b.id;
                TreeNode {
                    block: b,
                    children: recur(Some(id), map),
                }
            })
            .collect()
    }
    recur(None, &by_parent)
}

fn write_block_md(node: &TreeNode, depth: usize, out: &mut String) {
    let indent = "  ".repeat(depth);
    let block = &node.block;
    out.push_str(&indent);
    out.push_str("- ");
    // Block content — first line directly after the bullet; any
    // remaining lines get continuation indent so they belong to
    // the same block when re-imported.
    let mut first = true;
    for line in block.content.lines() {
        if first {
            out.push_str(line);
            out.push('\n');
            first = false;
        } else {
            out.push_str(&indent);
            out.push_str("  ");
            out.push_str(line);
            out.push('\n');
        }
    }
    if first {
        // Empty content — emit a blank bullet line.
        out.push('\n');
    }

    // Emit `id::` property so round-trip preserves block UUIDs.
    let prop_indent = format!("{indent}  ");
    out.push_str(&prop_indent);
    out.push_str("id:: ");
    out.push_str(&block.id.to_string());
    out.push('\n');

    // Other properties from `properties_json` (excluding `id`
    // which we just wrote).
    if let Ok(serde_json::Value::Object(m)) =
        serde_json::from_str::<serde_json::Value>(&block.properties_json)
    {
        let sorted: std::collections::BTreeMap<_, _> = m.iter().collect();
        for (k, v) in sorted {
            if k == "id" {
                continue;
            }
            let value = match v {
                serde_json::Value::String(s) => s.clone(),
                _ => v.to_string(),
            };
            out.push_str(&prop_indent);
            out.push_str(&format!("{k}:: {value}\n"));
        }
    }

    for child in &node.children {
        write_block_md(child, depth + 1, out);
    }
}

/// Background loop: subscribe to local Loro commits, debounce
/// 500ms, then write all pages in vaults that have a root_path.
#[cfg(not(target_arch = "wasm32"))]
pub async fn run_disk_writer_loop(doc: Arc<CrdtDoc>) {
    use futures::StreamExt;
    let (tx, mut rx) = futures::channel::mpsc::unbounded::<()>();
    let sub = doc.loro().subscribe_local_update(Box::new(move |_b| {
        let _ = tx.unbounded_send(());
        true
    }));
    std::mem::forget(sub);
    loop {
        if rx.next().await.is_none() {
            return;
        }
        let _ = tokio::time::timeout(std::time::Duration::from_millis(500), async {
            while rx.next().await.is_some() {}
        })
        .await;
        match write_all_pages(&doc).await {
            Ok(n) => {
                if n > 0 {
                    tracing::info!(pages = n, "wrote vault to disk");
                }
            }
            Err(e) => tracing::warn!(?e, "disk write failed"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use knowledge_proto::{BlockCreate, PageCreate, VaultCreate};
    use uuid::Uuid;

    fn mk_page(name: &str) -> Page {
        let now = Utc::now();
        Page {
            id: Uuid::new_v4(),
            vault_id: Uuid::nil(),
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
            created_at: now,
            updated_at: now,
        }
    }

    fn mk_block(page_id: Uuid, parent: Option<Uuid>, sort: &str, content: &str) -> Block {
        let now = Utc::now();
        Block {
            id: Uuid::new_v4(),
            vault_id: Uuid::nil(),
            page_id,
            parent_block_id: parent,
            sort_key: sort.into(),
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
            created_at: now,
            updated_at: now,
        }
    }

    #[test]
    fn sanitize_basename_replaces_problem_chars() {
        assert_eq!(sanitize_basename("foo/bar"), "foo_bar");
        assert_eq!(sanitize_basename("a:b*c?d"), "a_b_c_d");
        assert_eq!(sanitize_basename("plain"), "plain");
    }

    #[test]
    fn export_includes_frontmatter() {
        let mut p = mk_page("Notes");
        p.frontmatter_json = r#"{"status":"active","priority":"high"}"#.into();
        let md = export_page_markdown(&p, &[]);
        assert!(md.contains("status:: active"));
        assert!(md.contains("priority:: high"));
    }

    #[test]
    fn export_emits_aliases_from_typed_field() {
        let mut p = mk_page("Notes");
        p.aliases = vec!["Home".into(), "Start".into()];
        let md = export_page_markdown(&p, &[]);
        assert!(md.contains("alias:: Home, Start"));
    }

    #[test]
    fn export_writes_block_tree_with_ids() {
        let p = mk_page("P");
        let parent = mk_block(p.id, None, "a", "parent");
        let parent_id = parent.id;
        let child = mk_block(p.id, Some(parent_id), "a", "child");
        let md = export_page_markdown(&p, &[parent.clone(), child.clone()]);
        assert!(md.contains("- parent"));
        assert!(md.contains(&format!("  id:: {parent_id}")));
        assert!(md.contains("  - child"));
        assert!(md.contains(&format!("    id:: {}", child.id)));
    }

    #[tokio::test]
    async fn write_all_pages_round_trip() {
        // Import a tiny graph, then export it; spot-check output.
        let temp_in =
            std::env::temp_dir().join(format!("task-writer-in-{}", Uuid::new_v4().simple()));
        let temp_out =
            std::env::temp_dir().join(format!("task-writer-out-{}", Uuid::new_v4().simple()));
        std::fs::create_dir_all(temp_in.join("pages")).unwrap();
        std::fs::write(
            temp_in.join("pages/Notes.md"),
            "title:: Notes\nstatus:: active\n\n- first\n- second\n  - child\n",
        )
        .unwrap();

        let doc = Arc::new(CrdtDoc::ephemeral());
        let stats = crate::graph_loader::import_logseq_graph(&doc, &temp_in)
            .await
            .unwrap();
        assert_eq!(stats.pages, 1);
        assert_eq!(stats.blocks, 3);

        // Point the imported vault at the output dir.
        {
            let vr = VaultRepoLoro::new(&doc);
            let big = ListPage { index: 0, size: 10 };
            let vs = vr.list(big, None, None).await.unwrap().items;
            let v = &vs[0];
            vr.update(
                v.id,
                knowledge_proto::VaultUpdate {
                    root_path: Some(Some(temp_out.display().to_string())),
                    ..Default::default()
                },
            )
            .await
            .unwrap();
        }

        let written = write_all_pages(&doc).await.unwrap();
        assert_eq!(written, 1);
        let out_path = temp_out.join("pages/Notes.md");
        let body = std::fs::read_to_string(&out_path).unwrap();
        assert!(body.contains("status:: active"));
        assert!(body.contains("- first"));
        assert!(body.contains("- second"));
        assert!(body.contains("  - child"));

        let _ = std::fs::remove_dir_all(&temp_in);
        let _ = std::fs::remove_dir_all(&temp_out);
    }
}
