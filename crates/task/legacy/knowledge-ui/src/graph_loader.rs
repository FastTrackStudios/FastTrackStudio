//! Logseq-graph importer.
//!
//! Reads a directory shaped like Logseq's on-disk graph
//! (`pages/*.md`, `journals/*.md`) and seeds our `CrdtDoc` with
//! equivalent vault + pages + blocks. The parser supports the
//! common Logseq markdown grammar we care about for round-trip:
//!
//! - **Page properties** at the top of each file: leading
//!   `key:: value` lines before the first bullet.
//! - **Block tree** via `- ` bullets, indented with 2 spaces or
//!   a tab per nesting level.
//! - **Block properties**: `key:: value` lines under a bullet,
//!   indented past the bullet's payload column.
//! - **Block UUIDs**: the `id::` property is recognized and
//!   surfaced on the parsed block. Importers can use it as the
//!   CRDT block UUID for round-trip stability; missing id →
//!   mint a fresh v4.
//! - **Journal flag**: filenames under `journals/` set
//!   `is_journal: true` and the file stem becomes the
//!   `journal_day` (after normalizing `YYYY_MM_DD → YYYY-MM-DD`).
//!
//! Unsupported syntax is preserved as block content rather than
//! dropped.

use std::path::Path;
use std::sync::Arc;

use chrono::Utc;
use crdt::CrdtDoc;
use knowledge_crdt::{BlockRepoLoro, PageRepoLoro, VaultRepoLoro};
use knowledge_proto::{BlockCreate, BlockRepo, PageCreate, PageRepo, VaultCreate, VaultRepo};
use uuid::Uuid;

/// Summary of an import run.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ImportStats {
    pub vaults: usize,
    pub pages: usize,
    pub blocks: usize,
    pub journals: usize,
    pub failures: Vec<String>,
}

#[derive(Debug, thiserror::Error)]
pub enum ImportError {
    #[error("io: {0}")]
    Io(String),
    #[error("repo: {0}")]
    Repo(String),
}

/// Import a Logseq-shaped graph directory into the doc. Creates
/// one vault, then walks `<dir>/pages/*.md` and
/// `<dir>/journals/*.md`. Per-file failures are collected into
/// `stats.failures` without aborting the whole import.
pub async fn import_logseq_graph(
    doc: &Arc<CrdtDoc>,
    dir: &Path,
) -> Result<ImportStats, ImportError> {
    let mut stats = ImportStats::default();

    let vault_repo = VaultRepoLoro::new(doc);
    let page_repo = PageRepoLoro::new(doc);
    let block_repo = BlockRepoLoro::new(doc);

    let name = dir
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("Imported")
        .to_string();
    let vault = vault_repo
        .create(VaultCreate {
            name,
            root_path: Some(dir.display().to_string()),
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: String::new(),
            default_view_mode: "live-preview".into(),
            config_json: "{}".into(),
        })
        .await
        .map_err(|e| ImportError::Repo(format!("vault create: {e}")))?;
    stats.vaults = 1;

    for (subdir, is_journal) in [("pages", false), ("journals", true)] {
        let path = dir.join(subdir);
        if !path.is_dir() {
            continue;
        }
        let entries = std::fs::read_dir(&path).map_err(|e| ImportError::Io(e.to_string()))?;
        for entry in entries.flatten() {
            let file_path = entry.path();
            if !is_markdown(&file_path) {
                continue;
            }
            match import_one_file(&page_repo, &block_repo, vault.id, &file_path, is_journal).await {
                Ok((page_count, block_count, journal_count)) => {
                    stats.pages += page_count;
                    stats.blocks += block_count;
                    stats.journals += journal_count;
                }
                Err(e) => stats.failures.push(format!("{}: {e}", file_path.display())),
            }
        }
    }

    Ok(stats)
}

fn is_markdown(p: &Path) -> bool {
    p.extension()
        .and_then(|e| e.to_str())
        .map(|e| e.eq_ignore_ascii_case("md") || e.eq_ignore_ascii_case("markdown"))
        .unwrap_or(false)
}

async fn import_one_file(
    page_repo: &PageRepoLoro,
    block_repo: &BlockRepoLoro,
    vault_id: Uuid,
    path: &Path,
    is_journal: bool,
) -> Result<(usize, usize, usize), ImportError> {
    let raw = std::fs::read_to_string(path).map_err(|e| ImportError::Io(e.to_string()))?;
    let basename = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("Untitled")
        .to_string();
    let journal_day = if is_journal {
        Some(normalize_journal_day(&basename))
    } else {
        None
    };

    let parsed = parse_logseq_markdown(&raw);
    // Whiteboard detection — Logseq stores whiteboards as
    // `<name>.excalidraw.md` with the canvas JSON inside. We don't
    // ship Excalidraw, but we tag the page with `whiteboard: true`
    // + stash the raw payload as `excalidraw` so the UI can render
    // a read-only preview card.
    let is_whiteboard = path
        .file_name()
        .and_then(|s| s.to_str())
        .map(|s| s.to_lowercase().ends_with(".excalidraw.md"))
        .unwrap_or(false);
    let frontmatter_json = if is_whiteboard {
        let mut obj: serde_json::Map<String, serde_json::Value> =
            serde_json::from_str(&parsed.frontmatter_json).unwrap_or_default();
        obj.insert("whiteboard".into(), serde_json::Value::Bool(true));
        obj.insert(
            "excalidraw".into(),
            serde_json::Value::String(raw.chars().take(20_000).collect()),
        );
        serde_json::Value::Object(obj).to_string()
    } else {
        parsed.frontmatter_json
    };
    let now = Utc::now();
    let page = page_repo
        .create(PageCreate {
            vault_id,
            folder_id: None,
            path: path
                .file_name()
                .and_then(|s| s.to_str())
                .unwrap_or("")
                .to_string(),
            basename: basename.clone(),
            ext: path
                .extension()
                .and_then(|e| e.to_str())
                .unwrap_or("md")
                .to_string(),
            aliases: parsed.aliases,
            frontmatter_json,
            stat_ctime: now,
            stat_mtime: now,
            stat_size: raw.len() as i64,
            is_journal,
            journal_day,
            shadow_for_kind: None,
            shadow_for_id: None,
        })
        .await
        .map_err(|e| ImportError::Repo(format!("page create: {e}")))?;

    let mut count = 0usize;
    create_blocks_recursive(
        block_repo,
        vault_id,
        page.id,
        None,
        &parsed.blocks,
        &mut count,
    )
    .await?;

    Ok((1, count, if is_journal { 1 } else { 0 }))
}

/// Best-effort journal-day normalization. Logseq journal files
/// are typically named `YYYY_MM_DD.md`; convert to ISO 8601.
fn normalize_journal_day(stem: &str) -> String {
    let normalized = stem.replace('_', "-");
    if chrono::NaiveDate::parse_from_str(&normalized, "%Y-%m-%d").is_ok() {
        normalized
    } else {
        stem.to_string()
    }
}

/// Output of `parse_logseq_markdown`.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct ParsedFile {
    /// JSON-stringified front-matter properties.
    pub frontmatter_json: String,
    /// Aliases from `alias::` / `aliases::` (comma-split).
    pub aliases: Vec<String>,
    /// Top-level blocks; children for nested.
    pub blocks: Vec<ParsedBlock>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParsedBlock {
    /// `id::` property if present in the source.
    pub id: Option<Uuid>,
    pub content: String,
    /// Properties as JSON string (matches our `properties_json`
    /// wire shape).
    pub properties_json: String,
    pub children: Vec<ParsedBlock>,
}

/// Parse a Logseq markdown file into a `ParsedFile`.
pub fn parse_logseq_markdown(raw: &str) -> ParsedFile {
    // Phase 1: split frontmatter from body.
    let mut frontmatter_pairs: Vec<(String, String)> = Vec::new();
    let mut body_offset = 0usize;
    let mut cursor = 0usize;
    for line in raw.split_inclusive('\n') {
        let line_no_nl = line.trim_end_matches('\n');
        let trimmed = line_no_nl.trim_start();
        if trimmed.is_empty() {
            cursor += line.len();
            body_offset = cursor;
            continue;
        }
        if let Some((key, value)) = publish_core::parse_property_line(line_no_nl) {
            frontmatter_pairs.push((key, value));
            cursor += line.len();
            body_offset = cursor;
            continue;
        }
        break;
    }
    let body = raw.get(body_offset.min(raw.len())..).unwrap_or("");

    // Phase 2: build frontmatter JSON + aliases.
    let mut frontmatter_map = serde_json::Map::new();
    let mut aliases: Vec<String> = Vec::new();
    for (k, v) in &frontmatter_pairs {
        if k == "alias" || k == "aliases" {
            for piece in v.split(',') {
                let p = piece.trim();
                if !p.is_empty() {
                    aliases.push(p.to_string());
                }
            }
        }
        frontmatter_map.insert(k.clone(), serde_json::Value::String(v.clone()));
    }
    let frontmatter_json = if frontmatter_map.is_empty() {
        "{}".into()
    } else {
        serde_json::Value::Object(frontmatter_map).to_string()
    };

    // Phase 3: parse block tree.
    let blocks = parse_blocks(body);

    ParsedFile {
        frontmatter_json,
        aliases,
        blocks,
    }
}

/// Token shape used by the bullet-tree parser.
#[derive(Debug, Clone)]
enum LineToken {
    /// `(indent_cols, payload_after_bullet)`
    Bullet { indent: usize, payload: String },
    /// `(indent_cols, raw_line)` — continuation under a bullet.
    Continuation { indent: usize, line: String },
    /// Empty / whitespace-only.
    Blank,
}

fn tokenize(body: &str) -> Vec<LineToken> {
    let mut out = Vec::new();
    for line in body.lines() {
        let mut indent = 0usize;
        for ch in line.chars() {
            match ch {
                ' ' => indent += 1,
                '\t' => indent += 2,
                _ => break,
            }
        }
        let after = &line[line.len().min(indent)..];
        let trimmed_check = after.trim();
        if trimmed_check.is_empty() {
            out.push(LineToken::Blank);
            continue;
        }
        if let Some(rest) = after.strip_prefix("- ") {
            out.push(LineToken::Bullet {
                indent,
                payload: rest.to_string(),
            });
        } else if after == "-" {
            out.push(LineToken::Bullet {
                indent,
                payload: String::new(),
            });
        } else {
            out.push(LineToken::Continuation {
                indent,
                line: after.to_string(),
            });
        }
    }
    out
}

/// Build a flat list of `(depth, ParsedBlock)` then fold into a
/// tree. Two-pass keeps the parser linear + simple, at the cost
/// of one extra walk.
fn parse_blocks(body: &str) -> Vec<ParsedBlock> {
    let tokens = tokenize(body);

    // First pass: flatten into (depth, block) pairs. Depth is
    // measured in indent columns / 2 (tab = 2 cols already).
    // Continuation lines append to the most recent bullet's
    // content (separated by newline).
    let mut flat: Vec<(usize, ParsedBlock)> = Vec::new();
    for tok in tokens {
        match tok {
            LineToken::Bullet { indent, payload } => {
                let depth = indent / 2;
                flat.push((
                    depth,
                    ParsedBlock {
                        id: None,
                        content: payload,
                        properties_json: "{}".into(),
                        children: Vec::new(),
                    },
                ));
            }
            LineToken::Continuation { line, .. } => {
                if let Some((_, last)) = flat.last_mut() {
                    if !last.content.is_empty() {
                        last.content.push('\n');
                    }
                    last.content.push_str(&line);
                }
                // Continuation before any bullet: dropped (matches
                // Logseq's behavior — text outside a bullet is
                // typically the page properties block, already
                // consumed by Phase 1).
            }
            LineToken::Blank => {}
        }
    }

    // Per-block: peel `id::` + `key:: value` properties out of
    // the now-aggregated content.
    for (_, b) in flat.iter_mut() {
        peel_block_into(b);
    }

    // Second pass: fold flat list into a tree using a depth
    // stack. We hold pending blocks in a separate accumulator
    // since we can't mutate items already inside the tree by
    // path without re-traversal.
    fold_flat_to_tree(flat)
}

fn peel_block_into(block: &mut ParsedBlock) {
    let (json, rest) = publish_core::peel_block_properties(&block.content);
    block.properties_json = json;
    block.content = rest;
    // Hoist `id::` into the typed field.
    if let Ok(serde_json::Value::Object(m)) =
        serde_json::from_str::<serde_json::Value>(&block.properties_json)
    {
        if let Some(serde_json::Value::String(id_str)) = m.get("id") {
            if let Ok(parsed) = Uuid::parse_str(id_str) {
                block.id = Some(parsed);
            }
        }
    }
}

fn fold_flat_to_tree(flat: Vec<(usize, ParsedBlock)>) -> Vec<ParsedBlock> {
    // Walk in reverse so children come before parents — that
    // way each parent can `pop` children off the working stack.
    // Each step: collect children at depth+1 that come after the
    // current block, attach.
    //
    // Simpler iterative shape: keep a stack of (depth, Vec<ParsedBlock>).
    // For each (d, b):
    //   while stack.top.0 > d → pop, attach to stack.top.1 last block's children.
    //   if stack.top.0 < d → start a new level
    let mut stack: Vec<(usize, Vec<ParsedBlock>)> = vec![(0, Vec::new())];
    for (depth, block) in flat {
        // Close levels until stack top depth <= current depth.
        while stack.last().map(|s| s.0).unwrap_or(0) > depth {
            let (_, children) = stack.pop().unwrap();
            if let Some(parent_level) = stack.last_mut() {
                if let Some(last) = parent_level.1.last_mut() {
                    last.children.extend(children);
                }
            }
        }
        // Open a new level if we just stepped deeper.
        while stack.last().map(|s| s.0).unwrap_or(0) < depth {
            let new_depth = stack.last().map(|s| s.0 + 1).unwrap_or(1);
            stack.push((new_depth, Vec::new()));
        }
        // Push into current level.
        stack.last_mut().unwrap().1.push(block);
    }
    // Drain remaining levels into the root.
    while stack.len() > 1 {
        let (_, children) = stack.pop().unwrap();
        if let Some(parent_level) = stack.last_mut() {
            if let Some(last) = parent_level.1.last_mut() {
                last.children.extend(children);
            }
        }
    }
    stack.pop().map(|(_, v)| v).unwrap_or_default()
}

async fn create_blocks_recursive(
    repo: &BlockRepoLoro,
    vault_id: Uuid,
    page_id: Uuid,
    parent_block_id: Option<Uuid>,
    blocks: &[ParsedBlock],
    counter: &mut usize,
) -> Result<(), ImportError> {
    for (i, b) in blocks.iter().enumerate() {
        let sort_key = sort_key_for_index(i);
        let created = repo
            .create(BlockCreate {
                vault_id,
                page_id,
                parent_block_id,
                sort_key,
                kind: "paragraph".into(),
                content: b.content.clone(),
                heading_level: None,
                list_ordered: false,
                list_task: None,
                code_lang: None,
                callout_kind: None,
                callout_foldable: false,
                properties_json: b.properties_json.clone(),
                obsidian_block_id: None,
                collapsed: false,
                refs_json: "[]".into(),
                canvas_node_json: None,
            })
            .await
            .map_err(|e| ImportError::Repo(format!("block create: {e}")))?;
        *counter += 1;
        if !b.children.is_empty() {
            Box::pin(create_blocks_recursive(
                repo,
                vault_id,
                page_id,
                Some(created.id),
                &b.children,
                counter,
            ))
            .await?;
        }
    }
    Ok(())
}

/// Stable sort key from a 0-based index. Two letters (`aa`,
/// `ab`, … `zz`) ordered lexicographically. Sufficient for
/// imports under ~676 siblings per parent — well above any
/// realistic block list.
pub(crate) fn sort_key_for_index(i: usize) -> String {
    let lo = (b'a' + (i % 26) as u8) as char;
    let hi = (b'a' + ((i / 26) % 26) as u8) as char;
    format!("{hi}{lo}")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_frontmatter_only() {
        let raw = "title:: My Page\nstatus:: active\n\n";
        let parsed = parse_logseq_markdown(raw);
        let v: serde_json::Value = serde_json::from_str(&parsed.frontmatter_json).unwrap();
        assert_eq!(v["title"], serde_json::json!("My Page"));
        assert_eq!(v["status"], serde_json::json!("active"));
        assert!(parsed.blocks.is_empty());
    }

    #[test]
    fn parse_aliases_split_on_commas() {
        let raw = "alias:: foo, bar, baz\n";
        let parsed = parse_logseq_markdown(raw);
        assert_eq!(
            parsed.aliases,
            vec!["foo".to_string(), "bar".to_string(), "baz".to_string()]
        );
    }

    #[test]
    fn parse_flat_bullets() {
        let raw = "- first\n- second\n- third\n";
        let parsed = parse_logseq_markdown(raw);
        assert_eq!(parsed.blocks.len(), 3);
        assert_eq!(parsed.blocks[0].content, "first");
        assert_eq!(parsed.blocks[1].content, "second");
        assert_eq!(parsed.blocks[2].content, "third");
        assert!(parsed.blocks.iter().all(|b| b.children.is_empty()));
    }

    #[test]
    fn parse_nested_bullets() {
        let raw = "- parent\n  - child\n    - grandchild\n  - sibling\n- another top\n";
        let parsed = parse_logseq_markdown(raw);
        assert_eq!(parsed.blocks.len(), 2);
        assert_eq!(parsed.blocks[0].content, "parent");
        assert_eq!(parsed.blocks[0].children.len(), 2);
        assert_eq!(parsed.blocks[0].children[0].content, "child");
        assert_eq!(parsed.blocks[0].children[0].children.len(), 1);
        assert_eq!(
            parsed.blocks[0].children[0].children[0].content,
            "grandchild"
        );
        assert_eq!(parsed.blocks[0].children[1].content, "sibling");
        assert_eq!(parsed.blocks[1].content, "another top");
    }

    #[test]
    fn parse_block_with_id_and_properties() {
        let raw = "- a task\n  id:: 12345678-1234-1234-1234-123456789012\n  priority:: high\n";
        let parsed = parse_logseq_markdown(raw);
        assert_eq!(parsed.blocks.len(), 1);
        let b = &parsed.blocks[0];
        assert_eq!(b.content, "a task");
        assert_eq!(
            b.id,
            Some(Uuid::parse_str("12345678-1234-1234-1234-123456789012").unwrap())
        );
        let props: serde_json::Value = serde_json::from_str(&b.properties_json).unwrap();
        assert_eq!(props["priority"], serde_json::json!("high"));
        assert_eq!(
            props["id"],
            serde_json::json!("12345678-1234-1234-1234-123456789012")
        );
    }

    #[test]
    fn parse_block_continuation_lines() {
        let raw = "- first line\n  second line\n  third line\n";
        let parsed = parse_logseq_markdown(raw);
        assert_eq!(parsed.blocks.len(), 1);
        assert_eq!(
            parsed.blocks[0].content,
            "first line\nsecond line\nthird line"
        );
    }

    #[test]
    fn normalize_journal_day_basic() {
        assert_eq!(normalize_journal_day("2026_05_19"), "2026-05-19");
        assert_eq!(normalize_journal_day("2026-05-19"), "2026-05-19");
        assert_eq!(normalize_journal_day("Untitled"), "Untitled");
    }

    #[test]
    fn is_markdown_extension_check() {
        assert!(is_markdown(Path::new("foo.md")));
        assert!(is_markdown(Path::new("foo.MD")));
        assert!(is_markdown(Path::new("foo.markdown")));
        assert!(!is_markdown(Path::new("foo.txt")));
        assert!(!is_markdown(Path::new("foo")));
    }

    #[test]
    fn sort_key_for_index_lexicographic() {
        assert_eq!(sort_key_for_index(0), "aa");
        assert_eq!(sort_key_for_index(1), "ab");
        assert_eq!(sort_key_for_index(26), "ba");
        // Ordering matches numeric ordering.
        assert!(sort_key_for_index(0) < sort_key_for_index(1));
        assert!(sort_key_for_index(25) < sort_key_for_index(26));
    }

    #[tokio::test]
    async fn end_to_end_import_small_graph() {
        // Build a tiny on-disk graph and import it.
        let temp = std::env::temp_dir().join(format!(
            "task-graph-loader-test-{}",
            uuid::Uuid::new_v4().simple()
        ));
        let pages_dir = temp.join("pages");
        let journals_dir = temp.join("journals");
        std::fs::create_dir_all(&pages_dir).unwrap();
        std::fs::create_dir_all(&journals_dir).unwrap();
        std::fs::write(
            pages_dir.join("Welcome.md"),
            "title:: Welcome\nalias:: Home, Start\n\n- hello world\n  - nested block\n",
        )
        .unwrap();
        std::fs::write(
            journals_dir.join("2026_05_19.md"),
            "- journal entry\n  priority:: high\n",
        )
        .unwrap();

        let doc = Arc::new(CrdtDoc::ephemeral());
        let stats = import_logseq_graph(&doc, &temp).await.unwrap();
        assert_eq!(stats.vaults, 1);
        assert_eq!(stats.pages, 2);
        assert_eq!(stats.journals, 1);
        assert_eq!(stats.blocks, 3); // welcome's 2 + journal's 1
        assert!(
            stats.failures.is_empty(),
            "unexpected failures: {:?}",
            stats.failures
        );

        // Verify via repo that the journal day landed correctly.
        let pr = PageRepoLoro::new(&doc);
        let big = knowledge_proto::architect::Page {
            index: 0,
            size: 100,
        };
        let pages = pr.list(big, None, None).await.unwrap().items;
        let journal = pages.iter().find(|p| p.is_journal).expect("journal page");
        assert_eq!(journal.journal_day.as_deref(), Some("2026-05-19"));

        // Cleanup.
        let _ = std::fs::remove_dir_all(&temp);
    }
}
