//! `import_vault` — read each scanned file, parse it via the
//! `knowledge-proto` helpers, and produce in-memory entities ready
//! to be inserted into the Loro repos.

use crate::scan::{ScannedFile, ScannedFileKind, scan_vault_with};
use chrono::Utc;
use knowledge_proto::canvas::{
    Anchor, CanvasNode, CanvasShape, ConnectorEnd, ConnectorStyle, encode as canvas_encode,
    parse_obsidian_canvas,
};
use knowledge_proto::obsidian::{ParsedBlock, parse_page, translate_logseq_block_refs};
use knowledge_proto::{Base, Block, Folder, KnowledgeTag, Page, Ref, Vault};
use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::Path;
use uuid::Uuid;

#[derive(Debug)]
pub struct ImportResult {
    pub vault: Vault,
    pub folders: Vec<Folder>,
    pub pages: Vec<Page>,
    pub blocks: Vec<Block>,
    pub tags: Vec<KnowledgeTag>,
    pub bases: Vec<Base>,
}

#[derive(Debug, Clone)]
pub struct ImportOptions {
    pub name: String,
    pub include_obsidian_config: bool,
    pub include_canvas: bool,
    pub include_bases: bool,
}

impl Default for ImportOptions {
    fn default() -> Self {
        Self {
            name: "vault".into(),
            include_obsidian_config: false,
            include_canvas: true,
            include_bases: true,
        }
    }
}

#[derive(thiserror::Error, Debug)]
pub enum ImportError {
    #[error("io: {0}")]
    Io(#[from] std::io::Error),
    #[error("parse error in {path}: {reason}")]
    Parse { path: String, reason: String },
    #[error("not a directory: {0}")]
    NotADirectory(String),
}

pub fn import_vault(root: &Path, opts: ImportOptions) -> Result<ImportResult, ImportError> {
    let scanned = scan_vault_with(root, opts.include_obsidian_config)?;

    // ── Vault ─────────────────────────────────────────────────────
    let mut vault = Vault {
        id: Uuid::new_v4(),
        name: opts.name.clone(),
        root_path: Some(root.to_string_lossy().into_owned()),
        use_markdown_links: false,
        new_link_format: "shortest".into(),
        attachment_folder_path: String::new(),
        default_view_mode: "live-preview".into(),
        config_json: String::new(),
        created_at: Utc::now(),
        updated_at: Utc::now(),
    };
    let app_json_path = root.join(".obsidian").join("app.json");
    if app_json_path.is_file() {
        if let Ok(raw) = std::fs::read_to_string(&app_json_path) {
            if let Ok(v) = serde_json::from_str::<serde_json::Value>(&raw) {
                if let Some(b) = v.get("useMarkdownLinks").and_then(|x| x.as_bool()) {
                    vault.use_markdown_links = b;
                }
                if let Some(s) = v.get("newLinkFormat").and_then(|x| x.as_str()) {
                    vault.new_link_format = s.into();
                }
                if let Some(s) = v.get("attachmentFolderPath").and_then(|x| x.as_str()) {
                    vault.attachment_folder_path = s.into();
                }
            }
            vault.config_json = raw;
        }
    }

    // Detect Logseq vault — presence of `logseq/config.edn`.
    let is_logseq = root.join("logseq").join("config.edn").is_file();

    // ── Folders ──────────────────────────────────────────────────
    let mut folders: Vec<Folder> = Vec::new();
    let mut folder_by_path: HashMap<String, Uuid> = HashMap::new();
    for sf in scanned.iter().filter(|s| s.kind == ScannedFileKind::Folder) {
        let path = sf.vault_relative.clone();
        if path.is_empty() {
            continue;
        }
        let parent_id = path
            .rsplit_once('/')
            .and_then(|(parent, _)| folder_by_path.get(parent).copied());
        let id = Uuid::new_v4();
        folder_by_path.insert(path.clone(), id);
        folders.push(Folder {
            id,
            vault_id: vault.id,
            path,
            parent_id,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        });
    }

    // ── Pages + Blocks (markdown) ────────────────────────────────
    let mut pages: Vec<Page> = Vec::new();
    let mut blocks: Vec<Block> = Vec::new();
    let mut tag_set: HashSet<String> = HashSet::new();
    // Resolution table for .canvas `file` nodes — basename → page id.
    let mut page_by_basename: HashMap<String, Uuid> = HashMap::new();

    for sf in &scanned {
        if sf.kind != ScannedFileKind::Markdown {
            continue;
        }
        let src = match std::fs::read_to_string(&sf.absolute_path) {
            Ok(s) => s,
            Err(e) => {
                tracing::warn!("read failed {}: {e}", sf.vault_relative);
                continue;
            }
        };
        let parsed = parse_page(&src);

        let basename = strip_md_ext(&sf.vault_relative).to_string();
        let basename_only = basename
            .rsplit_once('/')
            .map(|(_, b)| b.to_string())
            .unwrap_or_else(|| basename.clone());
        let folder_path = sf
            .vault_relative
            .rsplit_once('/')
            .map(|(d, _)| d.to_string())
            .unwrap_or_default();
        let folder_id = if folder_path.is_empty() {
            None
        } else {
            folder_by_path.get(&folder_path).copied()
        };

        // Frontmatter — flatten to IndexMap<String, Value> for JSON.
        let mut fm_map: indexmap::IndexMap<String, serde_json::Value> = indexmap::IndexMap::new();
        for e in &parsed.frontmatter {
            fm_map.insert(e.key.clone(), e.value.clone());
        }
        let aliases = fm_map
            .get("aliases")
            .and_then(|v| v.as_array().cloned())
            .map(|arr| {
                arr.into_iter()
                    .filter_map(|v| v.as_str().map(str::to_string))
                    .collect()
            })
            .unwrap_or_default();

        let page_id = Uuid::new_v4();
        let page = Page {
            id: page_id,
            vault_id: vault.id,
            folder_id,
            path: sf.vault_relative.clone(),
            basename: basename_only.clone(),
            ext: "md".into(),
            aliases,
            frontmatter_json: serde_json::to_string(&fm_map).unwrap_or_default(),
            stat_ctime: sf.stat_ctime,
            stat_mtime: sf.stat_mtime,
            stat_size: sf.stat_size,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: None,
            shadow_for_id: None,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        };
        page_by_basename
            .entry(basename_only.clone())
            .or_insert(page_id);
        pages.push(page);

        // Blocks — walk parsed.blocks turning indent_depth into a
        // parent chain. Naive fractional-index sort_keys ("0.001",
        // "0.002", …). v1 limitation — upgrade to a real
        // fractional-indexing crate once order edits become a thing.
        let mut ancestors: Vec<(u32, Uuid)> = Vec::new();
        for (i, pb) in parsed.blocks.iter().enumerate() {
            // Maybe translate Logseq `((uuid))` refs in the content
            // first so refs_json reflects the rewritten body. The
            // helper's signature wants `&[Block]` — we pass an empty
            // slice because cross-block resolution needs a second
            // pass over the full vault, which v1 skips.
            let raw_content: String = if is_logseq {
                translate_logseq_block_refs(&pb.content, &[])
            } else {
                pb.content.clone()
            };
            let refs_vec: Vec<Ref> = knowledge_proto::obsidian::extract_refs(&raw_content);
            for r in &refs_vec {
                if let Ref::Tag(t) = r {
                    tag_set.insert(t.path.join("/"));
                }
            }

            while let Some((d, _)) = ancestors.last() {
                if *d >= pb.indent_depth {
                    ancestors.pop();
                } else {
                    break;
                }
            }
            let parent = ancestors.last().map(|(_, id)| *id);
            let id = Uuid::new_v4();
            let sort_key = format!("{:0>6}", i + 1);
            let mut props_map: indexmap::IndexMap<String, serde_json::Value> =
                indexmap::IndexMap::new();
            for e in &pb.properties {
                props_map.insert(e.key.clone(), e.value.clone());
            }
            blocks.push(Block {
                id,
                vault_id: vault.id,
                page_id,
                parent_block_id: parent,
                sort_key,
                kind: pb.kind.clone(),
                content: raw_content,
                heading_level: pb.heading_level,
                list_ordered: pb.list_ordered,
                list_task: pb.list_task.clone(),
                code_lang: pb.code_lang.clone(),
                callout_kind: pb.callout_kind.clone(),
                callout_foldable: pb.callout_foldable,
                properties_json: serde_json::to_string(&props_map).unwrap_or_default(),
                obsidian_block_id: pb.obsidian_block_id.clone(),
                collapsed: false,
                refs_json: serde_json::to_string(&refs_vec).unwrap_or_default(),
                canvas_node_json: None,
                created_at: Utc::now(),
                updated_at: Utc::now(),
            });
            ancestors.push((pb.indent_depth, id));
            let _ = ParsedBlock::default; // keep import used
        }
    }

    // ── Canvas pages ─────────────────────────────────────────────
    if opts.include_canvas {
        for sf in scanned.iter().filter(|s| s.kind == ScannedFileKind::Canvas) {
            let src = match std::fs::read_to_string(&sf.absolute_path) {
                Ok(s) => s,
                Err(e) => {
                    tracing::warn!("read failed {}: {e}", sf.vault_relative);
                    continue;
                }
            };
            let canvas = match parse_obsidian_canvas(&src) {
                Ok(c) => c,
                Err(e) => {
                    tracing::warn!("canvas parse failed {}: {e}", sf.vault_relative);
                    continue;
                }
            };

            let basename = strip_canvas_ext(&sf.vault_relative).to_string();
            let basename_only = basename
                .rsplit_once('/')
                .map(|(_, b)| b.to_string())
                .unwrap_or_else(|| basename.clone());
            let folder_path = sf
                .vault_relative
                .rsplit_once('/')
                .map(|(d, _)| d.to_string())
                .unwrap_or_default();
            let folder_id = if folder_path.is_empty() {
                None
            } else {
                folder_by_path.get(&folder_path).copied()
            };
            let page_id = Uuid::new_v4();
            pages.push(Page {
                id: page_id,
                vault_id: vault.id,
                folder_id,
                path: sf.vault_relative.clone(),
                basename: basename_only,
                ext: "canvas".into(),
                aliases: Vec::new(),
                frontmatter_json: String::new(),
                stat_ctime: sf.stat_ctime,
                stat_mtime: sf.stat_mtime,
                stat_size: sf.stat_size,
                is_journal: false,
                journal_day: None,
                shadow_for_kind: None,
                shadow_for_id: None,
                created_at: Utc::now(),
                updated_at: Utc::now(),
            });

            // Map node-id → block-id so edges resolve.
            let mut node_to_block: BTreeMap<String, Uuid> = BTreeMap::new();
            for (i, node) in canvas.nodes.iter().enumerate() {
                let block_id = Uuid::new_v4();
                node_to_block.insert(node.id.clone(), block_id);

                let (kind, content, shape): (String, String, CanvasShape) = match node.kind.as_str()
                {
                    "text" => (
                        "paragraph".into(),
                        node.text.clone().unwrap_or_default(),
                        CanvasShape::Text,
                    ),
                    "file" => {
                        let target_basename = node
                            .file
                            .as_deref()
                            .and_then(|p| {
                                Path::new(p)
                                    .file_stem()
                                    .and_then(|s| s.to_str())
                                    .map(str::to_string)
                            })
                            .unwrap_or_default();
                        let shape = match page_by_basename.get(&target_basename) {
                            Some(pid) => CanvasShape::PageRef {
                                target_page_id: *pid,
                            },
                            None => {
                                tracing::warn!(
                                    "canvas file node {:?} unresolved (basename {:?})",
                                    node.id,
                                    target_basename
                                );
                                let synthetic = Uuid::new_v4();
                                CanvasShape::BlockRef {
                                    target_block_id: synthetic,
                                    view: Default::default(),
                                }
                            }
                        };
                        ("paragraph".into(), String::new(), shape)
                    }
                    "link" => (
                        "paragraph".into(),
                        node.url.clone().unwrap_or_default(),
                        CanvasShape::Text,
                    ),
                    "group" => (
                        "paragraph".into(),
                        node.label.clone().unwrap_or_default(),
                        CanvasShape::Group,
                    ),
                    other => {
                        tracing::warn!("unknown canvas node type {other:?}");
                        ("paragraph".into(), String::new(), CanvasShape::Text)
                    }
                };
                let cn = CanvasNode {
                    x: node.x,
                    y: node.y,
                    w: node.width,
                    h: node.height,
                    z: i as i32,
                    rotation: 0.0,
                    shape,
                    color: node.color.clone(),
                    locked: false,
                    extras_json: None,
                };
                blocks.push(Block {
                    id: block_id,
                    vault_id: vault.id,
                    page_id,
                    parent_block_id: None,
                    sort_key: format!("{:0>6}", i + 1),
                    kind,
                    content,
                    heading_level: None,
                    list_ordered: false,
                    list_task: None,
                    code_lang: None,
                    callout_kind: None,
                    callout_foldable: false,
                    properties_json: String::new(),
                    obsidian_block_id: None,
                    collapsed: false,
                    refs_json: String::new(),
                    canvas_node_json: Some(canvas_encode(&cn)),
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                });
            }

            for (i, edge) in canvas.edges.iter().enumerate() {
                let from_id = match node_to_block.get(&edge.from_node) {
                    Some(id) => *id,
                    None => continue,
                };
                let to_id = match node_to_block.get(&edge.to_node) {
                    Some(id) => *id,
                    None => continue,
                };
                let connector = CanvasShape::Connector {
                    from: ConnectorEnd {
                        block_id: from_id,
                        anchor: anchor_from_side(edge.from_side.as_deref()),
                    },
                    to: ConnectorEnd {
                        block_id: to_id,
                        anchor: anchor_from_side(edge.to_side.as_deref()),
                    },
                    style: ConnectorStyle::default(),
                    label: edge.label.clone(),
                };
                let cn = CanvasNode {
                    x: 0.0,
                    y: 0.0,
                    w: 0.0,
                    h: 0.0,
                    z: 0,
                    rotation: 0.0,
                    shape: connector,
                    color: edge.color.clone(),
                    locked: false,
                    extras_json: None,
                };
                blocks.push(Block {
                    id: Uuid::new_v4(),
                    vault_id: vault.id,
                    page_id,
                    parent_block_id: None,
                    sort_key: format!("e{:0>6}", i + 1),
                    kind: "canvas".into(),
                    content: String::new(),
                    heading_level: None,
                    list_ordered: false,
                    list_task: None,
                    code_lang: None,
                    callout_kind: None,
                    callout_foldable: false,
                    properties_json: String::new(),
                    obsidian_block_id: None,
                    collapsed: false,
                    refs_json: String::new(),
                    canvas_node_json: Some(canvas_encode(&cn)),
                    created_at: Utc::now(),
                    updated_at: Utc::now(),
                });
            }
        }
    }

    // ── Bases ────────────────────────────────────────────────────
    let mut bases: Vec<Base> = Vec::new();
    if opts.include_bases {
        for sf in scanned.iter().filter(|s| s.kind == ScannedFileKind::Base) {
            let src = match std::fs::read_to_string(&sf.absolute_path) {
                Ok(s) => s,
                Err(e) => {
                    tracing::warn!("read failed {}: {e}", sf.vault_relative);
                    continue;
                }
            };
            let name = Path::new(&sf.vault_relative)
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("untitled")
                .to_string();
            let (parsed_filter_json, parsed_views_json) = match knowledge_proto::bases::parse(&src)
            {
                Ok(p) => (
                    serde_json::to_string(&p.global_filter).unwrap_or_default(),
                    serde_json::to_string(&p.views).unwrap_or_default(),
                ),
                Err(e) => {
                    tracing::warn!("base parse failed {}: {e}", sf.vault_relative);
                    (String::new(), String::new())
                }
            };
            bases.push(Base {
                id: Uuid::new_v4(),
                vault_id: vault.id,
                page_id: None,
                name,
                definition_yaml: src,
                parsed_filter_json,
                parsed_views_json,
                created_at: Utc::now(),
                updated_at: Utc::now(),
            });
        }
    }

    // ── Tags ─────────────────────────────────────────────────────
    let mut tags: Vec<KnowledgeTag> = tag_set
        .into_iter()
        .map(|tag| KnowledgeTag {
            id: Uuid::new_v4(),
            vault_id: vault.id,
            tag,
            color: None,
            description: None,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        })
        .collect();
    tags.sort_by(|a, b| a.tag.cmp(&b.tag));

    Ok(ImportResult {
        vault,
        folders,
        pages,
        blocks,
        tags,
        bases,
    })
}

fn strip_md_ext(p: &str) -> &str {
    p.strip_suffix(".md").unwrap_or(p)
}

fn strip_canvas_ext(p: &str) -> &str {
    p.strip_suffix(".canvas").unwrap_or(p)
}

fn anchor_from_side(s: Option<&str>) -> Anchor {
    match s {
        Some("top") => Anchor::Top,
        Some("right") => Anchor::Right,
        Some("bottom") => Anchor::Bottom,
        Some("left") => Anchor::Left,
        _ => Anchor::Auto,
    }
}

// Used by the scan helper to satisfy the scan_vault re-export with the
// configurable include_obsidian_config knob.
#[allow(dead_code)]
fn _unused(_: ScannedFile) {}

// ── Tests ────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::tempdir;

    fn write(root: &Path, rel: &str, content: &str) {
        let path = root.join(rel);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(&path, content).unwrap();
    }

    #[test]
    fn imports_small_markdown_vault() {
        let dir = tempdir().unwrap();
        let root = dir.path();
        write(
            root,
            "Inbox/Welcome.md",
            "---\ntags: [hello]\n---\n# Welcome\n\nA paragraph linking to [[Other]] and #demo.",
        );
        write(root, "Inbox/Other.md", "Just content.");
        write(root, "Inbox/Sub/Deep.md", "- nested\n  - more\n");
        write(root, "RootDoc.md", "Top-level page");
        write(
            root,
            "Notes/Tagged.md",
            "Has #project/alpha and #project/beta tags",
        );

        let r = import_vault(
            root,
            ImportOptions {
                name: "Test".into(),
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!(r.pages.len(), 5);
        // Folders: Inbox, Inbox/Sub, Notes
        assert!(r.folders.iter().any(|f| f.path == "Inbox"));
        assert!(r.folders.iter().any(|f| f.path == "Inbox/Sub"));
        assert!(r.folders.iter().any(|f| f.path == "Notes"));

        // Tags collected
        let tag_set: std::collections::HashSet<_> = r.tags.iter().map(|t| t.tag.clone()).collect();
        assert!(tag_set.contains("demo"));
        assert!(tag_set.contains("project/alpha"));
        assert!(tag_set.contains("project/beta"));

        // Welcome page has a wikilink ref
        let welcome = r
            .pages
            .iter()
            .find(|p| p.basename == "Welcome")
            .expect("welcome page");
        let welcome_blocks: Vec<_> = r
            .blocks
            .iter()
            .filter(|b| b.page_id == welcome.id)
            .collect();
        let has_other_ref = welcome_blocks.iter().any(|b| b.refs_json.contains("Other"));
        assert!(has_other_ref, "wikilink to Other not extracted");
    }

    #[test]
    fn imports_canvas_with_edges() {
        let dir = tempdir().unwrap();
        let root = dir.path();
        // Two text nodes, one file node (resolves to an existing page),
        // one edge.
        write(root, "Target.md", "I am a target.");
        let canvas_json = r#"{
            "nodes": [
                {"id": "a", "type": "text", "text": "first", "x": 0, "y": 0, "width": 100, "height": 50},
                {"id": "b", "type": "text", "text": "second", "x": 200, "y": 0, "width": 100, "height": 50},
                {"id": "c", "type": "file", "file": "Target.md", "x": 400, "y": 0, "width": 100, "height": 50}
            ],
            "edges": [
                {"id": "e1", "fromNode": "a", "toNode": "b"}
            ]
        }"#;
        write(root, "Map.canvas", canvas_json);

        let r = import_vault(
            root,
            ImportOptions {
                name: "Test".into(),
                include_canvas: true,
                ..Default::default()
            },
        )
        .unwrap();
        let canvas_page = r
            .pages
            .iter()
            .find(|p| p.ext == "canvas")
            .expect("canvas page");
        let canvas_blocks: Vec<_> = r
            .blocks
            .iter()
            .filter(|b| b.page_id == canvas_page.id)
            .collect();
        // 3 nodes + 1 edge = 4 blocks.
        assert_eq!(canvas_blocks.len(), 4);
        let connectors = canvas_blocks
            .iter()
            .filter(|b| {
                b.canvas_node_json
                    .as_deref()
                    .map(|j| j.contains("\"Connector\""))
                    .unwrap_or(false)
            })
            .count();
        assert_eq!(connectors, 1);
    }

    #[test]
    fn imports_base_file() {
        let dir = tempdir().unwrap();
        let root = dir.path();
        let yaml = r#"
filters: 'status == "done"'
views:
  - type: table
    name: "Done"
    order:
      - file.name
"#;
        write(root, "Tasks.base", yaml);

        let r = import_vault(
            root,
            ImportOptions {
                name: "Test".into(),
                include_bases: true,
                ..Default::default()
            },
        )
        .unwrap();
        assert_eq!(r.bases.len(), 1);
        let b = &r.bases[0];
        assert_eq!(b.name, "Tasks");
        assert!(!b.definition_yaml.is_empty());
        assert!(!b.parsed_views_json.is_empty());
    }
}
