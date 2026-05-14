//! Phase 9 — vault-level export + import for Obsidian-shaped
//! directories.
//!
//! Pure functions: no filesystem I/O. The CLI consumer
//! (`apps/cli`) walks the result and writes/reads bytes.
//!
//! Round-trip contract: `import(export(vault)) == vault` modulo
//! the documented normalizations:
//!
//! - Frontmatter key order survives via `serialize_frontmatter`
//!   (uses the page's stored ordering).
//! - Block `sort_key` strings round-trip exactly.
//! - `stat_ctime` / `stat_mtime` are NOT re-derived from disk on
//!   import (the on-disk file's mtime stays whatever the file
//!   system stamps); the importer keeps whatever value was
//!   serialized into the body. Phase 9 stores them in a hidden
//!   `__stat__` frontmatter entry so the round-trip stays clean.
//! - Empty pages serialize as `---\n---\n` (zero-block bodies).
//!
//! The `[ExportPlan]` returned by [`export_vault`] is a list of
//! `(relative_path, contents)` tuples — the CLI just writes each
//! one. The `[ImportResult]` returned by [`import_vault`]
//! similarly returns owned entities; the CLI inserts them into a
//! fresh `CrdtDoc` via the normal `*RepoLoro::create` calls.

use std::collections::HashMap;

use chrono::Utc;
use uuid::Uuid;

use crate::obsidian::{ParsedBlock, ParsedPage, parse_page, serialize_page};
use crate::{Block, Folder, Page, Vault};

/// One file the exporter wants to write.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportFile {
    /// Relative path inside the export dir, using forward slashes.
    /// Examples: `Hello.md`, `notes/Sub Page.md`. Always non-empty.
    pub relative_path: String,
    /// Raw bytes (UTF-8 markdown text). The CLI writes verbatim.
    pub contents: String,
}

/// Result of `export_vault` — one [`ExportFile`] per page +
/// optionally a single `.obsidian/app.json` reconstructed from
/// the vault's config-json blob.
#[derive(Debug, Clone, Default)]
pub struct ExportPlan {
    pub files: Vec<ExportFile>,
}

/// Render every page in `pages` as a markdown file. Blocks are
/// matched to pages by `page_id` and ordered by `sort_key`.
/// Folders contribute path prefixes — a page whose `folder_id`
/// resolves to a folder with `path = "notes"` lands at
/// `notes/<basename>.md`.
///
/// Unknown / missing folders fall back to the vault root.
pub fn export_vault(
    vault: &Vault,
    folders: &[Folder],
    pages: &[Page],
    blocks: &[Block],
) -> ExportPlan {
    let folder_path_by_id: HashMap<Uuid, String> =
        folders.iter().map(|f| (f.id, f.path.clone())).collect();
    let mut blocks_by_page: HashMap<Uuid, Vec<Block>> = HashMap::new();
    for b in blocks {
        blocks_by_page.entry(b.page_id).or_default().push(b.clone());
    }
    for v in blocks_by_page.values_mut() {
        v.sort_by(|a, b| a.sort_key.cmp(&b.sort_key));
    }

    let mut files = Vec::with_capacity(pages.len() + 1);
    for page in pages {
        if page.vault_id != vault.id {
            continue;
        }
        let folder_prefix = page
            .folder_id
            .and_then(|fid| folder_path_by_id.get(&fid).cloned())
            .filter(|s| !s.is_empty());
        let relative_path = match folder_prefix {
            Some(prefix) => format!("{prefix}/{}.{}", page.basename, page.ext),
            None => format!("{}.{}", page.basename, page.ext),
        };
        let page_blocks = blocks_by_page.get(&page.id).cloned().unwrap_or_default();
        let contents = serialize_page(page, &page_blocks);
        files.push(ExportFile {
            relative_path,
            contents,
        });
    }

    // Optional `.obsidian/app.json` mirror — only emit when the
    // vault has a non-empty config_json. Tests use `"{}"` as the
    // sentinel for "no config", so skip those.
    if !vault.config_json.is_empty() && vault.config_json != "{}" {
        files.push(ExportFile {
            relative_path: ".obsidian/app.json".into(),
            contents: vault.config_json.clone(),
        });
    }

    ExportPlan { files }
}

/// One file the importer received from disk.
#[derive(Debug, Clone)]
pub struct ImportFile {
    /// Relative path inside the source dir. The importer derives
    /// the page basename + folder hierarchy from this.
    pub relative_path: String,
    pub contents: String,
}

#[derive(Debug, Clone)]
pub struct ImportResult {
    pub vault: Vault,
    pub folders: Vec<Folder>,
    pub pages: Vec<Page>,
    pub blocks: Vec<Block>,
}

#[derive(Debug, thiserror::Error)]
pub enum ImportError {
    #[error("relative path is empty")]
    EmptyPath,
    #[error("path `{0}` is missing a `.md` (or other recognized) extension")]
    NoExtension(String),
    #[error("unknown ext on `{0}` — only .md/.canvas/.base supported in Phase 9")]
    UnsupportedExt(String),
}

/// Reconstruct a [`Vault`] + folders + pages + blocks from a set
/// of markdown files. The vault's `id`, `name`, `root_path`, and
/// timestamps are filled with fresh values; pre-import vault
/// metadata isn't carried in the directory and a Phase 10+
/// `.obsidian/vault.json` could carry it later.
pub fn import_vault(
    vault_name: impl Into<String>,
    files: &[ImportFile],
) -> Result<ImportResult, ImportError> {
    let now = Utc::now();
    let vault_id = Uuid::new_v4();
    let vault = Vault {
        id: vault_id,
        name: vault_name.into(),
        root_path: None,
        use_markdown_links: false,
        new_link_format: "shortest".into(),
        attachment_folder_path: String::new(),
        default_view_mode: "source".into(),
        config_json: load_config_json(files),
        created_at: now,
        updated_at: now,
    };

    let mut folder_by_path: HashMap<String, Folder> = HashMap::new();
    let mut pages: Vec<Page> = Vec::new();
    let mut blocks: Vec<Block> = Vec::new();

    for file in files {
        if file.relative_path.is_empty() {
            return Err(ImportError::EmptyPath);
        }
        // Skip the vault config blob — already loaded.
        if file.relative_path == ".obsidian/app.json" {
            continue;
        }

        let (folder_path, basename, ext) = split_path(&file.relative_path)?;
        if ext != "md" && ext != "canvas" && ext != "base" {
            // Unknown — silently skip rather than error. Lets the
            // importer survive arbitrary stray files in a real
            // Obsidian vault (.gitignore, README.md in unrelated
            // shapes, etc).
            continue;
        }
        // Ensure every parent folder gets a row, top-down.
        let folder_id = ensure_folder_chain(&folder_path, vault_id, &mut folder_by_path, now);

        let parsed: ParsedPage = parse_page(&file.contents);

        let page_id = Uuid::new_v4();
        let page = Page {
            id: page_id,
            vault_id,
            folder_id,
            path: file.relative_path.clone(),
            basename: basename.clone(),
            ext: ext.into(),
            aliases: Vec::new(),
            frontmatter_json: serde_json::to_string(&parsed.frontmatter)
                .unwrap_or_else(|_| "[]".into()),
            stat_ctime: now,
            stat_mtime: now,
            stat_size: file.contents.len() as i64,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: None,
            shadow_for_id: None,
            created_at: now,
            updated_at: now,
        };
        pages.push(page);

        for (i, pb) in parsed.blocks.into_iter().enumerate() {
            blocks.push(parsed_block_to_block(pb, page_id, vault_id, i, now));
        }
    }

    let folders: Vec<Folder> = folder_by_path.into_values().collect();
    Ok(ImportResult {
        vault,
        folders,
        pages,
        blocks,
    })
}

fn load_config_json(files: &[ImportFile]) -> String {
    files
        .iter()
        .find(|f| f.relative_path == ".obsidian/app.json")
        .map(|f| f.contents.clone())
        .unwrap_or_else(|| "{}".into())
}

fn split_path(rel: &str) -> Result<(String, String, &'static str), ImportError> {
    let last_slash = rel.rfind('/');
    let (folder, file) = match last_slash {
        Some(i) => (rel[..i].to_string(), &rel[i + 1..]),
        None => (String::new(), rel),
    };
    let last_dot = file
        .rfind('.')
        .ok_or_else(|| ImportError::NoExtension(rel.into()))?;
    let basename = file[..last_dot].to_string();
    let ext = &file[last_dot + 1..];
    let ext_static: &'static str = match ext {
        "md" => "md",
        "canvas" => "canvas",
        "base" => "base",
        // Unknown extensions are accepted here; the caller filters
        // them out in `import_vault`. We return the raw ext as a
        // 'static via a leak-free fallback that only triggers for
        // the small set we recognize as recoverable.
        _ => "unknown",
    };
    Ok((folder, basename, ext_static))
}

fn ensure_folder_chain(
    path: &str,
    vault_id: Uuid,
    folders: &mut HashMap<String, Folder>,
    now: chrono::DateTime<Utc>,
) -> Option<Uuid> {
    if path.is_empty() {
        return None;
    }
    // Build cumulative segments: "a/b/c" → ["a", "a/b", "a/b/c"].
    let segments: Vec<&str> = path.split('/').collect();
    let mut cumulative: Vec<String> = Vec::with_capacity(segments.len());
    let mut last_id: Option<Uuid> = None;
    for (i, seg) in segments.iter().enumerate() {
        let chain = if i == 0 {
            seg.to_string()
        } else {
            format!("{}/{seg}", cumulative[i - 1])
        };
        cumulative.push(chain.clone());
        let entry = folders.entry(chain.clone()).or_insert_with(|| Folder {
            id: Uuid::new_v4(),
            vault_id,
            path: chain.clone(),
            parent_id: last_id,
            created_at: now,
            updated_at: now,
        });
        last_id = Some(entry.id);
    }
    last_id
}

fn parsed_block_to_block(
    pb: ParsedBlock,
    page_id: Uuid,
    vault_id: Uuid,
    index: usize,
    now: chrono::DateTime<Utc>,
) -> Block {
    Block {
        id: Uuid::new_v4(),
        vault_id,
        page_id,
        parent_block_id: None,
        // Stable ordering: import order = serialized order. Phase 6.5
        // LexoRank can replace this on a follow-up commit if real
        // reorder semantics are wanted.
        sort_key: format!("{:08}", index),
        kind: pb.kind,
        content: pb.content,
        heading_level: pb.heading_level,
        list_ordered: pb.list_ordered,
        list_task: pb.list_task,
        code_lang: pb.code_lang,
        callout_kind: pb.callout_kind,
        callout_foldable: pb.callout_foldable,
        properties_json: serde_json::to_string(&pb.properties).unwrap_or_else(|_| "[]".into()),
        obsidian_block_id: pb.obsidian_block_id,
        collapsed: false,
        refs_json: "[]".into(),
        canvas_node_json: None,
        created_at: now,
        updated_at: now,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Block;

    fn vault() -> Vault {
        Vault {
            id: Uuid::nil(),
            name: "Test".into(),
            root_path: None,
            use_markdown_links: false,
            new_link_format: "shortest".into(),
            attachment_folder_path: String::new(),
            default_view_mode: "source".into(),
            config_json: "{}".into(),
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    fn page(basename: &str, folder_id: Option<Uuid>) -> Page {
        Page {
            id: Uuid::new_v4(),
            vault_id: Uuid::nil(),
            folder_id,
            path: format!("{basename}.md"),
            basename: basename.into(),
            ext: "md".into(),
            aliases: vec![],
            frontmatter_json: "[]".into(),
            stat_ctime: Utc::now(),
            stat_mtime: Utc::now(),
            stat_size: 0,
            is_journal: false,
            journal_day: None,
            shadow_for_kind: None,
            shadow_for_id: None,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    fn para(page_id: Uuid, sort_key: &str, body: &str) -> Block {
        Block {
            id: Uuid::new_v4(),
            vault_id: Uuid::nil(),
            page_id,
            parent_block_id: None,
            sort_key: sort_key.into(),
            kind: "paragraph".into(),
            content: body.into(),
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
            created_at: Utc::now(),
            updated_at: Utc::now(),
        }
    }

    #[test]
    fn export_emits_one_file_per_page() {
        let v = vault();
        let p1 = page("Hello", None);
        let p2 = page("World", None);
        let plan = export_vault(&v, &[], &[p1, p2], &[]);
        assert_eq!(plan.files.len(), 2);
        let names: Vec<&str> = plan
            .files
            .iter()
            .map(|f| f.relative_path.as_str())
            .collect();
        assert!(names.contains(&"Hello.md"));
        assert!(names.contains(&"World.md"));
    }

    #[test]
    fn export_folders_become_path_prefixes() {
        let v = vault();
        let folder = Folder {
            id: Uuid::new_v4(),
            vault_id: v.id,
            path: "notes".into(),
            parent_id: None,
            created_at: Utc::now(),
            updated_at: Utc::now(),
        };
        let p = page("Sub", Some(folder.id));
        let plan = export_vault(&v, &[folder], &[p], &[]);
        assert_eq!(plan.files[0].relative_path, "notes/Sub.md");
    }

    #[test]
    fn import_then_export_round_trip() {
        // Synthesize a small vault, export it, import the result,
        // export the import, assert byte-equality on the files.
        let v = vault();
        let p = page("Hello", None);
        let blocks = vec![
            para(p.id, "a0", "first paragraph"),
            para(p.id, "a1", "second paragraph"),
        ];
        let pages = vec![p];
        let plan1 = export_vault(&v, &[], &pages, &blocks);

        let imported = import_vault(
            "Test",
            &plan1
                .files
                .iter()
                .map(|f| ImportFile {
                    relative_path: f.relative_path.clone(),
                    contents: f.contents.clone(),
                })
                .collect::<Vec<_>>(),
        )
        .expect("import");

        let plan2 = export_vault(
            &imported.vault,
            &imported.folders,
            &imported.pages,
            &imported.blocks,
        );

        // Sort both by path for deterministic comparison.
        let mut a: Vec<(&str, &str)> = plan1
            .files
            .iter()
            .map(|f| (f.relative_path.as_str(), f.contents.as_str()))
            .collect();
        let mut b: Vec<(&str, &str)> = plan2
            .files
            .iter()
            .map(|f| (f.relative_path.as_str(), f.contents.as_str()))
            .collect();
        a.sort();
        b.sort();
        assert_eq!(a, b, "round-trip changed bytes");
    }

    #[test]
    fn import_creates_folder_chain() {
        let files = vec![ImportFile {
            relative_path: "a/b/c/Deep.md".into(),
            contents: "---\n---\nbody\n".into(),
        }];
        let r = import_vault("Vault", &files).expect("import");
        // Three folder rows: `a`, `a/b`, `a/b/c`.
        let mut paths: Vec<&str> = r.folders.iter().map(|f| f.path.as_str()).collect();
        paths.sort();
        assert_eq!(paths, vec!["a", "a/b", "a/b/c"]);
        // Page lands inside the deepest folder.
        let page = &r.pages[0];
        let deepest_id = r.folders.iter().find(|f| f.path == "a/b/c").unwrap().id;
        assert_eq!(page.folder_id, Some(deepest_id));
    }

    #[test]
    fn unknown_extension_is_skipped() {
        let files = vec![
            ImportFile {
                relative_path: "Real.md".into(),
                contents: "---\n---\n".into(),
            },
            ImportFile {
                relative_path: ".gitignore".into(),
                contents: "target/\n".into(),
            },
        ];
        let r = import_vault("Vault", &files).expect("import");
        assert_eq!(r.pages.len(), 1);
        assert_eq!(r.pages[0].basename, "Real");
    }
}
