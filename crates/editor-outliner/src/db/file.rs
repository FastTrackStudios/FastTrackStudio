//! Disk I/O for the vault. Reads every `.md` under
//! `<root>/{pages,journals}/` and rebuilds a [`Vault`] from them.
//! Writes are the inverse — given a [`Page`] + its blocks, emit
//! the markdown back to disk.
//!
//! Markdown shape we read + write (matches Logseq's edn-free
//! native format):
//!
//! ```text
//! - Top-level block
//!   id:: 11111111-1111-1111-1111-111111111111
//!   key:: value
//!   - Child block
//!     id:: 22222222-2222-2222-2222-222222222222
//! ```

use std::path::Path;
use uuid::Uuid;

use super::{Block, Page, Vault};
use crate::format::{parse_page, serialize_page};

/// Load the entire vault from `<root>/{pages,journals}/`. Pages
/// without files become empty pages; missing dirs are silently
/// skipped. Errors short-circuit the whole load — we'd rather
/// surface "the vault is broken" than partial state.
#[cfg(not(target_arch = "wasm32"))]
pub fn load_vault(root: &Path) -> std::io::Result<Vault> {
    let mut vault = Vault::default();
    for (dir, is_journal) in [(root.join("pages"), false), (root.join("journals"), true)] {
        if !dir.exists() {
            continue;
        }
        for entry in std::fs::read_dir(&dir)? {
            let entry = entry?;
            let path = entry.path();
            if path.extension().and_then(|s| s.to_str()) != Some("md") {
                continue;
            }
            let raw = std::fs::read_to_string(&path)?;
            let basename = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or("Untitled")
                .to_string();
            let relpath = format!(
                "{}/{}",
                if is_journal { "journals" } else { "pages" },
                path.file_name().and_then(|s| s.to_str()).unwrap_or("")
            );
            let (page, blocks) = parse_page(&raw, &basename, &relpath, is_journal);
            vault.pages.push(page);
            vault.blocks.extend(blocks);
        }
    }
    Ok(vault)
}

#[cfg(target_arch = "wasm32")]
pub fn load_vault(_root: &Path) -> std::io::Result<Vault> {
    Ok(Vault::default())
}

/// Write one page (and only that page's blocks) back to disk.
/// The page's `relpath` is treated as `<root>/<relpath>` — the
/// caller passes the root.
#[cfg(not(target_arch = "wasm32"))]
pub fn write_page(root: &Path, page: &Page, blocks: &[Block]) -> std::io::Result<()> {
    let path = root.join(&page.relpath);
    if let Some(parent) = path.parent() {
        std::fs::create_dir_all(parent)?;
    }
    let body = serialize_page(page, blocks);
    write_atomic(&path, body.as_bytes())
}

#[cfg(target_arch = "wasm32")]
pub fn write_page(_root: &Path, _page: &Page, _blocks: &[Block]) -> std::io::Result<()> {
    Ok(())
}

/// Write every page in the vault to disk. Used on initial seed
/// and when the watcher reports a full reload.
#[cfg(not(target_arch = "wasm32"))]
pub fn write_vault(root: &Path, vault: &Vault) -> std::io::Result<()> {
    for page in &vault.pages {
        let blocks: Vec<Block> = vault
            .blocks
            .iter()
            .filter(|b| b.page_id == page.id)
            .cloned()
            .collect();
        write_page(root, page, &blocks)?;
    }
    Ok(())
}

#[cfg(target_arch = "wasm32")]
pub fn write_vault(_root: &Path, _vault: &Vault) -> std::io::Result<()> {
    Ok(())
}

/// Atomic write: tmp file + rename. Avoids the watcher seeing a
/// half-written file when we flush.
#[cfg(not(target_arch = "wasm32"))]
fn write_atomic(target: &Path, body: &[u8]) -> std::io::Result<()> {
    let parent = target.parent().unwrap_or_else(|| Path::new("."));
    let tmp = parent.join(format!(
        ".{}.{}.tmp",
        target
            .file_name()
            .and_then(|s| s.to_str())
            .unwrap_or("page"),
        Uuid::new_v4().simple()
    ));
    std::fs::write(&tmp, body)?;
    std::fs::rename(&tmp, target)
}
