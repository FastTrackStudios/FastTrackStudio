//! In-memory snapshot of a vault root.

use std::path::{Path, PathBuf};
use std::time::SystemTime;

use thiserror::Error;

use crate::walker::walk_vault;

/// One `.md` file from the vault, loaded into memory. Raw bytes
/// are preserved verbatim — block / heading / link indexes are
/// computed lazily on top.
#[derive(Clone, Debug)]
pub struct VaultPage {
    /// Vault-relative, forward-slash separated, e.g.
    /// `Notes/2026/howdy.md`.
    pub rel_path: String,
    /// Filename without extension. Useful for `[[Wikilink]]`
    /// resolution.
    pub basename: String,
    /// Vault-relative parent directory; empty string at the root.
    pub folder: String,
    /// Raw file bytes at scan time. The editor sees this string;
    /// edits replace ranges within it.
    pub raw: String,
    /// `mtime` snapshot. The watcher uses it to detect external
    /// edits on reload.
    pub mtime: SystemTime,
}

/// In-memory snapshot of a vault root.
#[derive(Clone, Debug)]
pub struct Vault {
    pub root: PathBuf,
    pub pages: Vec<VaultPage>,
}

#[derive(Debug, Error)]
pub enum LoadError {
    #[error("io: {0}")]
    Io(String),
}

#[derive(Debug, Error)]
pub enum SaveError {
    #[error("io: {0}")]
    Io(String),
    #[error("page not found: {0}")]
    NotFound(String),
}

impl Vault {
    /// One-shot scan. Reads every `.md` file under `root` into
    /// memory. Dotfile / `.obsidian` / `.git` / `.trash` are
    /// skipped (see [`crate::walker::walk_vault`]).
    pub fn open(root: &Path) -> Result<Self, LoadError> {
        let entries = walk_vault(root);
        let mut pages = Vec::with_capacity(entries.len());
        for entry in entries {
            let raw = std::fs::read_to_string(&entry.abs_path)
                .map_err(|e| LoadError::Io(format!("{}: {}", entry.rel_path, e)))?;
            let mtime = std::fs::metadata(&entry.abs_path)
                .and_then(|m| m.modified())
                .unwrap_or(SystemTime::UNIX_EPOCH);
            let path = PathBuf::from(&entry.rel_path);
            let basename = path
                .file_stem()
                .and_then(|s| s.to_str())
                .unwrap_or(&entry.rel_path)
                .to_string();
            let folder = path
                .parent()
                .map(|p| p.to_string_lossy().replace('\\', "/"))
                .unwrap_or_default();
            pages.push(VaultPage {
                rel_path: entry.rel_path,
                basename,
                folder,
                raw,
                mtime,
            });
        }
        Ok(Self {
            root: root.to_path_buf(),
            pages,
        })
    }

    /// Find a page by basename (case-insensitive). Used by
    /// `[[Wikilink]]` resolution.
    pub fn page_by_basename(&self, basename: &str) -> Option<&VaultPage> {
        self.pages
            .iter()
            .find(|p| p.basename.eq_ignore_ascii_case(basename))
    }

    /// Find a page by vault-relative path.
    pub fn page_by_rel_path(&self, rel_path: &str) -> Option<&VaultPage> {
        self.pages.iter().find(|p| p.rel_path == rel_path)
    }
}
