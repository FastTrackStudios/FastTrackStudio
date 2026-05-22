//! `WikiLive` — the file-backed wiki handle.
//!
//! Single struct anchored at the **vault root**
//! (`<vault>/`). All operations route through helper
//! modules so the handle stays small. Concurrency: each
//! mutating method acquires a per-vault file lock by
//! convention (atomic temp+rename writes), so multiple
//! handles can coexist safely. No in-memory mutex.

use std::path::{Path, PathBuf};

use wiki_proto::paths;

use crate::error::WikiLiveError;

/// File-backed wiki handle. Cheap to clone (just wraps
/// a `PathBuf`).
#[derive(Debug, Clone)]
pub struct WikiLive {
    vault_root: PathBuf,
}

impl WikiLive {
    /// Open an existing wiki at `<vault_root>/Wiki/`. Does
    /// not validate structure — call [`Self::bootstrap`]
    /// or [`Self::is_bootstrapped`] if you need that.
    pub fn open(vault_root: impl Into<PathBuf>) -> Self {
        Self {
            vault_root: vault_root.into(),
        }
    }

    /// Vault root path.
    pub fn vault_root(&self) -> &Path {
        &self.vault_root
    }

    /// `<vault_root>/Wiki/`.
    pub fn wiki_root(&self) -> PathBuf {
        self.vault_root.join(paths::WIKI_ROOT)
    }

    /// Resolve a path relative to the wiki root, rejecting
    /// any escape via `..`.
    pub(crate) fn wiki_path(&self, rel: &str) -> Result<PathBuf, WikiLiveError> {
        let root = self.wiki_root();
        let full = root.join(rel);
        // Canonicalize the parent (it must exist for
        // canonicalize to succeed); the leaf can be new.
        let parent = full.parent().ok_or_else(|| WikiLiveError::PathEscape {
            path: rel.to_string(),
        })?;
        std::fs::create_dir_all(parent)?;
        let canon_parent = parent.canonicalize()?;
        let canon_root = root.canonicalize()?;
        if !canon_parent.starts_with(&canon_root) {
            return Err(WikiLiveError::PathEscape {
                path: rel.to_string(),
            });
        }
        Ok(canon_parent.join(full.file_name().unwrap_or_default()))
    }

    /// Has [`Self::bootstrap`] run for this vault?
    pub fn is_bootstrapped(&self) -> bool {
        let r = self.wiki_root();
        r.is_dir() && r.join(paths::SCHEMA_MD).is_file() && r.join(paths::PURPOSE_MD).is_file()
    }

    /// Scaffold the wiki on disk. Idempotent: existing
    /// files are kept; missing ones get defaults. Returns
    /// `Ok(true)` if anything was created, `Ok(false)`
    /// otherwise.
    pub fn bootstrap(&self) -> Result<bool, WikiLiveError> {
        crate::raw::bootstrap_dirs(self)?;
        let created_schema = crate::context::ensure_schema(self)?;
        let created_purpose = crate::context::ensure_purpose(self)?;
        let created_index = crate::index::ensure_index(self)?;
        let created_log = crate::log_md::ensure_log(self)?;
        Ok(created_schema || created_purpose || created_index || created_log)
    }
}
