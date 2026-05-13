//! `scan_vault` — walk a vault root and gather file metadata. Pure
//! filesystem layer; never opens a file's contents.

use chrono::{DateTime, Utc};
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use walkdir::WalkDir;

#[derive(Debug, Clone)]
pub struct ScannedFile {
    pub absolute_path: PathBuf,
    /// Vault-relative path, forward-slash normalized.
    pub vault_relative: String,
    pub stat_ctime: DateTime<Utc>,
    pub stat_mtime: DateTime<Utc>,
    pub stat_size: i64,
    pub kind: ScannedFileKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScannedFileKind {
    Markdown,
    Canvas,
    Base,
    Folder,
    Other,
}

pub fn scan_vault(root: &Path) -> Result<Vec<ScannedFile>, crate::ImportError> {
    scan_vault_with(root, false)
}

pub(crate) fn scan_vault_with(
    root: &Path,
    include_obsidian_config: bool,
) -> Result<Vec<ScannedFile>, crate::ImportError> {
    if !root.is_dir() {
        return Err(crate::ImportError::NotADirectory(
            root.to_string_lossy().into_owned(),
        ));
    }
    let root = root.canonicalize().unwrap_or_else(|_| root.to_path_buf());
    let mut out = Vec::new();
    for entry in WalkDir::new(&root)
        .follow_links(false)
        .into_iter()
        .filter_entry(|e| keep_entry(e, &root, include_obsidian_config))
    {
        let entry = match entry {
            Ok(e) => e,
            Err(e) => {
                tracing::warn!("walkdir error: {e}");
                continue;
            }
        };
        let abs = entry.path();
        if abs == root {
            continue;
        }
        let rel = match abs.strip_prefix(&root) {
            Ok(r) => r,
            Err(_) => continue,
        };
        let rel_str = rel
            .to_string_lossy()
            .replace(std::path::MAIN_SEPARATOR, "/");
        let meta = match entry.metadata() {
            Ok(m) => m,
            Err(e) => {
                tracing::warn!("metadata error on {}: {e}", abs.display());
                continue;
            }
        };
        let stat_mtime: DateTime<Utc> = meta
            .modified()
            .ok()
            .map(systemtime_to_utc)
            .unwrap_or_else(Utc::now);
        let stat_ctime: DateTime<Utc> = meta
            .created()
            .ok()
            .map(systemtime_to_utc)
            .unwrap_or(stat_mtime);
        let stat_size = meta.len() as i64;
        let kind = if meta.is_dir() {
            ScannedFileKind::Folder
        } else {
            classify(abs)
        };
        out.push(ScannedFile {
            absolute_path: abs.to_path_buf(),
            vault_relative: rel_str,
            stat_ctime,
            stat_mtime,
            stat_size,
            kind,
        });
    }
    Ok(out)
}

fn systemtime_to_utc(t: SystemTime) -> DateTime<Utc> {
    DateTime::<Utc>::from(t)
}

fn keep_entry(entry: &walkdir::DirEntry, root: &Path, include_obsidian_config: bool) -> bool {
    let p = entry.path();
    if p == root {
        return true;
    }
    let name = match p.file_name().and_then(|n| n.to_str()) {
        Some(n) => n,
        None => return false,
    };
    // Skip `.obsidian/` unless explicitly requested.
    if name == ".obsidian" && !include_obsidian_config {
        return false;
    }
    // Skip other hidden files / dirs starting with `.` — but allow
    // .canvas and .base extensions (those start with a letter after
    // the dot at the filename root).
    if name.starts_with('.') && name != ".obsidian" {
        return false;
    }
    true
}

fn classify(path: &Path) -> ScannedFileKind {
    match path.extension().and_then(|e| e.to_str()) {
        Some("md") => ScannedFileKind::Markdown,
        Some("canvas") => ScannedFileKind::Canvas,
        Some("base") => ScannedFileKind::Base,
        _ => ScannedFileKind::Other,
    }
}
