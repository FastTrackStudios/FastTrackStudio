//! `checkpoint_now`'s full-scan diff (spec's "Session checkpoint ...
//! certified by a full scan"): walk the live tree, walk the checkpoint
//! head's tracked paths, and turn the difference into a
//! `checkpoint::checkpoint` change-list.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use jj_lib::backend::{Tree, TreeValue};
use jj_lib::repo_path::{RepoPath, RepoPathBuf};
use task_files_version_store::VersionStoreBackend;
use task_files_version_store::checkpoint::Change;

use crate::consts::{MARKER_FILE, STORE_DIR};
use crate::error::{Error, Result};

/// Recursively list every regular file under `root_path`, as
/// (root-relative jj path, absolute disk path) pairs. Skips this
/// root's own internals ([`STORE_DIR`], [`MARKER_FILE`]) and symlinks
/// (v1: `checkpoint::Change` has no symlink writer yet).
pub fn walk_live_tree(root_path: &Path) -> Result<Vec<(RepoPathBuf, PathBuf)>> {
    let mut out = Vec::new();
    walk_dir(root_path, root_path, &mut out)?;
    Ok(out)
}

fn walk_dir(root_path: &Path, dir: &Path, out: &mut Vec<(RepoPathBuf, PathBuf)>) -> Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let path = entry.path();
        let name = entry.file_name();
        if dir == root_path && (name == MARKER_FILE || name == STORE_DIR) {
            continue;
        }
        if file_type.is_dir() {
            walk_dir(root_path, &path, out)?;
        } else if file_type.is_file() {
            let rel = path
                .strip_prefix(root_path)
                .expect("walked path is under root_path");
            let Some(rel_str) = rel.to_str() else {
                continue; // non-UTF8 paths are out of scope for v1
            };
            let normalized = rel_str.replace(std::path::MAIN_SEPARATOR, "/");
            let repo_path = RepoPathBuf::from_internal_string(&normalized)
                .map_err(|e| Error::BadRequest(format!("{normalized:?}: {e}")))?;
            out.push((repo_path, path));
        }
        // symlinks: skipped (see doc comment).
    }
    Ok(())
}

/// Recursively list every file path tracked in `tree` (root-relative jj
/// paths) — the checkpoint-head half of a checkpoint-now diff.
pub async fn walk_tree_paths(
    backend: &VersionStoreBackend,
    tree: &Tree,
    prefix: &RepoPath,
    out: &mut BTreeSet<RepoPathBuf>,
) -> Result<()> {
    for name in tree.names() {
        let Some(value) = tree.value(name) else {
            continue;
        };
        let path = prefix.join(name);
        match value {
            TreeValue::Tree(id) => {
                let sub = backend.tree(id).await?;
                Box::pin(walk_tree_paths(backend, &sub, &path, out)).await?;
            }
            TreeValue::File { .. } => {
                out.insert(path);
            }
            _ => {}
        }
    }
    Ok(())
}

/// Diff the live tree (`disk_files`) against the checkpoint head's
/// tracked paths (`head_paths`) into a `checkpoint::checkpoint`
/// change-list: every file present on disk is (re-)written — the
/// underlying content-addressed store dedups a byte-identical write
/// against the previous state, so `chain::version_chain` records no
/// new entry for a file whose content didn't change (see
/// `VersionEntry`'s `is_new_state` logic in that crate) — and every
/// tracked path no longer on disk is removed. v1 detects no renames: a
/// moved file surfaces as a remove+write pair rather than a recorded
/// `CopyHistory` link (ADR 0001: "detection may start simple").
pub fn diff_to_changes(
    disk_files: &[(RepoPathBuf, PathBuf)],
    head_paths: &BTreeSet<RepoPathBuf>,
) -> Result<Vec<Change>> {
    let mut changes = Vec::new();
    let mut present: BTreeSet<RepoPathBuf> = BTreeSet::new();
    for (repo_path, disk_path) in disk_files {
        present.insert(repo_path.clone());
        let content = std::fs::read(disk_path)?;
        changes.push(Change::Write {
            path: repo_path.clone(),
            content,
        });
    }
    for path in head_paths {
        if !present.contains(path) {
            changes.push(Change::Remove { path: path.clone() });
        }
    }
    Ok(changes)
}
