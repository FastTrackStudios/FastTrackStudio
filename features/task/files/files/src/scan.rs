//! `checkpoint_now`'s full-scan enumeration (spec's "Session checkpoint
//! ... certified by a full scan"): walk the live tree and walk the
//! checkpoint head's tracked paths. [`crate::checkpoint`] turns the two
//! into a streamed, skip-unchanged commit.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use jj_lib::backend::{Tree, TreeValue};
use jj_lib::repo_path::{RepoPath, RepoPathBuf};
use task_files_version_store::VersionStoreBackend;

use crate::consts::{MARKER_FILE, STORE_DIR};
use crate::error::{Error, Result};
use crate::ignore::IgnoreSet;

/// Recursively list every regular file under `root_path` that the
/// root's Ignore set lets through, as (root-relative jj path, absolute
/// disk path) pairs.
///
/// This is the one gate between a live tree and the store, which is
/// why the Ignore set is applied *here* rather than downstream: a
/// pattern the set covers is never enumerated, so it can never be
/// hashed, never enter the CAS, and never appear in a commit tree
/// (glossary "Ignore set" — "neither versioned nor synced"). Ignored
/// directories are not descended into at all.
///
/// Skips [`STORE_DIR`] / [`MARKER_FILE`] at *every* depth, not just the
/// root's own top level — a File Root's internals must never be
/// ingested as ordinary content even if they show up nested (e.g. a
/// rejected-but-still-on-disk nested root, or a manually copied
/// `.fts-files` directory; see PR #280 review finding on nested
/// roots). Symlinks are skipped (v1 has no symlink writer wired
/// through yet).
pub fn walk_live_tree(root_path: &Path, ignore: &IgnoreSet) -> Result<Vec<(RepoPathBuf, PathBuf)>> {
    let mut out = Vec::new();
    walk_dir(root_path, root_path, ignore, &mut out)?;
    Ok(out)
}

/// `path`'s root-relative, `/`-separated form, or `None` when it isn't
/// representable (non-UTF8 names are out of scope for v1).
fn relative(root_path: &Path, path: &Path) -> Option<String> {
    let rel = path
        .strip_prefix(root_path)
        .expect("walked path is under root_path");
    Some(rel.to_str()?.replace(std::path::MAIN_SEPARATOR, "/"))
}

fn walk_dir(
    root_path: &Path,
    dir: &Path,
    ignore: &IgnoreSet,
    out: &mut Vec<(RepoPathBuf, PathBuf)>,
) -> Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let path = entry.path();
        let name = entry.file_name();
        if name == MARKER_FILE || name == STORE_DIR {
            continue;
        }
        let Some(rel) = relative(root_path, &path) else {
            continue;
        };
        if file_type.is_dir() {
            if ignore.is_ignored_dir(&rel) {
                continue;
            }
            walk_dir(root_path, &path, ignore, out)?;
        } else if file_type.is_file() {
            if ignore.is_ignored(&rel) {
                continue;
            }
            let repo_path = RepoPathBuf::from_internal_string(&rel)
                .map_err(|e| Error::BadRequest(format!("{rel:?}: {e}")))?;
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

/// Look up `path` inside `tree`, descending through subtrees as
/// needed. Mirrors `task_files_version_store::chain::lookup` (that
/// one is `pub(crate)` to its own crate, and this ticket consumes the
/// version-store crate as-is rather than widening its visibility) —
/// small enough to duplicate.
pub(crate) async fn lookup(
    backend: &VersionStoreBackend,
    tree: &Tree,
    path: &RepoPath,
) -> Result<Option<TreeValue>> {
    let Some((dir, basename)) = path.split() else {
        return Ok(None);
    };
    if dir.as_internal_file_string().is_empty() {
        return Ok(tree.value(basename).cloned());
    }
    let mut current = tree.clone();
    for component in dir.components() {
        match current.value(component) {
            Some(TreeValue::Tree(id)) => current = backend.tree(id).await?,
            _ => return Ok(None),
        }
    }
    Ok(current.value(basename).cloned())
}
