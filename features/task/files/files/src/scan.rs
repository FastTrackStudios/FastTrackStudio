//! `checkpoint_now`'s full-scan enumeration (spec's "Session checkpoint
//! ... certified by a full scan"): walk the live tree and walk the
//! checkpoint head's tracked paths. [`crate::checkpoint`] turns the two
//! into a streamed, skip-unchanged commit.

use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use files_proto::RootFlavor;
use jj_lib::backend::{Backend, Tree, TreeValue};
use jj_lib::gitignore::GitIgnoreFile;
use jj_lib::repo_path::{RepoPath, RepoPathBuf};

use crate::consts::{GIT_DIR, MARKER_FILE, STORE_DIR};
use crate::error::{Error, Result};
use crate::ignore;

/// One regular file found in a root's live tree.
pub struct LiveFile {
    /// Root-relative jj path.
    pub repo_path: RepoPathBuf,
    /// Absolute path on disk.
    pub disk_path: PathBuf,
    /// Matched by the root's Ignore set ([`crate::ignore`]). Such a file
    /// is skipped by a checkpoint *unless it is already tracked* — an
    /// ignore pattern must never turn into a recorded deletion.
    pub ignored: bool,
}

/// Recursively list every regular file under `root_path`, as [`LiveFile`]
/// entries. Skips [`STORE_DIR`] / [`MARKER_FILE`] at *every* depth, not
/// just the root's own top level — a File Root's internals must never be
/// ingested as ordinary content even if they show up nested (e.g. a
/// rejected-but-still-on-disk nested root, or a manually copied
/// `.fts-files` directory; see PR #280 review finding on nested roots).
/// [`GIT_DIR`] joins that list on software roots, where it *is* the
/// root's object store (and where a nested one is a submodule's store,
/// which git itself doesn't track either). On a media root a `.git`
/// directory is ordinary content, versioned like anything else — the
/// media flavor is unchanged by this ticket. Symlinks are skipped (v1
/// has no symlink writer wired through yet).
///
/// `flavor` selects the Ignore set: its seed, and whether the tree's own
/// `.gitignore` files are chained in as the walk descends (software roots
/// only — see [`crate::ignore`]). Ignored *directories* are not descended
/// into at all, which is both correct gitignore semantics and what keeps a
/// stray `node_modules` from costing a full-tree stat walk.
pub fn walk_live_tree(root_path: &Path, flavor: RootFlavor) -> Result<Vec<LiveFile>> {
    let mut out = Vec::new();
    let ignores = ignore::seed(flavor)?;
    let ignores = chain_dir_gitignore(&ignores, RepoPath::root(), root_path, flavor)?;
    walk_dir(
        root_path,
        root_path,
        RepoPath::root(),
        &ignores,
        flavor,
        &mut out,
    )?;
    Ok(out)
}

/// Layer `dir`'s own `.gitignore` onto `parent`, on flavors that honor it.
fn chain_dir_gitignore(
    parent: &Arc<GitIgnoreFile>,
    prefix: &RepoPath,
    dir: &Path,
    flavor: RootFlavor,
) -> Result<Arc<GitIgnoreFile>> {
    if !ignore::honors_gitignore(flavor) {
        return Ok(parent.clone());
    }
    parent
        .chain_with_file(prefix, dir.join(".gitignore"))
        .map_err(|e| Error::Repo(format!("{}: reading .gitignore: {e}", dir.display())))
}

fn walk_dir(
    root_path: &Path,
    dir: &Path,
    dir_repo_path: &RepoPath,
    ignores: &Arc<GitIgnoreFile>,
    flavor: RootFlavor,
    out: &mut Vec<LiveFile>,
) -> Result<()> {
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let file_type = entry.file_type()?;
        let path = entry.path();
        let name = entry.file_name();
        if name == MARKER_FILE || name == STORE_DIR {
            continue;
        }
        if name == GIT_DIR && flavor == RootFlavor::Software {
            continue;
        }
        let Some(name_str) = name.to_str() else {
            continue; // non-UTF8 names are out of scope for v1
        };
        let Ok(component) = jj_lib::repo_path::RepoPathComponentBuf::new(name_str) else {
            continue;
        };
        let child_repo_path = dir_repo_path.join(&component);

        if file_type.is_dir() {
            if ignores.matches_dir(&child_repo_path) {
                // Ignored directory: not descended into (gitignore
                // semantics — every child is ignored too).
                continue;
            }
            let child_ignores = chain_dir_gitignore(ignores, &child_repo_path, &path, flavor)?;
            walk_dir(
                root_path,
                &path,
                &child_repo_path,
                &child_ignores,
                flavor,
                out,
            )?;
        } else if file_type.is_file() {
            out.push(LiveFile {
                ignored: ignores.matches_file(&child_repo_path),
                repo_path: child_repo_path,
                disk_path: path,
            });
        }
        // symlinks: skipped (see doc comment).
    }
    Ok(())
}

/// Recursively list every file path tracked in `tree` (root-relative jj
/// paths) — the checkpoint-head half of a checkpoint-now diff.
pub async fn walk_tree_paths(
    backend: &dyn Backend,
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
                let sub = backend.read_tree(&path, id).await?;
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

/// Look up `path` inside `tree`, descending through subtrees as needed.
/// Mirrors `task_files_version_store::chain::lookup` (that one is
/// `pub(crate)` to its own crate, and this ticket consumes the
/// version-store crate as-is rather than widening its visibility) —
/// small enough to duplicate. Written against `&dyn Backend` so it serves
/// both Root flavors (media's CAS backend and software's stock git one).
pub(crate) async fn lookup(
    backend: &dyn Backend,
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
    let mut prefix = RepoPathBuf::root();
    for component in dir.components() {
        prefix = prefix.join(component);
        match current.value(component) {
            Some(TreeValue::Tree(id)) => current = backend.read_tree(&prefix, id).await?,
            _ => return Ok(None),
        }
    }
    Ok(current.value(basename).cloned())
}
