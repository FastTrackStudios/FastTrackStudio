//! Badge derivation for root browsing (issue #266): the
//! resident-vs-stub and divergence state
//! [`files_proto::BrowseEntry`] carries, plus the root's Project
//! Version badge.
//!
//! Both entry badges are *derived from the version store*, never
//! stored twice (ADR 0001's "derived, never a second authority"
//! doctrine, the same rule per-file chains follow):
//!
//! - **Stub** — a path the checkpoint head tracks that is not resident
//!   in the live tree. Browsing lists it with its version-store
//!   identity so a 240 GB project can be explored without hydrating it
//!   (glossary "Pointer stub"; on-demand hydration is issue #263).
//! - **Divergent** — the root's op log has more than one visible head
//!   and this path's content differs between them (glossary "Divergent
//!   versions": concurrent saves survive side by side). Resolution
//!   (pick A / pick B / keep both) is issue #267.
//!
//! The Project Version badge is read from the root's marker file — the
//! Vault-entity half (naming, lineage, restart) is issue #261; this is
//! the wire shape the explorer renders today.

use std::collections::{BTreeMap, BTreeSet};
use std::path::Path;

use files_proto::{BrowseEntry, ProjectVersionBadge};
use jj_lib::backend::{CommitId, Tree, TreeValue};
use jj_lib::object_id::ObjectId as _;
use jj_lib::repo_path::{RepoPath, RepoPathBuf};
use task_files_version_store::VersionStoreBackend;

use crate::consts::MARKER_FILE;
use crate::error::{Error, Result};

/// A tree entry's content identity, comparable across heads: the
/// content address for a file, the subtree id for a directory. Two
/// heads agreeing on this key agree on the entry.
fn identity(value: &TreeValue) -> Option<(bool, String)> {
    match value {
        TreeValue::File { id, .. } => Some((false, id.hex())),
        TreeValue::Tree(id) => Some((true, id.hex())),
        _ => None,
    }
}

/// The tree at `dir` inside `commit_id`, or `None` when that commit
/// doesn't have the directory (a path that only exists on one side of
/// a divergence, or below a file).
async fn dir_tree(
    backend: &VersionStoreBackend,
    commit_id: &CommitId,
    dir: &RepoPath,
) -> Result<Option<Tree>> {
    let commit = backend.commit(commit_id).await?;
    let tree_id = commit
        .root_tree
        .clone()
        .into_resolved()
        .map_err(|_| Error::Repo("browsing a conflicted tree is unsupported (v1)".into()))?;
    let mut current = backend.tree(&tree_id).await?;
    for component in dir.components() {
        match current.value(component) {
            Some(TreeValue::Tree(id)) => current = backend.tree(id).await?,
            _ => return Ok(None),
        }
    }
    Ok(Some(current))
}

/// One head's listing of `dir`: entry name ⇒ (is_dir, content id).
async fn listing(
    backend: &VersionStoreBackend,
    commit_id: &CommitId,
    dir: &RepoPath,
) -> Result<BTreeMap<String, (bool, String)>> {
    let mut out = BTreeMap::new();
    let Some(tree) = dir_tree(backend, commit_id, dir).await? else {
        return Ok(out);
    };
    for name in tree.names() {
        let Some(value) = tree.value(name) else {
            continue;
        };
        if let Some(id) = identity(value) {
            out.insert(name.as_internal_str().to_owned(), id);
        }
    }
    Ok(out)
}

/// Annotate `entries` (the live-tree listing of `dir` inside a root)
/// with stub + divergence state, appending one entry per tracked-but-
/// not-resident path. `heads` is the root's visible head set; `head`
/// is the checkpoint head the rest of the surface reads.
///
/// Called on the blocking path — the version store is async, so the
/// walk is driven with `pollster::block_on` exactly like
/// `chain`/`checkpoint_now` (see `backend`'s module doc).
pub fn annotate(
    backend: &VersionStoreBackend,
    head: &CommitId,
    heads: &BTreeSet<CommitId>,
    dir: &RepoPath,
    entries: &mut Vec<BrowseEntry>,
) -> Result<()> {
    let head_listing = pollster::block_on(listing(backend, head, dir))?;

    // Divergence: compare every visible head's listing of this
    // directory. A name whose content id differs between two heads (or
    // exists in one and not the other) is divergent.
    let mut divergent: BTreeSet<String> = BTreeSet::new();
    if heads.len() > 1 {
        let mut names: BTreeSet<String> = BTreeSet::new();
        let mut per_head: Vec<BTreeMap<String, (bool, String)>> = Vec::new();
        for head_id in heads {
            let l = pollster::block_on(listing(backend, head_id, dir))?;
            names.extend(l.keys().cloned());
            per_head.push(l);
        }
        for name in names {
            // Distinct states across the heads — `None` (absent on that
            // side) counts as its own state, so an add on one side and
            // not the other is divergence just like differing content.
            let states: BTreeSet<Option<&(bool, String)>> =
                per_head.iter().map(|l| l.get(&name)).collect();
            if states.len() > 1 {
                divergent.insert(name);
            }
        }
    }

    for entry in entries.iter_mut() {
        entry.divergent = divergent.contains(&entry.name);
    }

    // Stubs: tracked at the head, absent from the live tree.
    let resident: BTreeSet<&str> = entries.iter().map(|e| e.name.as_str()).collect();
    let mut stubs: Vec<BrowseEntry> = head_listing
        .iter()
        .filter(|(name, _)| !resident.contains(name.as_str()))
        .map(|(name, (is_dir, _))| BrowseEntry {
            name: name.clone(),
            is_dir: *is_dir,
            size: None,
            stub: true,
            divergent: divergent.contains(name),
        })
        .collect();
    entries.append(&mut stubs);
    entries.sort_by(|a, b| a.name.cmp(&b.name));
    Ok(())
}

/// A browse `subpath` as a jj repo path — the same normalization the
/// scan walker applies (native separators to `/`, no leading or
/// trailing slash). An empty subpath is the root itself.
pub fn repo_dir(subpath: &str) -> Result<RepoPathBuf> {
    let normalized = subpath
        .replace(std::path::MAIN_SEPARATOR, "/")
        .trim_matches('/')
        .to_owned();
    if normalized.is_empty() {
        return Ok(RepoPathBuf::root().to_owned());
    }
    // Belt and braces with jj's own validation: a `.`/`..` component
    // must never reach the tracked-path lookup, because that lookup is
    // the one browse path that doesn't go through a canonicalize +
    // prefix check (see `browse_inner`).
    if normalized.split('/').any(|c| c == "." || c == "..") {
        return Err(Error::BadRequest(format!(
            "subpath escapes the root: {subpath}"
        )));
    }
    RepoPathBuf::from_internal_string(&normalized)
        .map_err(|e| Error::BadRequest(format!("{normalized:?}: {e}")))
}

/// The root's Project Version badge, read from its marker file. A
/// marker without a `project_version` object (every root created by v1)
/// has no badge; a malformed one is ignored rather than failing the
/// browse — the badge is decoration, never the reason a root can't be
/// listed.
#[must_use]
pub fn project_version(root_path: &Path) -> Option<ProjectVersionBadge> {
    let bytes = std::fs::read(root_path.join(MARKER_FILE)).ok()?;
    let marker: serde_json::Value = serde_json::from_slice(&bytes).ok()?;
    let pv = marker.get("project_version")?;
    let number = u32::try_from(pv.get("number")?.as_u64()?).ok()?;
    let label = pv
        .get("label")
        .and_then(|l| l.as_str())
        .map(str::to_owned)
        .filter(|l| !l.is_empty());
    Some(ProjectVersionBadge { number, label })
}
