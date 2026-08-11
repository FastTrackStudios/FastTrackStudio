//! `Backend::gc` — mark-and-sweep over the structural objects this crate
//! owns (trees, commits, copy-history records).
//!
//! Scope note: per ADR 0001, "manifests are the roots" for the chunk
//! store's own liveness (`task_files_chunk_store::ChunkStore`'s GC pinning
//! model doc), and that crate does not yet expose any enumeration or
//! deletion primitive over its `blobs/`/`manifests/` directories (that's
//! deliberately out of #256's scope). So this `gc` reclaims only the
//! version-store's own tree/commit/copy-history objects; chunk-level
//! reclamation is future work layered on top once the chunk store grows a
//! GC primitive to drive from this backend's manifest-reachability set.
//! Nothing here contradicts the "index-reachable ∪ Vault-referenced"
//! protect-set doctrine — Vault-referenced protection is a layer above the
//! backend (the Vault holds the truth on immortality), and this join is
//! deliberately unimplemented until that layer exists to answer it.

use std::collections::BTreeSet;
use std::time::SystemTime;

use jj_lib::backend::{CommitId, CopyId, TreeId, TreeValue};
use jj_lib::index::Index;
use jj_lib::object_id::ObjectId as _;

use crate::backend::VersionStoreBackend;
use crate::error::Result;

async fn mark_tree(
    backend: &VersionStoreBackend,
    id: &TreeId,
    live_trees: &mut BTreeSet<TreeId>,
    live_copies: &mut BTreeSet<CopyId>,
) -> Result<()> {
    if !live_trees.insert(id.clone()) {
        return Ok(()); // already visited
    }
    let tree = backend.tree(id).await?;
    for entry in tree.entries() {
        match entry.value() {
            TreeValue::Tree(sub) => {
                Box::pin(mark_tree(backend, sub, live_trees, live_copies)).await?
            }
            TreeValue::File { copy_id, .. } => {
                mark_copy_ancestry(backend, copy_id, live_copies).await?;
            }
            TreeValue::Symlink(_) | TreeValue::GitSubmodule(_) => {}
        }
    }
    Ok(())
}

async fn mark_copy_ancestry(
    backend: &VersionStoreBackend,
    id: &CopyId,
    live_copies: &mut BTreeSet<CopyId>,
) -> Result<()> {
    if !live_copies.insert(id.clone()) {
        return Ok(());
    }
    let history = backend.copy_history(id).await?;
    for parent in &history.parents {
        Box::pin(mark_copy_ancestry(backend, parent, live_copies)).await?;
    }
    Ok(())
}

/// Mark every tree/commit/copy-history object reachable from `index`'s
/// GC heads (or newer than `keep_newer`, protecting concurrent writers per
/// the `Backend::gc` contract), then sweep everything else.
pub async fn sweep(
    backend: &VersionStoreBackend,
    index: &dyn Index,
    keep_newer: SystemTime,
) -> Result<()> {
    let heads: Vec<CommitId> = index
        .all_heads_for_gc()
        .map_err(|e| crate::error::Error::Repo(e.to_string()))?
        .collect();

    let mut live_commits = BTreeSet::new();
    let mut live_trees = BTreeSet::new();
    let mut live_copies = BTreeSet::new();
    // Always pin the empty tree: it's the root commit's tree, it's handed
    // out as `empty_tree_id()` to any caller building a fresh tree, and
    // it's cheap to keep unconditionally rather than relying on the root
    // commit (a synthesized, never-written object — see `backend.commit`'s
    // special case for `root_commit_id`) to flow through the walk below to
    // mark it.
    mark_tree(
        backend,
        backend.empty_tree_id_for_gc(),
        &mut live_trees,
        &mut live_copies,
    )
    .await?;

    let mut frontier = heads;
    while let Some(id) = frontier.pop() {
        if !live_commits.insert(id.clone()) {
            continue;
        }
        let commit = backend.commit(&id).await?;
        if let Ok(tree_id) = commit.root_tree.clone().into_resolved() {
            mark_tree(backend, &tree_id, &mut live_trees, &mut live_copies).await?;
        } else {
            for tree_id in commit.root_tree.iter() {
                mark_tree(backend, tree_id, &mut live_trees, &mut live_copies).await?;
            }
        }
        frontier.extend(commit.parents);
    }

    let objects = backend.objects();
    for (hash, mtime) in objects.list_with_mtime().await? {
        if mtime >= keep_newer {
            continue;
        }
        let is_live = live_commits
            .iter()
            .any(|id| id.as_bytes() == hash.as_bytes())
            || live_trees.iter().any(|id| id.as_bytes() == hash.as_bytes())
            || live_copies
                .iter()
                .any(|id| id.as_bytes() == hash.as_bytes());
        if !is_live {
            objects.remove(&hash).await?;
        }
    }

    // Note: swept copy-history objects can leave dangling entries behind in
    // `copy-children` index files (a hint, not an authority — see
    // `ObjectStore::append_index_line`'s doc). Rather than pruning those
    // here — which would need to enumerate every index file, not just the
    // ones reachable from what we happened to mark — `VersionStoreBackend::
    // copy_children` tolerates a missing child object directly: it's
    // unreachable by definition once its own object is gone.

    Ok(())
}
