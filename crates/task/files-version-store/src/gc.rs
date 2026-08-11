//! `Backend::gc` — mark-and-sweep over the structural objects this crate
//! owns (trees, commits, copy-history records) plus, since issue #258, the
//! chunk store's own manifests/chunks underneath it.
//!
//! Two protect inputs feed the mark phase, matching ADR 0001's
//! "index-reachable ∪ Vault-referenced" doctrine:
//! - `index.all_heads_for_gc()` — jj-lib's own reachability, always honored
//!   (this is what `Backend::gc`'s fixed trait signature can pass through).
//! - `protected_commits` — externally-referenced versions (Named Versions,
//!   Project Version starts, share-link targets, review pins) the Vault
//!   resolves to commit ids and supplies directly. `Backend::gc`'s trait
//!   signature has no room for this (jj-lib calls it with exactly
//!   `(index, keep_newer)`), so it's threaded through [`sweep`] — the
//!   `Backend::gc` impl in `backend.rs` calls `sweep` with an empty slice;
//!   the Vault-facing layer (future RPC work) calls `sweep` directly with
//!   its own resolved set.
//!
//! Every `TreeValue::File` encountered while marking trees also records its
//! chunk-store `FileId` into `live_files`, so the chunk store's own
//! manifests/chunks can be swept from the same reachability computation
//! (`ChunkStore::gc`, #258) rather than needing a second index walk.

use std::collections::BTreeSet;
use std::time::SystemTime;

use jj_lib::backend::{CommitId, CopyId, TreeId, TreeValue};
use jj_lib::index::Index;
use jj_lib::object_id::ObjectId as _;

use crate::backend::VersionStoreBackend;
use crate::error::Result;

/// The outcome of one [`sweep`] pass.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct GcStats {
    /// Tree/commit/copy-history objects removed this pass.
    pub objects_swept: usize,
    /// The underlying chunk store's own mark-phase result (see
    /// `task_files_chunk_store::ChunkStore::gc`): manifests removed now,
    /// chunks marked for iroh-blobs' own background sweep to reclaim.
    pub chunks: task_files_chunk_store::GcStats,
}

fn chunk_file_id(id: &jj_lib::backend::FileId) -> Option<task_files_chunk_store::FileId> {
    task_files_chunk_store::FileId::from_hex(&id.hex()).ok()
}

async fn mark_tree(
    backend: &VersionStoreBackend,
    id: &TreeId,
    live_trees: &mut BTreeSet<TreeId>,
    live_copies: &mut BTreeSet<CopyId>,
    live_files: &mut BTreeSet<task_files_chunk_store::FileId>,
) -> Result<()> {
    if !live_trees.insert(id.clone()) {
        return Ok(()); // already visited
    }
    let tree = backend.tree(id).await?;
    for entry in tree.entries() {
        match entry.value() {
            TreeValue::Tree(sub) => {
                Box::pin(mark_tree(backend, sub, live_trees, live_copies, live_files)).await?
            }
            TreeValue::File { id, copy_id, .. } => {
                mark_copy_ancestry(backend, copy_id, live_copies).await?;
                if let Some(file_id) = chunk_file_id(id) {
                    live_files.insert(file_id);
                }
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

/// Mark every tree/commit/copy-history object reachable from `index`'s GC
/// heads or `protected_commits` (or newer than `keep_newer`, protecting
/// concurrent writers per the `Backend::gc` contract), then sweep
/// everything else — including, since #258, the chunk store underneath
/// this backend (see the module doc).
pub async fn sweep(
    backend: &VersionStoreBackend,
    index: &dyn Index,
    keep_newer: SystemTime,
    protected_commits: &[CommitId],
) -> Result<GcStats> {
    let heads: Vec<CommitId> = index
        .all_heads_for_gc()
        .map_err(|e| crate::error::Error::Repo(e.to_string()))?
        .collect();

    let mut live_commits = BTreeSet::new();
    let mut live_trees = BTreeSet::new();
    let mut live_copies = BTreeSet::new();
    let mut live_files: BTreeSet<task_files_chunk_store::FileId> = BTreeSet::new();
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
        &mut live_files,
    )
    .await?;

    let mut frontier = heads;
    frontier.extend(protected_commits.iter().cloned());
    while let Some(id) = frontier.pop() {
        if !live_commits.insert(id.clone()) {
            continue;
        }
        let commit = backend.commit(&id).await?;
        if let Ok(tree_id) = commit.root_tree.clone().into_resolved() {
            mark_tree(
                backend,
                &tree_id,
                &mut live_trees,
                &mut live_copies,
                &mut live_files,
            )
            .await?;
        } else {
            for tree_id in commit.root_tree.iter() {
                mark_tree(
                    backend,
                    tree_id,
                    &mut live_trees,
                    &mut live_copies,
                    &mut live_files,
                )
                .await?;
            }
        }
        frontier.extend(commit.parents);
    }

    let objects = backend.objects();
    let mut objects_swept = 0usize;
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
            objects_swept += 1;
        }
    }

    // Note: swept copy-history objects can leave dangling entries behind in
    // `copy-children` index files (a hint, not an authority — see
    // `ObjectStore::append_index_line`'s doc). Rather than pruning those
    // here — which would need to enumerate every index file, not just the
    // ones reachable from what we happened to mark — `VersionStoreBackend::
    // copy_children` tolerates a missing child object directly: it's
    // unreachable by definition once its own object is gone.

    let chunks = backend.chunks().gc(&live_files, keep_newer).await?;

    Ok(GcStats {
        objects_swept,
        chunks,
    })
}
