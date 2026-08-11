//! Streaming capture commit — the one write path behind both kinds of
//! capture the cadence engine drives (an ephemeral auto-snapshot and a
//! certified Session checkpoint) and behind the explicit
//! `checkpoint_now` RPC. It replaces
//! `task_files_version_store::checkpoint::checkpoint`'s in-memory
//! `Change::Write { content: Vec<u8> }` shape (PR #280 review finding:
//! reading every file into a `Vec<u8>` before handing it to
//! `checkpoint()` peaks RSS at the whole root's size — a 40 GB media
//! root OOMs the shared `task-server`). Each disk file streams straight
//! into the CAS chunk store through [`crate::certify`] (bounded memory,
//! and stat-sandwiched so a file being written right now is requeued
//! rather than committed torn); a file whose content is unchanged from
//! the base tree is skipped entirely — both to avoid the wasted
//! read/hash/CAS-write and so `changed_paths` reflects only what
//! actually changed.
//!
//! This bypasses `checkpoint::checkpoint` (the version-store crate's
//! own convenience helper) and builds the commit directly via jj-lib's
//! `TreeBuilder` + `Transaction`, mirroring that helper's own
//! internals — issue #259's "consume the version-store API as-is"
//! boundary means duplicating this small amount of logic rather than
//! widening that crate's visibility.

use std::collections::BTreeSet;
use std::path::PathBuf;
use std::sync::Arc;

use jj_lib::backend::{CommitId, FileId, Tree, TreeId, TreeValue};
use jj_lib::merged_tree::MergedTree;
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::repo_path::RepoPathBuf;
use jj_lib::tree_builder::TreeBuilder;
use task_files_version_store::VersionStoreBackend;

use crate::certify::{MidHashHook, stream_certified};
use crate::error::{Error, Result};
use crate::scan::lookup;

/// Everything one capture commit needs: what it is parented on, what
/// the live tree currently holds, and how hard to try certifying a file
/// that is moving under the scan.
pub struct Capture<'a> {
    pub repo: &'a Arc<ReadonlyRepo>,
    pub backend: &'a VersionStoreBackend,
    /// Commit this capture is parented on: the checkpoint head for a
    /// checkpoint, the snapshot branch's tip for a snapshot.
    pub parent_id: CommitId,
    pub base_tree_id: TreeId,
    pub base_tree: &'a Tree,
    /// (root-relative path, absolute disk path) for every live-tree
    /// file the Ignore set lets through.
    pub disk_files: &'a [(RepoPathBuf, PathBuf)],
    /// Every path tracked in the base tree — the set a capture removes
    /// from when a file has disappeared from the live tree.
    pub base_paths: &'a BTreeSet<RepoPathBuf>,
    pub description: String,
    /// How many times to re-read a file that changed while being
    /// hashed before requeueing it.
    pub attempts: u32,
    /// Test seam only — see [`crate::certify::MidHashHook`].
    pub hook: Option<MidHashHook>,
}

pub struct CaptureResult {
    pub repo: Arc<ReadonlyRepo>,
    pub commit_id: CommitId,
    /// Root-relative paths actually written or removed, sorted. Empty
    /// when the live tree was already identical to the base (a file
    /// whose streamed content hashes to the same `FileId` as the base's
    /// is never written).
    pub changed_paths: Vec<String>,
    /// Paths still being written after `attempts` certification tries.
    /// They keep their base state in this capture and ride into the
    /// next one, sorted.
    pub requeued_paths: Vec<String>,
}

/// Write one capture commit. Blocking wrapper — see `backend.rs`'s
/// module doc for why nothing in this crate `.await`s jj-lib from
/// inside an `async fn`.
pub fn write_capture(capture: Capture<'_>) -> Result<CaptureResult> {
    pollster::block_on(write_capture_async(capture))
}

async fn write_capture_async(capture: Capture<'_>) -> Result<CaptureResult> {
    let Capture {
        repo,
        backend,
        parent_id,
        base_tree_id,
        base_tree,
        disk_files,
        base_paths,
        description,
        attempts,
        hook,
    } = capture;

    let store = repo.store().clone();
    let mut builder = TreeBuilder::new(store.clone(), base_tree_id);
    let mut changed_paths = Vec::new();
    let mut requeued_paths = Vec::new();
    let mut present: BTreeSet<RepoPathBuf> = BTreeSet::new();

    for (repo_path, disk_path) in disk_files {
        // Recorded as present *before* the read: a requeued file must
        // keep its existing versioned state, not be removed as if it
        // had vanished from the live tree.
        present.insert(repo_path.clone());
        let existing = lookup(backend, base_tree, repo_path).await?;

        // Bounded-memory streaming write straight from disk, certified
        // stable by a stat sandwich (see `certify`) — never a
        // `std::fs::read` into a `Vec<u8>`.
        let Some(file_id) =
            stream_certified(backend.chunks(), disk_path, attempts, hook.as_ref()).await?
        else {
            requeued_paths.push(repo_path.as_internal_file_string().to_string());
            continue;
        };
        let jj_file_id = FileId::from_bytes(file_id.as_bytes());

        let copy_id = match &existing {
            Some(TreeValue::File {
                id: old_id,
                copy_id,
                ..
            }) => {
                if *old_id == jj_file_id {
                    // Unchanged: no builder mutation, no changed_paths
                    // entry — this is what keeps a no-op capture a true
                    // no-op in both the commit tree and the reported
                    // diff.
                    continue;
                }
                copy_id.clone()
            }
            _ => {
                backend
                    .write_origin_copy(repo_path, file_id.as_bytes().to_vec())
                    .await?
            }
        };
        let value = TreeValue::File {
            id: jj_file_id,
            executable: false,
            copy_id,
        };
        builder.set(repo_path.clone(), value);
        changed_paths.push(repo_path.as_internal_file_string().to_string());
    }

    for path in base_paths {
        if !present.contains(path) {
            builder.remove(path.clone());
            changed_paths.push(path.as_internal_file_string().to_string());
        }
    }
    changed_paths.sort();
    requeued_paths.sort();

    let new_tree_id = builder
        .write_tree()
        .await
        .map_err(|e| Error::Repo(format!("write_tree: {e}")))?;
    let merged_tree = MergedTree::resolved(store, new_tree_id);

    let mut tx = repo.start_transaction();
    let commit = tx
        .repo_mut()
        .new_commit(vec![parent_id], merged_tree)
        .set_description(description)
        .write()
        .await
        .map_err(|e| Error::Repo(format!("write commit: {e}")))?;
    // The commit's own id, never `view().heads()` — a root that has
    // taken auto-snapshots legitimately has more than one head (the
    // snapshot branch alongside the checkpoint line), so "the first
    // head" names the wrong commit exactly when it matters.
    let commit_id = commit.id().clone();
    let new_repo = tx
        .commit("checkpoint")
        .await
        .map_err(|e| Error::Repo(e.to_string()))?;

    Ok(CaptureResult {
        repo: new_repo,
        commit_id,
        changed_paths,
        requeued_paths,
    })
}
