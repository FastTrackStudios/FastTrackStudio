//! Streaming Session checkpoint commit — replaces
//! `task_files_version_store::checkpoint::checkpoint`'s in-memory
//! `Change::Write { content: Vec<u8> }` shape for `checkpoint_now`'s
//! full-scan write (PR #280 review finding: reading every file into a
//! `Vec<u8>` before handing it to `checkpoint()` peaks RSS at the
//! whole root's size — a 40 GB media root OOMs the shared
//! `task-server`). Each disk file streams straight into the backend via
//! `Backend::write_file` (bounded-memory on both flavors: the CAS
//! backend chunks the reader as it goes, git's streams into a loose
//! object); a file whose content is unchanged from the checkpoint head is
//! skipped entirely — both to avoid the wasted read/hash/CAS-write and
//! so `CheckpointInfo::changed_paths` reflects only what actually
//! changed, honoring its own doc contract.
//!
//! This bypasses `checkpoint::checkpoint` (the version-store crate's
//! own convenience helper) and builds the commit directly via jj-lib's
//! `TreeBuilder` + `Transaction`, mirroring that helper's own
//! internals — the ticket's "consume the version-store API as-is"
//! boundary means duplicating this small amount of logic rather than
//! widening that crate's visibility.
//!
//! **Flavor-agnostic** (issue #273): everything here is written against
//! jj-lib's `Backend` trait, so a media root's CAS backend and a software
//! root's stock `GitBackend` produce checkpoints through the same code —
//! which is what makes the chain/history RPC behave identically on both.
//! The one place they differ is copy tracking: git reports
//! `BackendError::Unsupported` from `write_copy`, so software roots use
//! `CopyId::placeholder()` (git's own convention) and their chains fall
//! back to path-following, exactly as `jj` does on a git repo.

use std::collections::BTreeSet;
use std::sync::Arc;

use jj_lib::backend::{
    Backend, BackendError, CommitId, CopyHistory, CopyId, FileId, Tree, TreeId, TreeValue,
};
use jj_lib::merged_tree::MergedTree;
use jj_lib::object_id::ObjectId as _;
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::repo_path::{RepoPath, RepoPathBuf};
use jj_lib::tree_builder::TreeBuilder;
use tokio_util::compat::TokioAsyncReadCompatExt as _;

use crate::error::{Error, Result};
use crate::scan::{LiveFile, lookup};

pub struct CheckpointResult {
    pub repo: Arc<ReadonlyRepo>,
    pub commit_id: CommitId,
    /// Root-relative paths actually written or removed, sorted. Empty
    /// when the live tree was already identical to the previous
    /// checkpoint (a file whose streamed content hashes to the same
    /// `FileId` as the head's is never written).
    pub changed_paths: Vec<String>,
}

/// Streams every file in `disk_files` into the backend, skipping any whose
/// content is unchanged from `head_tree`, removes any `head_paths`
/// entry absent from `disk_files`, and commits the result on top of
/// `parent_id`. v1 detects no renames: a moved file surfaces as a
/// remove+write pair rather than a recorded `CopyHistory` link (ADR
/// 0001: "detection may start simple").
///
/// Ignored files ([`LiveFile::ignored`]) are skipped — unless they are
/// already tracked in `head_paths`, in which case they keep being
/// versioned. An Ignore set decides what *starts* being versioned; it
/// never retroactively deletes history (see [`crate::ignore`]).
#[allow(clippy::too_many_arguments)]
pub fn write_checkpoint(
    repo: &Arc<ReadonlyRepo>,
    backend: &dyn Backend,
    parent_id: CommitId,
    base_tree_id: TreeId,
    head_tree: &Tree,
    disk_files: &[LiveFile],
    head_paths: &BTreeSet<RepoPathBuf>,
    description: String,
) -> Result<CheckpointResult> {
    pollster::block_on(write_checkpoint_async(
        repo,
        backend,
        parent_id,
        base_tree_id,
        head_tree,
        disk_files,
        head_paths,
        description,
    ))
}

/// A fresh copy-history record for a file with no recorded ancestry, or
/// git's placeholder on a backend that doesn't track copies at all.
async fn origin_copy_id(backend: &dyn Backend, path: &RepoPath, salt: Vec<u8>) -> Result<CopyId> {
    let history = CopyHistory {
        current_path: path.to_owned(),
        parents: vec![],
        salt,
    };
    match backend.write_copy(&history).await {
        Ok(id) => Ok(id),
        Err(BackendError::Unsupported(_)) => Ok(CopyId::placeholder()),
        Err(err) => Err(err.into()),
    }
}

#[allow(clippy::too_many_arguments)]
async fn write_checkpoint_async(
    repo: &Arc<ReadonlyRepo>,
    backend: &dyn Backend,
    parent_id: CommitId,
    base_tree_id: TreeId,
    head_tree: &Tree,
    disk_files: &[LiveFile],
    head_paths: &BTreeSet<RepoPathBuf>,
    description: String,
) -> Result<CheckpointResult> {
    let store = repo.store().clone();
    let mut builder = TreeBuilder::new(store.clone(), base_tree_id);
    let mut changed_paths = Vec::new();
    let mut present: BTreeSet<RepoPathBuf> = BTreeSet::new();

    for file in disk_files {
        let repo_path = &file.repo_path;
        if file.ignored && !head_paths.contains(repo_path) {
            // Ignored and untracked: never enters the store. (Ignored but
            // already tracked falls through and keeps being versioned —
            // see this module's doc.)
            continue;
        }
        present.insert(repo_path.clone());
        let existing = lookup(backend, head_tree, repo_path).await?;

        // Bounded-memory streaming write straight from disk — never a
        // `std::fs::read` into a `Vec<u8>` (see module doc).
        // `Backend::write_file` reads through futures-io; tokio's file
        // handle wears the `compat()` adapter (same seam the version-store
        // backend uses internally).
        let mut disk = tokio::fs::File::open(&file.disk_path).await?.compat();
        let new_id: FileId = backend.write_file(repo_path, &mut disk).await?;

        let copy_id = match &existing {
            Some(TreeValue::File {
                id: old_id,
                copy_id,
                ..
            }) => {
                if *old_id == new_id {
                    // Unchanged: no builder mutation, no changed_paths
                    // entry — this is what keeps a no-op checkpoint a
                    // true no-op in both the commit tree and the
                    // reported diff.
                    continue;
                }
                copy_id.clone()
            }
            _ => origin_copy_id(backend, repo_path, new_id.as_bytes().to_vec()).await?,
        };
        let value = TreeValue::File {
            id: new_id,
            executable: false,
            copy_id,
        };
        builder.set(repo_path.clone(), value);
        changed_paths.push(repo_path.as_internal_file_string().to_string());
    }

    for path in head_paths {
        if !present.contains(path) {
            builder.remove(path.clone());
            changed_paths.push(path.as_internal_file_string().to_string());
        }
    }
    changed_paths.sort();

    let new_tree_id = builder
        .write_tree()
        .await
        .map_err(|e| Error::Repo(format!("write_tree: {e}")))?;
    let merged_tree = MergedTree::resolved(store, new_tree_id);

    let mut tx = repo.start_transaction();
    tx.repo_mut()
        .new_commit(vec![parent_id], merged_tree)
        .set_description(description)
        .write()
        .await
        .map_err(|e| Error::Repo(format!("write commit: {e}")))?;
    let new_repo = tx
        .commit("checkpoint")
        .await
        .map_err(|e| Error::Repo(e.to_string()))?;
    let commit_id = new_repo
        .view()
        .heads()
        .iter()
        .next()
        .cloned()
        .ok_or_else(|| Error::Repo("checkpoint produced no head".into()))?;

    Ok(CheckpointResult {
        repo: new_repo,
        commit_id,
        changed_paths,
    })
}
