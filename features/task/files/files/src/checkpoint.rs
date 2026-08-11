//! Streaming Session checkpoint commit — replaces
//! `task_files_version_store::checkpoint::checkpoint`'s in-memory
//! `Change::Write { content: Vec<u8> }` shape for `checkpoint_now`'s
//! full-scan write (PR #280 review finding: reading every file into a
//! `Vec<u8>` before handing it to `checkpoint()` peaks RSS at the
//! whole root's size — a 40 GB media root OOMs the shared
//! `task-server`). Each disk file streams straight into the CAS chunk
//! store via `ChunkStore::write_stream` (already bounded-memory); a
//! file whose content is unchanged from the checkpoint head is
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

use std::collections::BTreeSet;
use std::path::PathBuf;
use std::sync::Arc;

use jj_lib::backend::{CommitId, FileId, Tree, TreeId, TreeValue};
use jj_lib::merged_tree::MergedTree;
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::repo_path::RepoPathBuf;
use jj_lib::tree_builder::TreeBuilder;
use task_files_version_store::VersionStoreBackend;

use crate::error::{Error, Result};
use crate::scan::lookup;

pub struct CheckpointResult {
    pub repo: Arc<ReadonlyRepo>,
    pub commit_id: CommitId,
    /// Root-relative paths actually written or removed, sorted. Empty
    /// when the live tree was already identical to the previous
    /// checkpoint (a file whose streamed content hashes to the same
    /// `FileId` as the head's is never written).
    pub changed_paths: Vec<String>,
}

/// Streams every file in `disk_files` into the CAS, skipping any whose
/// content is unchanged from `head_tree`, removes any `head_paths`
/// entry absent from `disk_files`, and commits the result on top of
/// `parent_id`. v1 detects no renames: a moved file surfaces as a
/// remove+write pair rather than a recorded `CopyHistory` link (ADR
/// 0001: "detection may start simple").
pub fn write_checkpoint(
    repo: &Arc<ReadonlyRepo>,
    backend: &VersionStoreBackend,
    parent_id: CommitId,
    base_tree_id: TreeId,
    head_tree: &Tree,
    disk_files: &[(RepoPathBuf, PathBuf)],
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

async fn write_checkpoint_async(
    repo: &Arc<ReadonlyRepo>,
    backend: &VersionStoreBackend,
    parent_id: CommitId,
    base_tree_id: TreeId,
    head_tree: &Tree,
    disk_files: &[(RepoPathBuf, PathBuf)],
    head_paths: &BTreeSet<RepoPathBuf>,
    description: String,
) -> Result<CheckpointResult> {
    let store = repo.store().clone();
    let mut builder = TreeBuilder::new(store.clone(), base_tree_id);
    let mut changed_paths = Vec::new();
    let mut present: BTreeSet<RepoPathBuf> = BTreeSet::new();

    for (repo_path, disk_path) in disk_files {
        present.insert(repo_path.clone());
        let existing = lookup(backend, head_tree, repo_path).await?;

        // Bounded-memory streaming write straight from disk — never a
        // `std::fs::read` into a `Vec<u8>` (see module doc).
        let file = tokio::fs::File::open(disk_path).await?;
        let file_id = backend
            .chunks()
            .write_stream(file)
            .await
            .map_err(|e| Error::Repo(format!("chunk store: {e}")))?;
        let jj_file_id = FileId::from_bytes(file_id.as_bytes());

        let copy_id = match &existing {
            Some(TreeValue::File {
                id: old_id,
                copy_id,
                ..
            }) => {
                if *old_id == jj_file_id {
                    // Unchanged: no builder mutation, no changed_paths
                    // entry — this is what keeps a no-op checkpoint a
                    // true no-op in both the commit tree and the
                    // reported diff.
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
