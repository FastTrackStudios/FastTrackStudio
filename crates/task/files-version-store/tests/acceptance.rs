//! Issue #257 acceptance criteria, exercised directly against the backend
//! (the spec's Testing Decisions "secondary harness", since the RPC layer
//! consuming this crate is future work — the same posture #256's own tests
//! took for the chunk store underneath it).

use std::io::Cursor;

use jj_lib::backend::{Backend as _, FileId as JjFileId};
use jj_lib::repo::Repo as _;
use jj_lib::repo_path::RepoPathBuf;
use tokio_util::compat::TokioAsyncReadCompatExt as _;

use task_files_version_store::VersionStoreBackend;
use task_files_version_store::chain::version_chain;
use task_files_version_store::checkpoint::{Change, checkpoint};
use task_files_version_store::repo::init_repo;

fn path(s: &str) -> RepoPathBuf {
    RepoPathBuf::from_internal_string(s).unwrap()
}

fn backend_of(repo: &std::sync::Arc<jj_lib::repo::ReadonlyRepo>) -> &VersionStoreBackend {
    repo.store()
        .backend_impl::<VersionStoreBackend>()
        .expect("repo's store is a VersionStoreBackend")
}

async fn read_file_content(backend: &VersionStoreBackend, id: &JjFileId) -> Vec<u8> {
    let path = path("irrelevant-for-reads");
    let mut reader = backend.read_file(&path, id).await.unwrap();
    let mut buf = Vec::new();
    futures::AsyncReadExt::read_to_end(&mut reader, &mut buf)
        .await
        .unwrap();
    buf
}

/// "A temp tree checkpoints; a second checkpoint after edits yields a
/// derivable per-file version chain."
#[tokio::test]
async fn second_checkpoint_yields_a_derivable_version_chain() {
    let dir = tempfile::tempdir().unwrap();
    let repo = init_repo(dir.path().join("repo").as_path()).await.unwrap();
    let root_id = repo.store().root_commit_id().clone();
    let mix = path("session/mix.wav");

    let repo = checkpoint(
        &repo,
        root_id,
        vec![Change::Write {
            path: mix.clone(),
            content: b"take one".to_vec(),
        }],
        "first checkpoint",
    )
    .await
    .unwrap();
    let first_head = repo.view().heads().iter().next().unwrap().clone();

    let repo = checkpoint(
        &repo,
        first_head.clone(),
        vec![Change::Write {
            path: mix.clone(),
            content: b"take two, much longer this time".to_vec(),
        }],
        "second checkpoint",
    )
    .await
    .unwrap();
    let second_head = repo.view().heads().iter().next().unwrap().clone();
    assert_ne!(first_head, second_head);

    let backend = backend_of(&repo);
    let chain = version_chain(backend, &second_head, &mix).await.unwrap();

    // Newest first: the second checkpoint's save, then the first's.
    assert_eq!(chain.len(), 2, "expected two saved states, got {chain:?}");
    assert_eq!(chain[0].commit_id, second_head);
    assert_eq!(chain[1].commit_id, first_head);
    assert_eq!(
        read_file_content(backend, &chain[0].file_id).await,
        b"take two, much longer this time"
    );
    assert_eq!(
        read_file_content(backend, &chain[1].file_id).await,
        b"take one"
    );
}

/// "Renames are recorded (CopyHistory) and chains follow them."
#[tokio::test]
async fn rename_is_recorded_and_the_chain_follows_it() {
    let dir = tempfile::tempdir().unwrap();
    let repo = init_repo(dir.path().join("repo").as_path()).await.unwrap();
    let root_id = repo.store().root_commit_id().clone();
    let old_path = path("stems/guitar.wav");
    let new_path = path("stems/guitar_final.wav");

    let repo = checkpoint(
        &repo,
        root_id,
        vec![Change::Write {
            path: old_path.clone(),
            content: b"guitar take".to_vec(),
        }],
        "record guitar",
    )
    .await
    .unwrap();
    let created_at = repo.view().heads().iter().next().unwrap().clone();

    let repo = checkpoint(
        &repo,
        created_at.clone(),
        vec![Change::Rename {
            from: old_path.clone(),
            to: new_path.clone(),
            new_content: None,
        }],
        "rename to final",
    )
    .await
    .unwrap();
    let renamed_at = repo.view().heads().iter().next().unwrap().clone();

    let backend = backend_of(&repo);

    // The chain, followed from the file's *current* (renamed) path, walks
    // back across the rename to the commit where it was created.
    let chain = version_chain(backend, &renamed_at, &new_path)
        .await
        .unwrap();
    assert_eq!(chain.len(), 2, "expected rename + origin, got {chain:?}");
    assert_eq!(chain[0].commit_id, renamed_at);
    assert_eq!(chain[0].path, new_path);
    assert_eq!(chain[0].renamed_from.as_ref(), Some(&old_path));
    assert_eq!(chain[1].commit_id, created_at);
    assert_eq!(chain[1].path, old_path);
    assert_eq!(
        read_file_content(backend, &chain[1].file_id).await,
        b"guitar take"
    );

    // `Backend::get_copy_records` sees the same rename as recorded fact
    // (not a heuristic) over the dag range root..renamed_at.
    let mut records_stream = backend
        .get_copy_records(
            Some(std::slice::from_ref(&new_path)),
            &root_commit(backend),
            &renamed_at,
        )
        .unwrap();
    let mut records = Vec::new();
    while let Some(record) = futures::StreamExt::next(&mut records_stream).await {
        records.push(record.unwrap());
    }
    assert_eq!(
        records.len(),
        1,
        "expected exactly one copy record, got {records:?}"
    );
    assert_eq!(records[0].target, new_path);
    assert_eq!(records[0].source, old_path);
    assert_eq!(records[0].target_commit, renamed_at);
    assert_eq!(records[0].source_commit, created_at);
}

fn root_commit(backend: &VersionStoreBackend) -> jj_lib::backend::CommitId {
    backend.root_commit_id().clone()
}

/// "Divergent concurrent writes both survive under one change id (jj
/// op-log semantics intact through our backend)." Two independent
/// transactions each rewrite the same base commit differently, both
/// commit, and reloading the repo must merge the op-log heads the way jj
/// always does: both rewrites survive as siblings under one `ChangeId`
/// rather than one silently winning.
#[tokio::test]
async fn divergent_concurrent_rewrites_both_survive_under_one_change_id() {
    let dir = tempfile::tempdir().unwrap();
    let repo = init_repo(dir.path().join("repo").as_path()).await.unwrap();
    let root_id = repo.store().root_commit_id().clone();
    let mix = path("mix.wav");

    let repo = checkpoint(
        &repo,
        root_id,
        vec![Change::Write {
            path: mix.clone(),
            content: b"base".to_vec(),
        }],
        "base checkpoint",
    )
    .await
    .unwrap();
    let base_id = repo.view().heads().iter().next().unwrap().clone();
    let base_change_id = repo
        .store()
        .get_commit_async(&base_id)
        .await
        .unwrap()
        .change_id()
        .clone();

    // Two independent rewrites of the *same* base commit — as two
    // machines editing the same checkpoint offline would produce — each
    // committed from the same starting `repo` view (neither has seen the
    // other's write yet).
    let mut tx_a = repo.start_transaction();
    let commit_a = tx_a
        .repo_mut()
        .rewrite_commit(&repo.store().get_commit_async(&base_id).await.unwrap())
        .set_description("side A")
        .write()
        .await
        .unwrap();
    tx_a.repo_mut().rebase_descendants().await.unwrap();
    let repo_a = tx_a.commit("side A").await.unwrap();

    let mut tx_b = repo.start_transaction();
    let commit_b = tx_b
        .repo_mut()
        .rewrite_commit(&repo.store().get_commit_async(&base_id).await.unwrap())
        .set_description("side B")
        .write()
        .await
        .unwrap();
    tx_b.repo_mut().rebase_descendants().await.unwrap();
    let _repo_b = tx_b.commit("side B").await.unwrap();

    assert_ne!(commit_a.id(), commit_b.id());
    assert_eq!(commit_a.change_id(), &base_change_id);
    assert_eq!(commit_b.change_id(), &base_change_id);

    // Reloading merges the op-log heads left behind by `repo_a`/`repo_b`.
    let merged = repo_a.reload_at_head().await.unwrap();

    // Both rewrites of the same base commit — same `ChangeId`, two distinct
    // `CommitId`s — survive as heads after the reload merges the op-log:
    // this *is* jj's divergent-change mechanism (a change with more than
    // one visible commit), riding entirely on our backend.
    let heads: Vec<_> = merged.view().heads().iter().cloned().collect();
    assert!(
        heads.contains(commit_a.id()) && heads.contains(commit_b.id()),
        "both divergent rewrites must survive as heads, got {heads:?}"
    );
}

/// "File content streams through the backend in both directions."
#[tokio::test]
async fn file_content_streams_through_the_backend_both_directions() {
    let dir = tempfile::tempdir().unwrap();
    let repo = init_repo(dir.path().join("repo").as_path()).await.unwrap();
    let backend = backend_of(&repo);
    let path = path("clip.wav");

    // A few megabytes — enough to guarantee multiple FastCDC chunks without
    // paying multi-GB stress-test cost on every run.
    let content: Vec<u8> = (0..8 * 1024 * 1024).map(|i| (i % 251) as u8).collect();

    let file_id = backend
        .write_file(&path, &mut Cursor::new(content.clone()).compat())
        .await
        .unwrap();
    let round_tripped = read_file_content(backend, &file_id).await;
    assert_eq!(round_tripped, content);
}
