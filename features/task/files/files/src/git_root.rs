//! The colocated-git half of a **software** File Root (issue #273, ADR
//! 0001: "software roots use stock colocated git — a perfectly normal
//! `.git` for GitHub, CI, IDEs").
//!
//! Colocated means two views of one history. jj-lib's `GitBackend`
//! already makes every Files checkpoint a real git commit in the root's
//! own object database; what this module adds is the rest of what "a
//! normal git repo" means to git tooling:
//!
//! - **Refs.** A commit no ref points at is unreachable — `git log` would
//!   show nothing and `git push` would have nothing to send. After every
//!   checkpoint the root's checked-out branch is moved to the new commit
//!   ([`publish_checkpoint`]) via jj's own `export_refs`, so clone, fetch,
//!   push, and CI see ordinary branch history.
//! - **The index.** Git compares worktree ⇄ index ⇄ HEAD; a repo whose
//!   index is missing reports every tracked file as deleted-and-untracked,
//!   which no IDE would call normal. [`publish_checkpoint`] rewrites the
//!   index from the checkpoint's tree, so `git status` is clean right
//!   after a checkpoint — the same thing jj does when it moves the
//!   working-copy commit.
//! - **The other direction.** Commits a human (or CI) makes with plain
//!   `git` are imported into the jj view on every open
//!   ([`import_from_git`]), so the Files chain/history RPC reflects them
//!   and the next checkpoint builds on top of them instead of forking
//!   history behind git's back.
//!
//! Adoption falls out of the same seam: pointing `create_root` at a
//! folder that already contains `.git` keeps that repository (and its
//! remotes, and its history) and layers Files on top.

use std::collections::HashMap;
use std::sync::Arc;

use jj_lib::backend::CommitId;
use jj_lib::git::{GitImportOptions, GitSettings};
use jj_lib::object_id::ObjectId as _;
use jj_lib::op_store::RefTarget;
use jj_lib::ref_name::{RefName, RefNameBuf};
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::settings::UserSettings;

use crate::error::{Error, Result};

/// Branch a root's checkpoints land on when git's `HEAD` doesn't name
/// one (a fresh repo whose `HEAD` is unborn is fine — this is the name
/// the first checkpoint gives it).
const FALLBACK_BOOKMARK: &str = "main";

fn import_options(settings: &UserSettings) -> Result<GitImportOptions> {
    let git_settings = GitSettings::from_settings(settings)
        .map_err(|e| Error::Repo(format!("reading git settings: {e}")))?;
    Ok(GitImportOptions {
        abandon_unreachable_commits: git_settings.abandon_unreachable_commits,
        record_synthetic_predecessors: git_settings.record_synthetic_predecessors,
        remote_auto_track_bookmarks: HashMap::new(),
    })
}

/// The local bookmark git's `HEAD` points at, e.g. `main` or `master` —
/// read from the git repo itself so an adopted repository keeps
/// committing to the branch it was already on.
pub fn checked_out_bookmark(repo: &Arc<ReadonlyRepo>) -> Result<RefNameBuf> {
    let git_repo = jj_lib::git::get_git_repo(repo.store())
        .map_err(|e| Error::Repo(format!("not a git-backed root: {e}")))?;
    let name = git_repo
        .head_name()
        .map_err(|e| Error::Repo(format!("reading git HEAD: {e}")))?
        .map(|full| full.shorten().to_string());
    Ok(RefNameBuf::from(
        name.unwrap_or_else(|| FALLBACK_BOOKMARK.to_string()),
    ))
}

/// The commit a software root's next checkpoint builds on: the tip of
/// the checked-out branch, or the root commit when the branch is unborn.
///
/// Deliberately *not* `view().heads().next()` (what media roots use):
/// an adopted repo can have many branches, and picking an arbitrary head
/// would silently commit onto whichever one sorted first.
pub fn head_commit(repo: &Arc<ReadonlyRepo>) -> Result<CommitId> {
    let bookmark = checked_out_bookmark(repo)?;
    let target = repo.view().get_local_bookmark(&bookmark);
    Ok(target
        .as_normal()
        .cloned()
        .unwrap_or_else(|| repo.store().root_commit_id().clone()))
}

/// What Files itself keeps in the tree, hidden from git.
const EXCLUDE_BLOCK: &str = "\
# Files (Task) root internals — the version store and the root marker.
# Written to .git/info/exclude rather than .gitignore: this is Files'
# business with this checkout, not a project decision to commit.
/.fts-files/
/.fts-root.json
";

/// Teach git to ignore a File Root's own internals, via the repo-local
/// `info/exclude` (never the project's `.gitignore`, which belongs to the
/// project and would end up in its commits). Without this, `git status`
/// on a perfectly clean software root reports two untracked entries that
/// no developer put there. Idempotent.
pub fn exclude_root_internals(repo: &Arc<ReadonlyRepo>) -> Result<()> {
    let git_repo = jj_lib::git::get_git_repo(repo.store())
        .map_err(|e| Error::Repo(format!("not a git-backed root: {e}")))?;
    let path = git_repo.common_dir().join("info").join("exclude");
    let existing = std::fs::read_to_string(&path).unwrap_or_default();
    if existing.contains("/.fts-files/") {
        return Ok(());
    }
    if let Some(dir) = path.parent() {
        std::fs::create_dir_all(dir)?;
    }
    let mut contents = existing;
    if !contents.is_empty() && !contents.ends_with('\n') {
        contents.push('\n');
    }
    contents.push_str(EXCLUDE_BLOCK);
    std::fs::write(&path, contents)?;
    Ok(())
}

/// Import whatever git has done since we last looked — an adopted
/// repository's existing history on first touch, plus any commits,
/// fetches, or branch moves made with plain `git` between RPC calls.
pub fn import_from_git(repo: Arc<ReadonlyRepo>) -> Result<Arc<ReadonlyRepo>> {
    let options = import_options(repo.settings())?;
    let mut tx = repo.start_transaction();
    pollster::block_on(jj_lib::git::import_refs(tx.repo_mut(), &options))
        .map_err(|e| Error::Repo(format!("importing git refs: {e}")))?;
    pollster::block_on(jj_lib::git::import_head(tx.repo_mut()))
        .map_err(|e| Error::Repo(format!("importing git HEAD: {e}")))?;
    if tx.repo().has_changes() {
        pollster::block_on(tx.commit("import git refs")).map_err(|e| Error::Repo(e.to_string()))
    } else {
        Ok(repo)
    }
}

/// Make `commit_id` the tip of the root's checked-out branch, in both
/// views: jj's (a local bookmark) and git's (`refs/heads/<branch>` plus a
/// rewritten index). After this, `git log`, `git status`, `git clone`,
/// and `git push` behave exactly as they would in a repository a human
/// had committed to.
pub fn publish_checkpoint(
    repo: Arc<ReadonlyRepo>,
    commit_id: &CommitId,
) -> Result<Arc<ReadonlyRepo>> {
    let bookmark = checked_out_bookmark(&repo)?;
    let mut tx = repo.start_transaction();
    tx.repo_mut().set_local_bookmark_target(
        RefName::new(bookmark.as_str()),
        RefTarget::normal(commit_id.clone()),
    );
    // jj records what it believes git's HEAD is; keeping it in step means
    // the next `import_from_git` sees "nothing moved" rather than
    // mistaking our own export for a git-side change.
    tx.repo_mut()
        .set_git_head_target(RefTarget::normal(commit_id.clone()));
    jj_lib::git::export_refs(tx.repo_mut())
        .map_err(|e| Error::Repo(format!("exporting git refs: {e}")))?;
    let repo =
        pollster::block_on(tx.commit("export git refs")).map_err(|e| Error::Repo(e.to_string()))?;

    attach_head(&repo, &bookmark)?;
    write_git_index(&repo, commit_id)?;
    Ok(repo)
}

/// Point git's `HEAD` back at `refs/heads/<bookmark>`.
///
/// jj's `export_refs` deliberately *detaches* `HEAD` whenever it moves the
/// branch `HEAD` is on — correct for jj's own working-copy model, where
/// the checkout is jj's to own. A software File Root is the opposite
/// case: the checkout belongs to whoever opens the folder, and a
/// permanently detached `HEAD` is exactly the "not a normal repository"
/// state this flavor exists to avoid (`git status` says "HEAD detached",
/// `git push` needs an explicit refspec, IDEs show no branch). So the
/// branch is re-attached after every export — it already points at the
/// checkpoint we just wrote, so attaching changes nothing about what is
/// reachable, only how git presents it.
fn attach_head(repo: &Arc<ReadonlyRepo>, bookmark: &RefNameBuf) -> Result<()> {
    let git_repo = jj_lib::git::get_git_repo(repo.store())
        .map_err(|e| Error::Repo(format!("not a git-backed root: {e}")))?;
    let branch_ref: gix::refs::FullName = format!("refs/heads/{}", bookmark.as_str())
        .try_into()
        .map_err(|e| Error::Repo(format!("invalid branch name {bookmark:?}: {e}")))?;
    git_repo
        .edit_reference(gix::refs::transaction::RefEdit {
            change: gix::refs::transaction::Change::Update {
                log: gix::refs::transaction::LogChange {
                    message: "checkpoint (Files)".into(),
                    ..Default::default()
                },
                expected: gix::refs::transaction::PreviousValue::Any,
                new: gix::refs::Target::Symbolic(branch_ref),
            },
            name: "HEAD"
                .try_into()
                .expect("HEAD is a valid full reference name"),
            deref: false,
        })
        .map_err(|e| Error::Repo(format!("attaching git HEAD: {e}")))?;
    Ok(())
}

/// Rewrite `.git/index` from `commit_id`'s tree so git sees a clean
/// worktree. Mirrors jj-lib's own `reset_index` (private to that crate)
/// for the resolved-tree case; a conflicted tree can't occur here
/// because a checkpoint always writes a resolved tree.
fn write_git_index(repo: &Arc<ReadonlyRepo>, commit_id: &CommitId) -> Result<()> {
    let git_repo = jj_lib::git::get_git_repo(repo.store())
        .map_err(|e| Error::Repo(format!("not a git-backed root: {e}")))?;
    let commit = pollster::block_on(repo.store().get_commit_async(commit_id))?;
    let tree_id =
        commit.tree_ids().as_resolved().cloned().ok_or_else(|| {
            Error::Repo("checkpoint wrote a conflicted tree (unsupported)".into())
        })?;

    let mut index = if &tree_id == repo.store().empty_tree_id() {
        // Git doesn't require the empty tree to be present in the object
        // database, so gix can fail to load it — use an empty index.
        gix::index::File::from_state(
            gix::index::State::new(git_repo.object_hash()),
            git_repo.index_path(),
        )
    } else {
        git_repo
            .index_from_tree(&gix::ObjectId::from_bytes_or_panic(tree_id.as_bytes()))
            .map_err(|e| Error::Repo(format!("building the git index: {e}")))?
    };
    index
        .write(gix::index::write::Options::default())
        .map_err(|e| Error::Repo(format!("writing the git index: {e}")))?;
    Ok(())
}
