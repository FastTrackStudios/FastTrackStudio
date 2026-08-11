//! Opens the `task-files-version-store` jj repo backing one File Root.
//!
//! The open/reopen plumbing itself moved down into the version-store
//! crate (`repo::open_or_init_repo_blocking`, issue #262) — it is the
//! same operation every Storage agent hosting a live tree performs, so
//! it belongs with the engine rather than in one of its consumers. What
//! stays here is the error mapping onto this crate's own [`Error`].
//!
//! **Sync, not async.** jj-lib's own async fns (`load_at_head` in
//! particular, on the divergent-op-heads-merge path) hold a `&dyn Repo`
//! across an await point; `dyn Repo` isn't `Sync`, so that future isn't
//! `Send`. `#[architect::rpc]` methods must return a `MaybeSend`
//! future, so any of this crate's async fns that `.await`ed jj-lib
//! directly would poison the whole RPC method's future — see
//! `backend.rs`'s module doc, and `open_or_init_repo_blocking`'s own.

use std::path::Path;
use std::sync::Arc;

use jj_lib::repo::ReadonlyRepo;

use crate::error::{Error, Result};

/// Open the version-store repo at `repo_path`, initializing it on first
/// touch — this is what makes "root identity survives" (issue #259
/// acceptance criteria) true across a `FilesBackend` restart, not just
/// within one process's lifetime.
pub fn open_or_init_repo(repo_path: &Path) -> Result<Arc<ReadonlyRepo>> {
    task_files_version_store::repo::open_or_init_repo_blocking(repo_path).map_err(Error::from)
}
