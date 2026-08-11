//! Opens the `task-files-version-store` jj repo backing one File Root.
//! Wraps `task_files_version_store::repo::init_repo` (first touch) with
//! a reopen path through jj-lib's own `RepoLoader` (every subsequent
//! touch, including after a process restart) — the version-store crate
//! only ships the init side (issue #257's own scope), so the reopen
//! plumbing lives here rather than there, per this ticket's "consume
//! the version-store API as-is" boundary with #258's concurrent work.
//!
//! **Sync, not async.** jj-lib's own async fns (`load_at_head` in
//! particular, on the divergent-op-heads-merge path) hold a `&dyn Repo`
//! across an await point; `dyn Repo` isn't `Sync`, so that future isn't
//! `Send`. `#[architect::rpc]` methods must return a `MaybeSend`
//! future, so any of this crate's async fns that `.await`ed jj-lib
//! directly would poison the whole RPC method's future. Driving jj-lib
//! to completion with `pollster::block_on` inside a plain sync fn (same
//! pattern `VersionStoreBackend`'s own `block_on` helper documents for
//! its sync `Backend` methods) keeps the non-Send future entirely off
//! this crate's async call stack — see `backend.rs`'s module doc.

use std::path::Path;
use std::sync::Arc;

use jj_lib::backend::{Backend, BackendLoadError};
use jj_lib::default_backend_factories::default_backend_factories;
use jj_lib::repo::{ReadonlyRepo, RepoLoader};
use task_files_version_store::VersionStoreBackend;

use crate::error::{Error, Result};

/// `ReadonlyRepo::init` (via `init_repo`) requires a directory with no
/// existing repo internals; jj lays those out under `store/` on init,
/// so its presence is what distinguishes "never touched" from
/// "reopen".
fn already_initialized(repo_path: &Path) -> bool {
    repo_path.join("store").exists()
}

/// Open the version-store repo at `repo_path`, initializing it on first
/// touch. Reopening goes through `RepoLoader::init_from_file_system`
/// with our own `VersionStoreBackend` layered onto jj-lib's stock
/// op-store/op-heads-store/index/submodule-store factories
/// (`default_backend_factories`) — this is what makes "root identity
/// survives" (issue #259 acceptance criteria) true across a
/// `FilesBackend` restart, not just within one process's lifetime.
pub fn open_or_init_repo(repo_path: &Path) -> Result<Arc<ReadonlyRepo>> {
    if already_initialized(repo_path) {
        open_existing(repo_path)
    } else {
        pollster::block_on(task_files_version_store::repo::init_repo(repo_path)).map_err(Error::from)
    }
}

fn open_existing(repo_path: &Path) -> Result<Arc<ReadonlyRepo>> {
    let settings =
        task_files_version_store::repo::default_settings().map_err(|e| Error::Repo(e.to_string()))?;

    let mut factories = default_backend_factories();
    factories.add_backend(
        VersionStoreBackend::NAME,
        Box::new(|_settings, store_path| {
            let store_path = store_path.to_path_buf();
            pollster::block_on(VersionStoreBackend::open(&store_path))
                .map(|backend| Box::new(backend) as Box<dyn Backend>)
                .map_err(|e| BackendLoadError(e.into()))
        }),
    );

    let loader = RepoLoader::init_from_file_system(&settings, repo_path, &factories)
        .map_err(|e| Error::Repo(e.to_string()))?;
    pollster::block_on(loader.load_at_head()).map_err(|e| Error::Repo(e.to_string()))
}
