//! Server-side [`ProjectService`] backend.
//!
//! Walks the configured vault root on each call (cheap — the
//! vault page index lives in memory once `Vault::open` runs).
//! `ProjectBackend` is what the task-server mounts under
//! `/org/<slug>/vox`; the architect-rpc macro emits a sync
//! shim, so the server-bridge can call this directly even
//! though the trait surface is sync.
//!
//! Cheap to `Clone` — the inner [`std::path::PathBuf`] is
//! reused; each request re-opens the vault. Future
//! optimization: cache the parsed list with an mtime check.

use std::path::{Path, PathBuf};

use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
use uuid::Uuid;
use vault::Vault;

use crate::model::ProjectInfo;
use crate::scan::scan_vault;
use crate::service::{ProjectError, ProjectService};

/// File-backed `ProjectService` impl. Built once at server
/// boot per org, cloned into the vox bridge.
#[derive(Debug, Clone)]
pub struct ProjectBackend {
    vault_root: PathBuf,
}

impl ProjectBackend {
    #[must_use]
    pub fn new(vault_root: impl Into<PathBuf>) -> Self {
        Self {
            vault_root: vault_root.into(),
        }
    }

    /// Vault root this backend reads from.
    #[must_use]
    pub fn vault_root(&self) -> &Path {
        &self.vault_root
    }

    fn list_inner(&self) -> Result<Vec<ProjectInfo>, ProjectError> {
        let vault = Vault::open(&self.vault_root).map_err(|e| {
            ProjectError::Io(format!("open vault {}: {e}", self.vault_root.display()))
        })?;
        scan_vault(&vault).map_err(|e| ProjectError::Io(format!("scan: {e}")))
    }
}

impl HasDispatcher for ProjectBackend {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

impl ProjectService for ProjectBackend {
    fn list(&self) -> Result<Vec<ProjectInfo>, ProjectError> {
        self.list_inner()
    }

    fn get(&self, id: Uuid) -> Result<ProjectInfo, ProjectError> {
        self.list_inner()?
            .into_iter()
            .find(|p| p.id == id)
            .ok_or_else(|| ProjectError::NotFound(id.to_string()))
    }

    fn get_by_path(&self, path: &str) -> Result<ProjectInfo, ProjectError> {
        self.list_inner()?
            .into_iter()
            .find(|p| p.path == path)
            .ok_or_else(|| ProjectError::NotFound(path.to_owned()))
    }
}
