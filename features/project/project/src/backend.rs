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
use chrono::Utc;
use uuid::Uuid;
use vault::Vault;

use crate::model::ProjectInfo;
use crate::scan::scan_vault;
use crate::service::{ProjectError, ProjectService};
use crate::write::{default_project_path, write_project};

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

    fn create(&self, mut project: ProjectInfo) -> Result<ProjectInfo, ProjectError> {
        if project.title.trim().is_empty() {
            return Err(ProjectError::BadRequest("title is required".into()));
        }
        if project.id.is_nil() {
            project.id = Uuid::new_v4();
        }
        if project.path.is_empty() {
            project.path = default_project_path(&project.title);
        }
        let now = Utc::now();
        if project.date_created.is_none() {
            project.date_created = Some(now);
        }
        project.date_modified = Some(now);

        let abs = self.vault_root.join(&project.path);
        if abs.exists() {
            return Err(ProjectError::AlreadyExists(project.path.clone()));
        }
        write_project(&self.vault_root, &mut project, false)
            .map_err(|e| ProjectError::Io(format!("write: {e}")))?;
        Ok(project)
    }

    fn update(&self, project: ProjectInfo) -> Result<ProjectInfo, ProjectError> {
        let existing = self
            .list_inner()?
            .into_iter()
            .find(|p| p.id == project.id)
            .ok_or_else(|| ProjectError::NotFound(project.id.to_string()))?;
        // Carry the on-disk path forward — caller cannot
        // smuggle a rename through `update`.
        let mut next = project;
        next.path = existing.path;
        next.date_created = existing.date_created.or(next.date_created);
        next.date_modified = Some(Utc::now());
        write_project(&self.vault_root, &mut next, true)
            .map_err(|e| ProjectError::Io(format!("write: {e}")))?;
        Ok(next)
    }

    fn rename(&self, id: Uuid, new_path: &str) -> Result<ProjectInfo, ProjectError> {
        if new_path.is_empty() || new_path.contains("..") || new_path.starts_with('/') {
            return Err(ProjectError::BadRequest(format!("bad path: {new_path}")));
        }
        let mut p = self
            .list_inner()?
            .into_iter()
            .find(|p| p.id == id)
            .ok_or_else(|| ProjectError::NotFound(id.to_string()))?;
        let from = self.vault_root.join(&p.path);
        let to = self.vault_root.join(new_path);
        if to.exists() {
            return Err(ProjectError::AlreadyExists(new_path.to_owned()));
        }
        if let Some(parent) = to.parent() {
            std::fs::create_dir_all(parent).map_err(|e| ProjectError::Io(format!("mkdir: {e}")))?;
        }
        std::fs::rename(&from, &to).map_err(|e| ProjectError::Io(format!("rename: {e}")))?;
        p.path = new_path.to_owned();
        p.date_modified = Some(Utc::now());
        // Re-serialize so frontmatter mtime tracks the move.
        write_project(&self.vault_root, &mut p, true)
            .map_err(|e| ProjectError::Io(format!("write: {e}")))?;
        Ok(p)
    }

    fn delete(&self, id: Uuid) -> Result<(), ProjectError> {
        let all = self.list_inner()?;
        let p = all
            .iter()
            .find(|p| p.id == id)
            .ok_or_else(|| ProjectError::NotFound(id.to_string()))?;
        // Refuse if any other project parents off this one —
        // orphans here would silently float to the root in the
        // UI and quietly break agent reasoning.
        if let Some(child) = all.iter().find(|c| c.parent_id == Some(id)) {
            return Err(ProjectError::BadRequest(format!(
                "{} is parent of {}; reparent or delete children first",
                p.title, child.title
            )));
        }
        let abs = self.vault_root.join(&p.path);
        std::fs::remove_file(&abs)
            .map_err(|e| ProjectError::Io(format!("remove {}: {e}", abs.display())))?;
        Ok(())
    }
}
