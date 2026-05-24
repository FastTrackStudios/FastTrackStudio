//! Server-side [`TaskService`] backend — walks the configured
//! vault on each call.
//!
//! Mirror of `project::ProjectBackend` / `goal::GoalBackend`.
//! Cheap to `Clone`.

use std::path::{Path, PathBuf};

use architect::HasDispatcher;
use architect::dispatch::TokioBlockingDispatcher;
use chrono::Utc;
use uuid::Uuid;
use vault::Vault;

use crate::model::TaskInfo;
use crate::parse::{looks_like_task, parse_page};
use crate::service::{TaskError, TaskService};
use crate::write::{default_task_path, write_task};

#[derive(Debug, Clone)]
pub struct TaskBackend {
    vault_root: PathBuf,
}

impl TaskBackend {
    #[must_use]
    pub fn new(vault_root: impl Into<PathBuf>) -> Self {
        Self {
            vault_root: vault_root.into(),
        }
    }

    #[must_use]
    pub fn vault_root(&self) -> &Path {
        &self.vault_root
    }

    fn list_inner(&self) -> Result<Vec<TaskInfo>, TaskError> {
        let vault = Vault::open(&self.vault_root)
            .map_err(|e| TaskError::Io(format!("open vault {}: {e}", self.vault_root.display())))?;
        let mut out = Vec::new();
        for page in &vault.pages {
            if !looks_like_task(page) {
                continue;
            }
            match parse_page(page) {
                Ok(t) => out.push(t),
                Err(e) => tracing::warn!(path = %page.rel_path, ?e, "task parse failed"),
            }
        }
        Ok(out)
    }
}

impl HasDispatcher for TaskBackend {
    type Dispatcher = TokioBlockingDispatcher;
    fn dispatcher(&self) -> Self::Dispatcher {
        TokioBlockingDispatcher
    }
}

impl TaskService for TaskBackend {
    fn list(&self) -> Result<Vec<TaskInfo>, TaskError> {
        self.list_inner()
    }

    fn get(&self, id: Uuid) -> Result<TaskInfo, TaskError> {
        self.list_inner()?
            .into_iter()
            .find(|t| t.id == id)
            .ok_or_else(|| TaskError::NotFound(id.to_string()))
    }

    fn get_by_path(&self, path: &str) -> Result<TaskInfo, TaskError> {
        self.list_inner()?
            .into_iter()
            .find(|t| t.path == path)
            .ok_or_else(|| TaskError::NotFound(path.to_owned()))
    }

    fn create(&self, mut task: TaskInfo) -> Result<TaskInfo, TaskError> {
        if task.title.trim().is_empty() {
            return Err(TaskError::BadRequest("title is required".into()));
        }
        if task.id.is_nil() {
            task.id = Uuid::new_v4();
        }
        if task.path.is_empty() {
            task.path = default_task_path(&task.title, None);
        }
        let now = Utc::now();
        if task.date_created.is_none() {
            task.date_created = Some(now);
        }
        task.date_modified = Some(now);

        let abs = self.vault_root.join(&task.path);
        if abs.exists() {
            return Err(TaskError::AlreadyExists(task.path.clone()));
        }
        write_task(&self.vault_root, &mut task, false)
            .map_err(|e| TaskError::Io(format!("write: {e}")))?;
        Ok(task)
    }

    fn update(&self, task: TaskInfo) -> Result<TaskInfo, TaskError> {
        let existing = self
            .list_inner()?
            .into_iter()
            .find(|t| t.id == task.id)
            .ok_or_else(|| TaskError::NotFound(task.id.to_string()))?;
        let mut next = task;
        next.path = existing.path;
        next.date_created = existing.date_created.or(next.date_created);
        next.date_modified = Some(Utc::now());
        write_task(&self.vault_root, &mut next, true)
            .map_err(|e| TaskError::Io(format!("write: {e}")))?;
        Ok(next)
    }

    fn rename(&self, id: Uuid, new_path: &str) -> Result<TaskInfo, TaskError> {
        if new_path.is_empty() || new_path.contains("..") || new_path.starts_with('/') {
            return Err(TaskError::BadRequest(format!("bad path: {new_path}")));
        }
        let mut t = self
            .list_inner()?
            .into_iter()
            .find(|t| t.id == id)
            .ok_or_else(|| TaskError::NotFound(id.to_string()))?;
        let from = self.vault_root.join(&t.path);
        let to = self.vault_root.join(new_path);
        if to.exists() {
            return Err(TaskError::AlreadyExists(new_path.to_owned()));
        }
        if let Some(parent) = to.parent() {
            std::fs::create_dir_all(parent).map_err(|e| TaskError::Io(format!("mkdir: {e}")))?;
        }
        std::fs::rename(&from, &to).map_err(|e| TaskError::Io(format!("rename: {e}")))?;
        t.path = new_path.to_owned();
        t.date_modified = Some(Utc::now());
        write_task(&self.vault_root, &mut t, true)
            .map_err(|e| TaskError::Io(format!("write: {e}")))?;
        Ok(t)
    }

    fn delete(&self, id: Uuid) -> Result<(), TaskError> {
        let t = self
            .list_inner()?
            .into_iter()
            .find(|t| t.id == id)
            .ok_or_else(|| TaskError::NotFound(id.to_string()))?;
        let abs = self.vault_root.join(&t.path);
        std::fs::remove_file(&abs)
            .map_err(|e| TaskError::Io(format!("remove {}: {e}", abs.display())))?;
        Ok(())
    }
}
