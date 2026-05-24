//! `TaskService` — wire surface for reading + mutating tasks.
//!
//! Same shape as `ProjectService` / `GoalService` / `LocationsService`:
//! `create` / `update` take the full record, `rename` is the
//! only path-changing op, `delete` removes the file.
//!
//! No special filters on `list()` yet — clients filter
//! client-side after fetching. The CLI is a thin wrapper that
//! adds `--status`, `--tag`, `--context`, `--project`,
//! `--milestone` etc.

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;

use crate::model::TaskInfo;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum TaskError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("io: {0}")]
    Io(String),
}

#[architect::rpc]
pub trait TaskService {
    /// Every task page under the org's vault.
    fn list(&self) -> Result<Vec<TaskInfo>, TaskError>;

    fn get(&self, id: Uuid) -> Result<TaskInfo, TaskError>;

    fn get_by_path(&self, path: &str) -> Result<TaskInfo, TaskError>;

    /// Create a task. Backend assigns `task.path`
    /// (default `tasks/<slug>.md`) and `task.id` when nil.
    fn create(&self, task: TaskInfo) -> Result<TaskInfo, TaskError>;

    /// Replace the task whose `id` matches. Path mutations
    /// ignored — use [`Self::rename`].
    fn update(&self, task: TaskInfo) -> Result<TaskInfo, TaskError>;

    /// Move the backing markdown file. `id` preserved.
    fn rename(&self, id: Uuid, new_path: &str) -> Result<TaskInfo, TaskError>;

    /// Remove the backing file.
    fn delete(&self, id: Uuid) -> Result<(), TaskError>;
}
