//! Internal error type. Converts cleanly into the proto's
//! `AgentError` at the trait boundary.

use agent_proto::AgentError;
use thiserror::Error;

#[derive(Debug, Error)]
pub enum TasksDbError {
    #[error("sqlite: {0}")]
    Sqlite(#[from] rusqlite::Error),

    #[error("json: {0}")]
    Json(#[from] serde_json::Error),

    #[error("not found: {kind} {id}")]
    NotFound { kind: &'static str, id: String },

    /// Conflict on an atomic claim — another worker already
    /// holds it, or the task is in a non-claimable status.
    #[error("conflict: {0}")]
    Conflict(String),

    /// Caller tried to set `status = Running` outside the
    /// claim path, or tried to move a Done task to Blocked.
    #[error("invalid transition: {0}")]
    InvalidTransition(String),

    #[error("invalid: {0}")]
    Invalid(String),
}

impl From<TasksDbError> for AgentError {
    fn from(value: TasksDbError) -> Self {
        match value {
            TasksDbError::NotFound { kind: "queue", id } => AgentError::QueueNotFound(id),
            TasksDbError::NotFound { id, .. } => AgentError::AgentTaskNotFound(id),
            TasksDbError::Conflict(msg) => AgentError::Conflict(msg),
            TasksDbError::InvalidTransition(msg) | TasksDbError::Invalid(msg) => {
                AgentError::Invalid(msg)
            }
            other => AgentError::Backend(other.to_string()),
        }
    }
}
