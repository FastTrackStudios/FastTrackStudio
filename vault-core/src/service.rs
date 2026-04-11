//! Vox service definitions for vault-core.
//!
//! All vault operations are exposed as a VaultService trait using #[vox::service].
//! Hosts (desktop app, Obsidian plugin, iOS app) implement or connect to this service.

use crate::project::{Project, ProjectStats};
use crate::query::Query;
use crate::task::Task;

/// The primary vault service — exposes task and project operations over Vox RPC.
#[vox::service]
pub trait VaultService {
    // ── Tasks ────────────────────────────────────────────────────────

    /// Return all tasks in the vault.
    async fn list_tasks(&self) -> Vec<Task>;

    /// Execute a query and return matching tasks sorted by the query's sort.
    async fn execute_query(&self, query: Query) -> Vec<Task>;

    /// Compute the urgency score for a single task.
    async fn urgency_score(&self, task: Task) -> i32;

    /// Create a new task. Returns the created task with generated id/dates filled in.
    async fn create_task(&self, task: Task) -> Result<Task, VaultError>;

    /// Update an existing task. Returns the updated task.
    async fn update_task(&self, task: Task) -> Result<Task, VaultError>;

    /// Mark a task complete. Handles recurrence logic and sets completedDate.
    async fn complete_task(&self, title: String) -> Result<Task, VaultError>;

    // ── Projects ─────────────────────────────────────────────────────

    /// Return all projects in the vault.
    async fn list_projects(&self) -> Vec<Project>;

    /// Return task count stats for a project.
    async fn project_stats(&self, project_title: String) -> ProjectStats;

    /// Return the next actionable task for a project.
    // r[impl project.computed.next-task]
    async fn next_task(&self, project_title: String) -> Option<Task>;
}

/// Errors returned by vault operations.
#[derive(Debug, facet::Facet, thiserror::Error)]
#[repr(C)]
pub enum VaultError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("parse error: {0}")]
    ParseError(String),
    #[error("io error: {0}")]
    IoError(String),
}
