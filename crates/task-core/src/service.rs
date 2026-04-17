//! Vox service definitions for vault-core.
//!
//! All vault operations are exposed as a VaultService trait using #[vox::service].
//! Hosts (desktop app, Obsidian plugin, iOS app) implement or connect to this service.

use crate::project::{Project, ProjectStats};
use crate::query::Query;
use crate::task::Task;

/// The primary vault service — exposes task and project operations over Vox RPC.
///
/// Clients connect via:
/// - In-process (desktop, CLI) → direct Rust calls
/// - WebSocket (web, mobile) → Vox over `vox-websocket`
/// - WASM (Obsidian plugin) → `wasm-bindgen` exports
///
/// The same trait generates Swift, TypeScript, and Kotlin clients via Vox codegen.
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

    /// Delete a task by title.
    async fn delete_task(&self, title: String) -> Result<(), VaultError>;

    /// Search tasks by text query (uses FTS5 index).
    async fn search_tasks(&self, query: String) -> Vec<Task>;

    /// Get tasks assigned to a specific user.
    async fn tasks_for_user(&self, username: String) -> Vec<Task>;

    /// Get tasks due on or before a date (YYYY-MM-DD).
    async fn tasks_due_by(&self, date: String) -> Vec<Task>;

    // ── Projects ─────────────────────────────────────────────────────

    /// Return all projects in the vault.
    async fn list_projects(&self) -> Vec<Project>;

    /// Return task count stats for a project.
    async fn project_stats(&self, project_title: String) -> ProjectStats;

    /// Return the next actionable task for a project.
    async fn next_task(&self, project_title: String) -> Option<Task>;

    /// Get all tasks for a project.
    async fn tasks_for_project(&self, project_title: String) -> Vec<Task>;

    // ── Sync ─────────────────────────────────────────────────────────

    /// Trigger a Nextcloud sync cycle. Returns sync stats.
    async fn trigger_sync(&self) -> Result<SyncStats, VaultError>;

    /// Get the last sync result.
    async fn sync_status(&self) -> Option<SyncStats>;
}

/// Sync operation statistics.
#[derive(Debug, Clone, Default, facet::Facet)]
pub struct SyncStats {
    pub timestamp: String,
    pub calendar_pushed: u32,
    pub calendar_pulled: u32,
    pub deck_pushed: u32,
    pub deck_pulled: u32,
    pub files_created: u32,
    pub files_updated: u32,
    pub errors: Vec<String>,
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
