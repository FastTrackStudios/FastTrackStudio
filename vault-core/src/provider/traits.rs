//! Core traits for storage providers.

use async_trait::async_trait;

use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;

/// A project loaded with all its tasks.
#[derive(Debug, Clone)]
pub struct ProjectBundle {
    /// The project metadata.
    pub project: Project,
    /// All tasks belonging to this project.
    pub tasks: Vec<Task>,
    /// Provider-specific location identifier (path, URL, S3 key, etc.)
    pub location: String,
    /// Which provider this came from.
    pub source: String,
}

/// Metadata about a provider instance.
#[derive(Debug, Clone)]
pub struct ProviderInfo {
    /// Unique name for this provider (e.g. "starcommand", "personal-vault", "s3-prod").
    pub name: String,
    /// Human-readable label.
    pub label: String,
    /// Provider type (e.g. "local", "vault", "s3", "webdav").
    pub kind: String,
    /// Whether this provider supports write operations.
    pub writable: bool,
}

/// Change event emitted when a provider detects modifications.
#[derive(Debug, Clone)]
pub enum ProviderEvent {
    /// A task was created or modified.
    TaskChanged { project: String, task_title: String },
    /// A task was deleted.
    TaskDeleted { project: String, task_title: String },
    /// A project was created or modified.
    ProjectChanged { project: String },
    /// A project was deleted.
    ProjectDeleted { project: String },
    /// Full refresh needed (bulk change).
    Refresh,
}

/// Storage-agnostic interface for reading and writing projects + tasks.
///
/// Every storage backend (local filesystem, S3, WebDAV, git) implements
/// this trait. The `ProjectRegistry` aggregates multiple providers into
/// a unified view.
#[async_trait]
pub trait ProjectProvider: Send + Sync {
    /// Provider metadata.
    fn info(&self) -> &ProviderInfo;

    // ── Read ─────────────────────────────────────────────────────────

    /// List all projects (metadata only, no tasks).
    async fn list_projects(&self) -> Result<Vec<Project>, VaultError>;

    /// Load a project with all its tasks.
    async fn get_project(&self, title: &str) -> Result<Option<ProjectBundle>, VaultError>;

    /// Load all projects with all tasks.
    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError>;

    /// List all tasks across all projects.
    async fn list_tasks(&self) -> Result<Vec<Task>, VaultError> {
        let bundles = self.list_all().await?;
        Ok(bundles.into_iter().flat_map(|b| b.tasks).collect())
    }

    /// Get tasks for a specific project.
    async fn get_tasks(&self, project_title: &str) -> Result<Vec<Task>, VaultError> {
        match self.get_project(project_title).await? {
            Some(bundle) => Ok(bundle.tasks),
            None => Ok(vec![]),
        }
    }

    // ── Write ────────────────────────────────────────────────────────

    /// Create a new project. Returns the location.
    async fn create_project(&self, project: &Project) -> Result<String, VaultError>;

    /// Update project metadata.
    async fn update_project(&self, project: &Project) -> Result<(), VaultError>;

    /// Create or update a task within a project.
    async fn save_task(&self, project_title: &str, task: &Task) -> Result<(), VaultError>;

    /// Delete a task from a project.
    async fn delete_task(&self, project_title: &str, task_title: &str) -> Result<(), VaultError>;

    // ── Watch ────────────────────────────────────────────────────────

    /// Subscribe to change events. Returns a receiver.
    /// Default: no-op (polling-based providers override this).
    async fn watch(&self) -> Result<tokio::sync::mpsc::Receiver<ProviderEvent>, VaultError> {
        let (_tx, rx) = tokio::sync::mpsc::channel(1);
        Ok(rx)
    }
}
