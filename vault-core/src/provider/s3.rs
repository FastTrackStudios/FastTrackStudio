//! S3-compatible object storage provider.
//!
//! Reads/writes the same `project.md` + `tasks/*.md` format, stored as
//! S3 objects with key prefixes mapping to project folders.
//!
//! ```text
//! s3://my-bucket/projects/
//! ├── Montreal Album/project.md
//! ├── Montreal Album/tasks/Mix track 1.md
//! └── Website Redesign/project.md
//! ```
//!
//! Currently a stub — requires `aws-sdk-s3` or `rusoto` dependency.
//! The interface is defined so the rest of the system can code against it.

use async_trait::async_trait;

use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;
use super::traits::*;

/// Configuration for an S3-compatible storage backend.
#[derive(Debug, Clone)]
pub struct S3Config {
    /// Bucket name.
    pub bucket: String,
    /// Key prefix for projects (e.g. "projects/").
    pub prefix: String,
    /// S3 endpoint URL (for MinIO, R2, etc.). None = AWS default.
    pub endpoint: Option<String>,
    /// AWS region.
    pub region: String,
    /// Access key ID.
    pub access_key: String,
    /// Secret access key.
    pub secret_key: String,
}

/// S3-compatible project provider.
///
/// Stores projects as:
/// - `{prefix}{project_title}/project.md` — project metadata
/// - `{prefix}{project_title}/tasks/{task_title}.md` — individual tasks
pub struct S3Provider {
    info: ProviderInfo,
    config: S3Config,
}

impl S3Provider {
    pub fn new(name: impl Into<String>, label: impl Into<String>, config: S3Config) -> Self {
        Self {
            info: ProviderInfo {
                name: name.into(),
                label: label.into(),
                kind: "s3".into(),
                writable: true,
            },
            config,
        }
    }

    fn project_key(&self, title: &str) -> String {
        format!("{}{}/project.md", self.config.prefix, title)
    }

    fn task_key(&self, project_title: &str, task_title: &str) -> String {
        format!("{}{}/tasks/{}.md", self.config.prefix, project_title, task_title)
    }

    fn tasks_prefix(&self, project_title: &str) -> String {
        format!("{}{}/tasks/", self.config.prefix, project_title)
    }
}

#[async_trait]
impl ProjectProvider for S3Provider {
    fn info(&self) -> &ProviderInfo {
        &self.info
    }

    async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        // TODO: ListObjectsV2 with prefix + delimiter to find project folders,
        // then GetObject each project.md and parse.
        Err(VaultError::IoError("S3 provider not yet implemented — add aws-sdk-s3 dependency".into()))
    }

    async fn get_project(&self, _title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        Err(VaultError::IoError("S3 provider not yet implemented".into()))
    }

    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        Err(VaultError::IoError("S3 provider not yet implemented".into()))
    }

    async fn create_project(&self, _project: &Project) -> Result<String, VaultError> {
        Err(VaultError::IoError("S3 provider not yet implemented".into()))
    }

    async fn update_project(&self, _project: &Project) -> Result<(), VaultError> {
        Err(VaultError::IoError("S3 provider not yet implemented".into()))
    }

    async fn save_task(&self, _project_title: &str, _task: &Task) -> Result<(), VaultError> {
        Err(VaultError::IoError("S3 provider not yet implemented".into()))
    }

    async fn delete_task(&self, _project_title: &str, _task_title: &str) -> Result<(), VaultError> {
        Err(VaultError::IoError("S3 provider not yet implemented".into()))
    }
}
