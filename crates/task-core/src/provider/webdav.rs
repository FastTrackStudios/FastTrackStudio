//! WebDAV / Nextcloud provider.
//!
//! Reads/writes projects over WebDAV, which Nextcloud exposes for all
//! user files. This allows the Task app to access project files on a
//! Nextcloud server without NFS — just HTTPS.
//!
//! ```text
//! https://cloud.example.com/remote.php/dav/files/codywright/Projects/
//! ├── Montreal Album/project.md
//! ├── Montreal Album/tasks/Mix track 1.md
//! └── ...
//! ```
//!
//! Currently a stub — requires `reqwest` + WebDAV XML parsing.

use async_trait::async_trait;

use super::traits::*;
use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;

/// Configuration for a WebDAV/Nextcloud connection.
#[derive(Debug, Clone)]
pub struct WebDavConfig {
    /// WebDAV base URL (e.g. "https://cloud.example.com/remote.php/dav/files/codywright/").
    pub url: String,
    /// Username for authentication.
    pub username: String,
    /// Password or app token.
    pub password: String,
    /// Path within the WebDAV share to the Projects directory (e.g. "Projects/").
    pub projects_path: String,
}

/// WebDAV-based project provider for Nextcloud and other WebDAV servers.
pub struct WebDavProvider {
    info: ProviderInfo,
    config: WebDavConfig,
}

impl WebDavProvider {
    pub fn new(name: impl Into<String>, label: impl Into<String>, config: WebDavConfig) -> Self {
        Self {
            info: ProviderInfo {
                name: name.into(),
                label: label.into(),
                kind: "webdav".into(),
                writable: true,
            },
            config,
        }
    }

    fn projects_url(&self) -> String {
        format!(
            "{}/{}",
            self.config.url.trim_end_matches('/'),
            self.config.projects_path.trim_start_matches('/')
        )
    }
}

#[async_trait]
impl ProjectProvider for WebDavProvider {
    fn info(&self) -> &ProviderInfo {
        &self.info
    }

    async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        // TODO: PROPFIND on projects_url with Depth: 1 to list project folders,
        // then GET each project.md and parse YAML frontmatter.
        Err(VaultError::IoError(format!(
            "WebDAV provider not yet implemented for {}",
            self.projects_url()
        )))
    }

    async fn get_project(&self, _title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        Err(VaultError::IoError(
            "WebDAV provider not yet implemented".into(),
        ))
    }

    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        Err(VaultError::IoError(
            "WebDAV provider not yet implemented".into(),
        ))
    }

    async fn create_project(&self, _project: &Project) -> Result<String, VaultError> {
        Err(VaultError::IoError(
            "WebDAV provider not yet implemented".into(),
        ))
    }

    async fn update_project(&self, _project: &Project) -> Result<(), VaultError> {
        Err(VaultError::IoError(
            "WebDAV provider not yet implemented".into(),
        ))
    }

    async fn save_task(&self, _project_title: &str, _task: &Task) -> Result<(), VaultError> {
        Err(VaultError::IoError(
            "WebDAV provider not yet implemented".into(),
        ))
    }

    async fn delete_task(&self, _project_title: &str, _task_title: &str) -> Result<(), VaultError> {
        Err(VaultError::IoError(
            "WebDAV provider not yet implemented".into(),
        ))
    }
}
