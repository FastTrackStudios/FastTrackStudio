//! Local filesystem provider — reads project folders from a local directory.
//!
//! Works with NFS mounts, local drives, USB drives — anything that looks
//! like a directory on the filesystem.

use std::path::{Path, PathBuf};

use async_trait::async_trait;

use crate::project::Project;
use crate::project_vault;
use crate::service::VaultError;
use crate::task::Task;
use super::traits::*;

/// Reads projects from a local `Projects/` directory.
///
/// ```text
/// /mnt/starcommand/Projects/
/// ├── Montreal Album/
/// │   ├── project.md
/// │   └── tasks/*.md
/// └── Website Redesign/
///     ├── project.md
///     └── tasks/*.md
/// ```
pub struct LocalProvider {
    info: ProviderInfo,
    root: PathBuf,
}

impl LocalProvider {
    pub fn new(name: impl Into<String>, label: impl Into<String>, root: impl AsRef<Path>) -> Self {
        Self {
            info: ProviderInfo {
                name: name.into(),
                label: label.into(),
                kind: "local".into(),
                writable: true,
            },
            root: root.as_ref().to_path_buf(),
        }
    }
}

#[async_trait]
impl ProjectProvider for LocalProvider {
    fn info(&self) -> &ProviderInfo {
        &self.info
    }

    async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        let bundles = project_vault::scan_project_vault(&self.root);
        Ok(bundles.into_iter().map(|b| b.project).collect())
    }

    async fn get_project(&self, title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        let bundles = project_vault::scan_project_vault(&self.root);
        Ok(bundles.into_iter().find(|b| b.project.title == title).map(|b| {
            ProjectBundle {
                project: b.project,
                tasks: b.tasks,
                location: b.path.to_string_lossy().to_string(),
                source: self.info.name.clone(),
            }
        }))
    }

    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        let bundles = project_vault::scan_project_vault(&self.root);
        Ok(bundles
            .into_iter()
            .map(|b| ProjectBundle {
                location: b.path.to_string_lossy().to_string(),
                source: self.info.name.clone(),
                project: b.project,
                tasks: b.tasks,
            })
            .collect())
    }

    async fn create_project(&self, project: &Project) -> Result<String, VaultError> {
        let path = project_vault::create_project(&self.root, project)?;
        Ok(path.to_string_lossy().to_string())
    }

    async fn update_project(&self, project: &Project) -> Result<(), VaultError> {
        let project_dir = self.root.join(&project.title);
        project_vault::save_project_metadata(&project_dir, project)
    }

    async fn save_task(&self, project_title: &str, task: &Task) -> Result<(), VaultError> {
        let project_dir = self.root.join(project_title);
        project_vault::save_project_task(&project_dir, task)
    }

    async fn delete_task(&self, project_title: &str, task_title: &str) -> Result<(), VaultError> {
        let path = self.root.join(project_title).join("tasks").join(format!("{}.md", task_title));
        if path.exists() {
            std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
        }
        Ok(())
    }

    async fn watch(&self) -> Result<tokio::sync::mpsc::Receiver<ProviderEvent>, VaultError> {
        let (tx, rx) = tokio::sync::mpsc::channel(32);
        let root = self.root.clone();

        // Use notify crate for filesystem watching
        tokio::spawn(async move {
            use crate::watch::start_watch;
            let (change_tx, _change_rx) = tokio::sync::watch::channel(0u64);
            if let Ok(_handle) = start_watch(&root, change_tx.clone()) {
                let mut watch_rx = change_tx.subscribe();
                loop {
                    if watch_rx.changed().await.is_err() {
                        break;
                    }
                    let _ = tx.send(ProviderEvent::Refresh).await;
                }
            }
        });

        Ok(rx)
    }
}
