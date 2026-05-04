//! Obsidian vault provider — reads tasks from a personal vault directory.
//!
//! Unlike LocalProvider which expects `project/tasks/*.md` structure,
//! this reads flat task files from a vault directory (like Obsidian's TaskNotes/).
//! Tasks may reference projects via their `projects` frontmatter field.

use std::path::{Path, PathBuf};

use async_trait::async_trait;

use super::traits::*;
use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;
use crate::vault::Vault;

/// Reads tasks from a flat Obsidian vault directory.
///
/// ```text
/// ~/Documents/The Observatory/TaskNotes/
/// ├── Buy groceries.md
/// ├── Fix auth bug.md        (projects: [Task App])
/// ├── Mix track 1.md         (projects: [Album])
/// └── Projects/
///     ├── Task App.md        (project metadata)
///     └── Album.md
/// ```
pub struct VaultProvider {
    info: ProviderInfo,
    vault: Vault,
    root: PathBuf,
}

impl VaultProvider {
    pub fn new(name: impl Into<String>, label: impl Into<String>, root: impl AsRef<Path>) -> Self {
        let root = root.as_ref().to_path_buf();
        Self {
            info: ProviderInfo {
                name: name.into(),
                label: label.into(),
                kind: "vault".into(),
                writable: true,
            },
            vault: Vault::new(&root),
            root,
        }
    }
}

#[async_trait]
impl ProjectProvider for VaultProvider {
    fn info(&self) -> &ProviderInfo {
        &self.info
    }

    async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        Ok(self.vault.load_projects())
    }

    async fn get_project(&self, title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        let projects = self.vault.load_projects();
        let project = match projects.into_iter().find(|p| p.title == title) {
            Some(p) => p,
            None => return Ok(None),
        };

        let all_tasks = self.vault.load_tasks();
        let tasks: Vec<Task> = all_tasks
            .into_iter()
            .filter(|t| t.projects.iter().any(|p| p.0 == title))
            .collect();

        Ok(Some(ProjectBundle {
            project,
            tasks,
            location: self.root.to_string_lossy().to_string(),
            source: self.info.name.clone(),
        }))
    }

    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        let projects = self.vault.load_projects();
        let all_tasks = self.vault.load_tasks();

        let mut bundles: Vec<ProjectBundle> = projects
            .into_iter()
            .map(|project| {
                let tasks: Vec<Task> = all_tasks
                    .iter()
                    .filter(|t| t.projects.iter().any(|p| p.0 == project.title))
                    .cloned()
                    .collect();
                ProjectBundle {
                    location: self.root.to_string_lossy().to_string(),
                    source: self.info.name.clone(),
                    project,
                    tasks,
                }
            })
            .collect();

        // Also include "unlinked" tasks (no project) as a virtual "Inbox" bundle
        let unlinked: Vec<Task> = all_tasks
            .into_iter()
            .filter(|t| t.projects.is_empty())
            .collect();
        if !unlinked.is_empty() {
            bundles.push(ProjectBundle {
                project: Project {
                    title: "Inbox".into(),
                    ..Default::default()
                },
                tasks: unlinked,
                location: self.root.to_string_lossy().to_string(),
                source: self.info.name.clone(),
            });
        }

        Ok(bundles)
    }

    async fn list_tasks(&self) -> Result<Vec<Task>, VaultError> {
        Ok(self.vault.load_tasks())
    }

    async fn create_project(&self, project: &Project) -> Result<String, VaultError> {
        self.vault.save_project(project)?;
        Ok(self.root.join(&project.title).to_string_lossy().to_string())
    }

    async fn update_project(&self, project: &Project) -> Result<(), VaultError> {
        self.vault.save_project(project)
    }

    async fn save_task(&self, _project_title: &str, task: &Task) -> Result<(), VaultError> {
        self.vault.save_task(task)
    }

    async fn delete_task(&self, _project_title: &str, task_title: &str) -> Result<(), VaultError> {
        let path = self.root.join(format!("{}.md", task_title));
        if path.exists() {
            std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
        }
        Ok(())
    }
}
