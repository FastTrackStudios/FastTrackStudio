//! Project vault — reads the shared `Projects/` directory structure.
//!
//! Each project is a folder with a `project.md` frontmatter file and
//! an optional `tasks/` subdirectory containing task `.md` files.
//!
//! ```text
//! Projects/
//! ├── Montreal Album/
//! │   ├── project.md          ← project metadata
//! │   └── tasks/
//! │       ├── Mix track 1.md  ← task with assignee, due, etc.
//! │       └── Record vocals.md
//! ├── Website Redesign/
//! │   ├── project.md
//! │   └── tasks/
//! │       └── Build navbar.md
//! └── ...
//! ```

use std::fs;
use std::path::{Path, PathBuf};

use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;
use crate::vault::Vault;

/// A loaded project with its tasks.
#[derive(Debug, Clone)]
pub struct ProjectWithTasks {
    pub project: Project,
    pub tasks: Vec<Task>,
    /// Filesystem path to the project directory.
    pub path: PathBuf,
}

/// Scan a shared `Projects/` directory and load all projects + their tasks.
///
/// Expects the structure:
/// ```text
/// root/
/// ├── Project Name/
/// │   ├── project.md      (optional — if missing, infers from folder name)
/// │   └── tasks/
/// │       ├── task1.md
/// │       └── task2.md
/// ```
pub fn scan_project_vault(root: &Path) -> Vec<ProjectWithTasks> {
    let mut results = Vec::new();

    let entries = match fs::read_dir(root) {
        Ok(e) => e,
        Err(_) => return results,
    };

    for entry in entries.flatten() {
        let path = entry.path();
        if !path.is_dir() {
            continue;
        }

        // Skip hidden dirs and special folders
        let dir_name = match path.file_name().and_then(|n| n.to_str()) {
            Some(n) if n.starts_with('.') || n.starts_with('_') => continue,
            Some(n) => n.to_string(),
            None => continue,
        };

        // Try to load project.md
        let project_md = path.join("project.md");
        let project = if project_md.exists() {
            match fs::read_to_string(&project_md) {
                Ok(content) => Vault::parse_project_from_md(&content),
                Err(_) => None,
            }
        } else {
            None
        };

        // If no project.md, create a minimal project from the folder name
        let project = project.unwrap_or_else(|| Project {
            title: dir_name.clone(),
            ..Default::default()
        });

        // Load tasks from tasks/ subdirectory
        let tasks_dir = path.join("tasks");
        let mut tasks = Vec::new();
        if tasks_dir.is_dir() {
            if let Ok(task_entries) = fs::read_dir(&tasks_dir) {
                for task_entry in task_entries.flatten() {
                    let task_path = task_entry.path();
                    if task_path.extension().and_then(|s| s.to_str()) != Some("md") {
                        continue;
                    }
                    if let Ok(content) = fs::read_to_string(&task_path) {
                        if let Some(mut task) = Vault::parse_task_from_md(&content) {
                            // Auto-link task to this project if not already linked
                            let project_link = crate::task::WikiLink(project.title.clone());
                            if !task.projects.contains(&project_link) {
                                task.projects.push(project_link);
                            }
                            tasks.push(task);
                        }
                    }
                }
            }
        }

        // Also scan for .md files directly in the project folder (besides project.md)
        if let Ok(dir_entries) = fs::read_dir(&path) {
            for dir_entry in dir_entries.flatten() {
                let file_path = dir_entry.path();
                if file_path.extension().and_then(|s| s.to_str()) != Some("md") {
                    continue;
                }
                let file_name = file_path.file_name().and_then(|n| n.to_str()).unwrap_or("");
                if file_name == "project.md" {
                    continue; // Already loaded
                }
                // Try parsing as a task
                if let Ok(content) = fs::read_to_string(&file_path) {
                    if let Some(mut task) = Vault::parse_task_from_md(&content) {
                        let project_link = crate::task::WikiLink(project.title.clone());
                        if !task.projects.contains(&project_link) {
                            task.projects.push(project_link);
                        }
                        // Don't duplicate if already loaded from tasks/
                        if !tasks.iter().any(|t| t.title == task.title) {
                            tasks.push(task);
                        }
                    }
                }
            }
        }

        results.push(ProjectWithTasks {
            project,
            tasks,
            path,
        });
    }

    results
}

/// Save a task into a project's `tasks/` directory.
pub fn save_project_task(project_dir: &Path, task: &Task) -> Result<(), VaultError> {
    let tasks_dir = project_dir.join("tasks");
    fs::create_dir_all(&tasks_dir).map_err(|e| VaultError::IoError(e.to_string()))?;

    let path = tasks_dir.join(format!("{}.md", task.title));
    let body = if path.exists() {
        let content = fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
        Vault::extract_body(&content).unwrap_or("").to_string()
    } else {
        String::new()
    };

    let content = Vault::render_task_file(task, &body)?;
    Vault::atomic_write(&path, &content)
}

/// Save a project.md file in a project directory.
pub fn save_project_metadata(project_dir: &Path, project: &Project) -> Result<(), VaultError> {
    let path = project_dir.join("project.md");
    let body = if path.exists() {
        let content = fs::read_to_string(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
        Vault::extract_body(&content).unwrap_or("").to_string()
    } else {
        String::new()
    };

    let content = Vault::render_project_file(project, &body)?;
    Vault::atomic_write(&path, &content)
}

/// Create a new project directory with project.md and tasks/ folder.
pub fn create_project(projects_root: &Path, project: &Project) -> Result<PathBuf, VaultError> {
    let project_dir = projects_root.join(&project.title);
    fs::create_dir_all(project_dir.join("tasks"))
        .map_err(|e| VaultError::IoError(e.to_string()))?;

    save_project_metadata(&project_dir, project)?;

    Ok(project_dir)
}
