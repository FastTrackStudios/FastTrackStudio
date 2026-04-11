// r[impl task.file]
use std::fs;
use std::path::{Path, PathBuf};

use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;

pub struct Vault {
    pub root: PathBuf,
}

impl Vault {
    pub fn new(root: impl AsRef<Path>) -> Self {
        Self {
            root: root.as_ref().to_path_buf(),
        }
    }

    // r[impl task.file]
    /// Walk the vault and load all parseable tasks.
    pub fn load_tasks(&self) -> Vec<Task> {
        self.walk_md_files()
            .filter_map(|content| Self::parse_task_from_md(&content))
            .collect()
    }

    /// Walk the vault and load all parseable projects.
    pub fn load_projects(&self) -> Vec<Project> {
        self.walk_md_files()
            .filter_map(|content| Self::parse_project_from_md(&content))
            .collect()
    }

    /// Walk a specific subdirectory (relative to root) and load projects.
    pub fn load_projects_in(&self, subdir: &str) -> Vec<Project> {
        let dir = self.root.join(subdir);
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_project_from_md(&content))
            .collect()
    }

    /// Walk a specific subdirectory (relative to root) and load tasks.
    pub fn load_tasks_in(&self, subdir: &str) -> Vec<Task> {
        let dir = self.root.join(subdir);
        if !dir.exists() {
            return vec![];
        }
        Self::walk_md_in(&dir)
            .filter_map(|content| Self::parse_task_from_md(&content))
            .collect()
    }

    // r[impl task.dates.created-modified]
    /// Write a task back to the vault. Uses task.body for content after frontmatter.
    /// If task.body is empty and the file already exists, preserves existing body.
    pub fn save_task(&self, task: &Task) -> Result<(), VaultError> {
        let path = self.task_path(&task.title);
        let body = if !task.body.is_empty() {
            task.body.clone()
        } else if path.exists() {
            let content = fs::read_to_string(&path)
                .map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };

        let content = Self::render_task_file(task, &body)?;
        Self::atomic_write(&path, &content)
    }

    /// Write a project back to the vault. Creates the file if missing, preserves body.
    pub fn save_project(&self, project: &Project) -> Result<(), VaultError> {
        let path = self.project_path(&project.title);
        let body = if path.exists() {
            let content = fs::read_to_string(&path)
                .map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };

        let content = Self::render_project_file(project, &body)?;
        Self::atomic_write(&path, &content)
    }

    /// Write a project to a specific subdirectory (e.g. "Projects").
    pub fn save_project_in(&self, subdir: &str, project: &Project) -> Result<(), VaultError> {
        let dir = self.root.join(subdir);
        let path = dir.join(format!("{}/project.md", project.title));
        let body = if path.exists() {
            let content = fs::read_to_string(&path)
                .map_err(|e| VaultError::IoError(e.to_string()))?;
            Self::extract_body(&content).unwrap_or("").to_string()
        } else {
            String::new()
        };

        let content = Self::render_project_file(project, &body)?;
        Self::atomic_write(&path, &content)
    }

    // ── Helpers ──────────────────────────────────────────────────────

    // r[impl sync.atomic-write]
    pub fn atomic_write(path: &Path, content: &str) -> Result<(), VaultError> {
        let tmp_path = path.with_extension("md.tmp");
        fs::create_dir_all(path.parent().unwrap())
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        fs::write(&tmp_path, content)
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        fs::rename(&tmp_path, path)
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        Ok(())
    }

    fn task_path(&self, title: &str) -> PathBuf {
        self.root.join(format!("{}.md", title))
    }

    fn project_path(&self, title: &str) -> PathBuf {
        self.root.join(format!("{}/project.md", title))
    }

    fn walk_md_files(&self) -> impl Iterator<Item = String> {
        Self::walk_md_in(&self.root)
    }

    fn walk_md_in(dir: &Path) -> impl Iterator<Item = String> {
        walkdir::WalkDir::new(dir)
            .follow_links(false)
            .into_iter()
            .filter_map(|e| e.ok())
            .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("md"))
            .filter_map(|e| fs::read_to_string(e.path()).ok())
    }

    pub fn parse_task_from_md(content: &str) -> Option<Task> {
        let (frontmatter, body) = Self::split_frontmatter(content)?;
        let mut task = facet_yaml::from_str::<Task>(frontmatter).ok()?;
        task.body = body.to_string();
        Some(task)
    }

    pub fn parse_project_from_md(content: &str) -> Option<Project> {
        let (frontmatter, _body) = Self::split_frontmatter(content)?;
        facet_yaml::from_str::<Project>(frontmatter).ok()
    }

    pub fn render_task_file(task: &Task, body: &str) -> Result<String, VaultError> {
        let yaml = facet_yaml::to_string(task)
            .map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        // Use provided body, or fall back to task.body
        let body = if body.is_empty() { &task.body } else { body };
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    pub fn render_project_file(project: &Project, body: &str) -> Result<String, VaultError> {
        let yaml = facet_yaml::to_string(project)
            .map_err(|e| VaultError::ParseError(e.to_string()))?;
        let yaml = yaml.strip_prefix("---\n").unwrap_or(&yaml);
        Ok(format!("---\n{}---\n{}", yaml, body))
    }

    /// Split "---\nFRONTMATTER\n---\nBODY" into (frontmatter, body).
    pub fn split_frontmatter(content: &str) -> Option<(&str, &str)> {
        let content = content.trim_start();
        if !content.starts_with("---") {
            return None;
        }
        let rest = &content[3..];
        let end = rest.find("\n---")?;
        let frontmatter = &rest[..end].trim_start_matches('\n');
        let body = &rest[end + 4..];
        let body = body.trim_start_matches('\n');
        Some((frontmatter, body))
    }

    pub fn extract_body(content: &str) -> Option<&str> {
        Self::split_frontmatter(content).map(|(_, body)| body)
    }

}
