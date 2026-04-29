//! Mock project provider for testing.
//!
//! In-memory storage — no filesystem, no network. Simulates a full
//! provider with configurable latency and failure injection.

use std::collections::HashMap;
use std::sync::Arc;

use async_trait::async_trait;
use tokio::sync::Mutex;

use super::traits::*;
use crate::project::Project;
use crate::service::VaultError;
use crate::task::Task;

/// In-memory project provider for testing.
///
/// All data lives in memory. Supports concurrent access via Mutex.
/// Can inject latency and errors for testing edge cases.
pub struct MockProvider {
    info: ProviderInfo,
    projects: Arc<Mutex<HashMap<String, Project>>>,
    tasks: Arc<Mutex<HashMap<String, Vec<Task>>>>,
    /// Optional latency to simulate network delay (milliseconds).
    latency_ms: Option<u64>,
    /// If set, all writes will fail with this error.
    fail_writes: Arc<Mutex<Option<String>>>,
    /// Change event sender.
    event_tx: tokio::sync::mpsc::Sender<ProviderEvent>,
    event_rx: Arc<tokio::sync::Mutex<tokio::sync::mpsc::Receiver<ProviderEvent>>>,
}

impl MockProvider {
    pub fn new(name: &str) -> Self {
        let (event_tx, event_rx) = tokio::sync::mpsc::channel(64);
        Self {
            info: ProviderInfo {
                name: name.to_string(),
                label: format!("Mock: {name}"),
                kind: "mock".to_string(),
                writable: true,
            },
            projects: Arc::new(Mutex::new(HashMap::new())),
            tasks: Arc::new(Mutex::new(HashMap::new())),
            latency_ms: None,
            fail_writes: Arc::new(Mutex::new(None)),
            event_tx,
            event_rx: Arc::new(tokio::sync::Mutex::new(event_rx)),
        }
    }

    /// Set simulated network latency.
    pub fn with_latency(mut self, ms: u64) -> Self {
        self.latency_ms = Some(ms);
        self
    }

    /// Make all writes fail with an error message.
    pub async fn set_fail_writes(&self, msg: Option<String>) {
        *self.fail_writes.lock().await = msg;
    }

    /// Seed with test data.
    pub async fn seed_project(&self, project: Project, tasks: Vec<Task>) {
        let title = project.title.clone();
        self.projects.lock().await.insert(title.clone(), project);
        self.tasks.lock().await.insert(title, tasks);
    }

    /// Get raw task count (for test assertions).
    pub async fn task_count(&self) -> usize {
        self.tasks.lock().await.values().map(|v| v.len()).sum()
    }

    /// Get raw project count.
    pub async fn project_count(&self) -> usize {
        self.projects.lock().await.len()
    }

    async fn maybe_delay(&self) {
        if let Some(ms) = self.latency_ms {
            tokio::time::sleep(std::time::Duration::from_millis(ms)).await;
        }
    }

    async fn check_write_allowed(&self) -> Result<(), VaultError> {
        if let Some(ref msg) = *self.fail_writes.lock().await {
            Err(VaultError::IoError(msg.clone()))
        } else {
            Ok(())
        }
    }
}

#[async_trait]
impl ProjectProvider for MockProvider {
    fn info(&self) -> &ProviderInfo {
        &self.info
    }

    async fn list_projects(&self) -> Result<Vec<Project>, VaultError> {
        self.maybe_delay().await;
        Ok(self.projects.lock().await.values().cloned().collect())
    }

    async fn get_project(&self, title: &str) -> Result<Option<ProjectBundle>, VaultError> {
        self.maybe_delay().await;
        let projects = self.projects.lock().await;
        let tasks = self.tasks.lock().await;

        let Some(project) = projects.get(title) else {
            return Ok(None);
        };

        let project_tasks = tasks.get(title).cloned().unwrap_or_default();

        Ok(Some(ProjectBundle {
            project: project.clone(),
            tasks: project_tasks,
            location: format!("mock://{title}"),
            source: self.info.name.clone(),
        }))
    }

    async fn list_all(&self) -> Result<Vec<ProjectBundle>, VaultError> {
        self.maybe_delay().await;
        let projects = self.projects.lock().await;
        let tasks = self.tasks.lock().await;

        Ok(projects
            .values()
            .map(|p| {
                let project_tasks = tasks.get(&p.title).cloned().unwrap_or_default();
                ProjectBundle {
                    project: p.clone(),
                    tasks: project_tasks,
                    location: format!("mock://{}", p.title),
                    source: self.info.name.clone(),
                }
            })
            .collect())
    }

    async fn create_project(&self, project: &Project) -> Result<String, VaultError> {
        self.maybe_delay().await;
        self.check_write_allowed().await?;

        self.projects
            .lock()
            .await
            .insert(project.title.clone(), project.clone());
        self.tasks
            .lock()
            .await
            .insert(project.title.clone(), Vec::new());

        let _ = self
            .event_tx
            .send(ProviderEvent::ProjectChanged {
                project: project.title.clone(),
            })
            .await;

        Ok(format!("mock://{}", project.title))
    }

    async fn update_project(&self, project: &Project) -> Result<(), VaultError> {
        self.maybe_delay().await;
        self.check_write_allowed().await?;

        self.projects
            .lock()
            .await
            .insert(project.title.clone(), project.clone());

        Ok(())
    }

    async fn save_task(&self, project_title: &str, task: &Task) -> Result<(), VaultError> {
        self.maybe_delay().await;
        self.check_write_allowed().await?;

        let mut tasks = self.tasks.lock().await;
        let project_tasks = tasks.entry(project_title.to_string()).or_default();

        // Update or insert
        if let Some(existing) = project_tasks.iter_mut().find(|t| t.title == task.title) {
            *existing = task.clone();
        } else {
            project_tasks.push(task.clone());
        }

        let _ = self
            .event_tx
            .send(ProviderEvent::TaskChanged {
                project: project_title.to_string(),
                task_title: task.title.clone(),
            })
            .await;

        Ok(())
    }

    async fn delete_task(&self, project_title: &str, task_title: &str) -> Result<(), VaultError> {
        self.maybe_delay().await;
        self.check_write_allowed().await?;

        let mut tasks = self.tasks.lock().await;
        if let Some(project_tasks) = tasks.get_mut(project_title) {
            project_tasks.retain(|t| t.title != task_title);
        }

        let _ = self
            .event_tx
            .send(ProviderEvent::TaskDeleted {
                project: project_title.to_string(),
                task_title: task_title.to_string(),
            })
            .await;

        Ok(())
    }

    async fn watch(&self) -> Result<tokio::sync::mpsc::Receiver<ProviderEvent>, VaultError> {
        let _rx = self.event_rx.lock().await;
        // Create a new channel pair — the original rx is consumed
        let (_tx, new_rx) = tokio::sync::mpsc::channel(64);
        Ok(new_rx)
    }
}
