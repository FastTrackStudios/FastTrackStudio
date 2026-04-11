// r[impl api.service]
use std::path::Path;
use std::sync::Arc;

use chrono::Utc;
use tokio::sync::RwLock;
use uuid::Uuid;

use crate::project::{next_task as find_next_task, Project, ProjectStats};
use crate::query::Query;
use crate::rrule;
use crate::service::VaultError;
use crate::task::{Status, Task};
use crate::vault::Vault;
use crate::watch::{start_watch, WatchHandle};

/// A named vault source with its role in the system.
#[derive(Debug, Clone)]
pub struct VaultSource {
    pub name: String,
    pub root: std::path::PathBuf,
    pub kind: VaultKind,
}

/// What kind of content a vault holds.
#[derive(Debug, Clone, PartialEq)]
pub enum VaultKind {
    /// Personal Obsidian vault — tasks, daily notes, personal projects.
    Personal,
    /// Shared project vault (Nextcloud-synced) — Projects/Resources/Archive.
    Projects,
}

#[derive(Clone)]
pub struct VaultServiceImpl {
    /// The primary vault root (personal or first-registered vault).
    root: std::path::PathBuf,
    vault: Arc<RwLock<Vault>>,
    /// Additional vault sources beyond the primary.
    extra_vaults: Arc<RwLock<Vec<VaultSource>>>,
    change_tx: Arc<tokio::sync::watch::Sender<u64>>,
    // Keep one receiver so the sender is never considered "closed".
    _change_rx: tokio::sync::watch::Receiver<u64>,
}

impl VaultServiceImpl {
    pub fn new(vault_root: impl AsRef<Path>) -> Self {
        let root = vault_root.as_ref().to_path_buf();
        let (tx, rx) = tokio::sync::watch::channel(0u64);
        Self {
            vault: Arc::new(RwLock::new(Vault::new(&root))),
            root,
            extra_vaults: Arc::new(RwLock::new(Vec::new())),
            change_tx: Arc::new(tx),
            _change_rx: rx,
        }
    }

    /// Register an additional vault source (e.g. a shared project vault).
    pub async fn add_vault(&self, source: VaultSource) {
        self.extra_vaults.write().await.push(source);
    }

    // r[impl sync.file-watch]
    /// Start watching the vault for file system changes.
    /// Returns a `WatchHandle`; dropping it stops watching.
    pub fn watch(&self) -> notify::Result<WatchHandle> {
        let tx = (*self.change_tx).clone();
        start_watch(&self.root, tx)
    }

    /// Start watching all registered vaults (primary + extras).
    /// Returns handles for each; dropping a handle stops its watcher.
    pub async fn watch_all(&self) -> Vec<notify::Result<WatchHandle>> {
        let mut handles = vec![self.watch()];
        let extras = self.extra_vaults.read().await;
        for src in extras.iter() {
            let tx = (*self.change_tx).clone();
            handles.push(start_watch(&src.root, tx));
        }
        handles
    }

    // r[impl sync.file-watch]
    /// Subscribe to vault change notifications.
    /// The receiver yields a monotonically increasing counter each time the
    /// vault changes (after debounce). Use `changed().await` to wait.
    pub fn subscribe(&self) -> tokio::sync::watch::Receiver<u64> {
        self.change_tx.subscribe()
    }

    // r[impl api.service.list-tasks]
    pub async fn list_tasks(&self) -> Vec<Task> {
        // r[impl query.execute.snapshot]
        self.vault.read().await.load_tasks()
    }

    // r[impl api.service.execute-query]
    pub async fn execute_query(&self, query: Query) -> Vec<Task> {
        let tasks = self.vault.read().await.load_tasks();
        query.execute(&tasks).into_iter().cloned().collect()
    }

    pub async fn urgency_score(&self, task: Task) -> i32 {
        task.urgency_score()
    }

    // r[impl api.service.create-task]
    pub async fn create_task(&self, mut task: Task) -> Result<Task, VaultError> {
        if task.id.is_none() {
            task.id = Some(Uuid::new_v4().to_string());
        }
        let now = Utc::now();
        task.date_created = Some(now);
        task.date_modified = Some(now);
        self.vault.read().await.save_task(&task)?;
        Ok(task)
    }

    // r[impl api.service.update-task]
    pub async fn update_task(&self, mut task: Task) -> Result<Task, VaultError> {
        task.date_modified = Some(Utc::now());
        self.vault.read().await.save_task(&task)?;
        Ok(task)
    }

    // r[impl api.service.complete-task]
    pub async fn complete_task(&self, title: String) -> Result<Task, VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let mut task = tasks
            .into_iter()
            .find(|t| t.title == title)
            .ok_or_else(|| VaultError::NotFound(title.clone()))?;

        let today = chrono::Local::now().date_naive();
        task.date_modified = Some(Utc::now());

        if task.recurrence.is_some() {
            // r[impl task.recurrence.instances]
            let today_str = today.format("%Y-%m-%d").to_string();
            if !task.completed_instances.contains(&today_str) {
                task.completed_instances.push(today_str);
            }
            // Advance scheduled date to next occurrence
            if let Some(next) = rrule::next_occurrence(&task) {
                task.scheduled = Some(next);
            }
        } else {
            // r[impl task.status.transition]
            task.status = Status::Done;
            if task.completed_date.is_none() {
                task.completed_date = Some(today);
            }
        }

        vault.save_task(&task)?;
        Ok(task)
    }

    // ── Projects ─────────────────────────────────────────────────────

    // r[impl api.service.list-projects]
    pub async fn list_projects(&self) -> Vec<Project> {
        let mut projects = self.vault.read().await.load_projects();

        // Also load from extra vaults (shared project vaults).
        let extras = self.extra_vaults.read().await;
        for src in extras.iter() {
            let vault = Vault::new(&src.root);
            match src.kind {
                VaultKind::Projects => {
                    // In project vaults, projects live under Projects/.
                    projects.extend(vault.load_projects_in("Projects"));
                }
                VaultKind::Personal => {
                    projects.extend(vault.load_projects());
                }
            }
        }

        projects
    }

    /// List projects from a specific vault source by name.
    pub async fn list_projects_from(&self, vault_name: &str) -> Vec<Project> {
        let extras = self.extra_vaults.read().await;
        let Some(src) = extras.iter().find(|s| s.name == vault_name) else {
            return vec![];
        };
        let vault = Vault::new(&src.root);
        match src.kind {
            VaultKind::Projects => vault.load_projects_in("Projects"),
            VaultKind::Personal => vault.load_projects(),
        }
    }

    /// Create a project in the shared project vault's Projects/ directory.
    pub async fn create_project(&self, project: Project, vault_name: Option<&str>) -> Result<Project, VaultError> {
        if let Some(name) = vault_name {
            let extras = self.extra_vaults.read().await;
            let src = extras.iter().find(|s| s.name == name)
                .ok_or_else(|| VaultError::NotFound(format!("vault '{}' not registered", name)))?;
            let vault = Vault::new(&src.root);
            vault.save_project_in("Projects", &project)?;
        } else {
            self.vault.read().await.save_project(&project)?;
        }
        Ok(project)
    }

    // r[impl api.service.project-stats]
    pub async fn project_stats(&self, project_title: String) -> ProjectStats {
        let tasks = self.vault.read().await.load_tasks();
        let refs: Vec<&Task> = tasks
            .iter()
            .filter(|t| t.projects.iter().any(|p| p.0 == project_title))
            .collect();
        ProjectStats::from_tasks(&refs)
    }

    // r[impl api.service.next-task]
    pub async fn next_task(&self, project_title: String) -> Option<Task> {
        let tasks = self.vault.read().await.load_tasks();
        find_next_task(&project_title, &tasks).cloned()
    }
}
