// r[impl api.service]
use std::path::Path;
use std::sync::Arc;

use chrono::Utc;
use tokio::sync::RwLock;
use uuid::Uuid;

use crate::index::TaskIndex;
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
    /// SQLite index for fast queries; `None` when unavailable (e.g. WASM).
    pub index: Arc<std::sync::Mutex<Option<TaskIndex>>>,
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

        // Create SQLite index in a .task-index.db file next to the vault
        let index = {
            let index_path = root.join(".task-index.db");
            match TaskIndex::open(&index_path) {
                Ok(idx) => {
                    // Rebuild index from files on startup
                    if let Ok(stats) = idx.rebuild_from_dir(&root) {
                        tracing::info!(tasks = stats.tasks, files = stats.files_scanned, "Index rebuilt");
                    }
                    Some(idx)
                }
                Err(e) => {
                    tracing::warn!(error = %e, "Failed to open index, queries will scan files");
                    None
                }
            }
        };

        Self {
            vault: Arc::new(RwLock::new(Vault::new(&root))),
            root,
            index: Arc::new(std::sync::Mutex::new(index)),
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
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.index_task(&task, &format!("{}.md", task.title));
            }
        }
        Ok(task)
    }

    // r[impl api.service.update-task]
    pub async fn update_task(&self, mut task: Task) -> Result<Task, VaultError> {
        task.date_modified = Some(Utc::now());
        self.vault.read().await.save_task(&task)?;
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.index_task(&task, &format!("{}.md", task.title));
            }
        }
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
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.index_task(&task, &format!("{}.md", task.title));
                let _ = index.record_change("task", &task.title, Some("status"), Some("Open"), Some("Done"), None, None);
            }
        }
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

    pub async fn delete_task(&self, title: String) -> Result<(), VaultError> {
        let path = self.root.join(format!("{}.md", title));
        if path.exists() {
            std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            if let Ok(guard) = self.index.lock() {
                if let Some(ref index) = *guard {
                    let _ = index.record_change("task", &title, None, None, None, None, None);
                }
            }
            Ok(())
        } else {
            Err(VaultError::NotFound(title))
        }
    }

    pub async fn search_tasks(&self, query: String) -> Vec<Task> {
        // Try index first — extract matching titles under the mutex, then drop it
        // before hitting any .await so the MutexGuard doesn't cross an await point.
        let index_titles: Option<std::collections::HashSet<String>> = self
            .index
            .lock()
            .ok()
            .and_then(|guard| {
                guard.as_ref().and_then(|index| {
                    index.search(&query).ok().map(|rows| {
                        rows.iter().map(|r| r.title.clone()).collect()
                    })
                })
            });

        if let Some(matching_titles) = index_titles {
            let all_tasks = self.vault.read().await.load_tasks();
            return all_tasks.into_iter()
                .filter(|t| matching_titles.contains(&t.title))
                .collect();
        }

        // Fallback: scan all tasks
        let tasks = self.vault.read().await.load_tasks();
        let q = query.to_lowercase();
        tasks.into_iter()
            .filter(|t| t.title.to_lowercase().contains(&q) || t.body.to_lowercase().contains(&q))
            .collect()
    }

    pub async fn tasks_for_user(&self, username: String) -> Vec<Task> {
        let tasks = self.vault.read().await.load_tasks();
        tasks.into_iter()
            .filter(|t| t.assignee.as_deref() == Some(&username))
            .collect()
    }

    pub async fn tasks_due_by(&self, date: String) -> Vec<Task> {
        let due_date = match chrono::NaiveDate::parse_from_str(&date, "%Y-%m-%d") {
            Ok(d) => d,
            Err(_) => return vec![],
        };
        let tasks = self.vault.read().await.load_tasks();
        tasks.into_iter()
            .filter(|t| {
                t.due.map(|d| d <= due_date).unwrap_or(false)
                    && !t.is_complete()
            })
            .collect()
    }

    pub async fn tasks_for_project(&self, project_title: String) -> Vec<Task> {
        let tasks = self.vault.read().await.load_tasks();
        tasks.into_iter()
            .filter(|t| t.projects.iter().any(|p| p.0 == project_title))
            .collect()
    }

    pub async fn trigger_sync(&self) -> Result<crate::service::SyncStats, VaultError> {
        // Sync is handled by the server's sync loop — this is a no-op at the service level.
        // The server calls this endpoint to trigger an immediate cycle.
        Ok(crate::service::SyncStats::default())
    }

    pub async fn sync_status(&self) -> Option<crate::service::SyncStats> {
        None
    }

    // ── Time tracking ───────────────────────────────────────────────────────

    /// Start a timer on a task. Returns an error if any task in the vault
    /// already has a running timer — we enforce the "single running timer"
    /// constraint across the whole vault to match the solidtime/Toggl model.
    pub async fn start_timer(
        &self,
        task_ref: &str,
        description: Option<String>,
        billable: bool,
        billable_rate: Option<u32>,
        user: Option<String>,
    ) -> Result<crate::task::TimeEntry, VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();

        if let Some(active) = tasks.iter().find_map(|t| {
            t.running_timer()
                .map(|e| (t.title.clone(), e.id.clone()))
        }) {
            return Err(VaultError::IoError(format!(
                "timer already running on '{}' (id {})",
                active.0, active.1
            )));
        }

        let mut task = tasks
            .into_iter()
            .find(|t| {
                t.id.as_deref() == Some(task_ref) || t.title.eq_ignore_ascii_case(task_ref)
            })
            .ok_or_else(|| VaultError::NotFound(task_ref.to_string()))?;

        let entry = crate::task::TimeEntry {
            id: Uuid::new_v4().to_string(),
            user,
            start_time: Utc::now(),
            end_time: None,
            description,
            billable,
            billable_rate,
            ..Default::default()
        };
        task.time_entries.push(entry.clone());
        task.date_modified = Some(Utc::now());
        vault.save_task(&task)?;
        Ok(entry)
    }

    /// Stop the running timer. If `task_ref` is provided, stops the timer on
    /// that task specifically; otherwise stops whichever task has a running
    /// timer (since we enforce at-most-one across the vault).
    pub async fn stop_timer(
        &self,
        task_ref: Option<&str>,
    ) -> Result<(String, crate::task::TimeEntry), VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();

        let target = tasks
            .into_iter()
            .find(|t| match task_ref {
                Some(r) => {
                    (t.id.as_deref() == Some(r) || t.title.eq_ignore_ascii_case(r))
                        && t.running_timer().is_some()
                }
                None => t.running_timer().is_some(),
            });

        let mut task = target.ok_or_else(|| {
            VaultError::NotFound(match task_ref {
                Some(r) => format!("no running timer on '{r}'"),
                None => "no running timer".into(),
            })
        })?;

        let now = Utc::now();
        let idx = task
            .time_entries
            .iter()
            .position(|e| e.is_running())
            .ok_or_else(|| VaultError::NotFound("running entry vanished".into()))?;
        task.time_entries[idx].end_time = Some(now);
        task.date_modified = Some(now);

        let stopped = task.time_entries[idx].clone();
        let title = task.title.clone();
        vault.save_task(&task)?;
        Ok((title, stopped))
    }

    /// Log a completed time entry manually (for back-dating or bulk import).
    pub async fn log_time(
        &self,
        task_ref: &str,
        start: chrono::DateTime<Utc>,
        end: chrono::DateTime<Utc>,
        description: Option<String>,
        billable: bool,
        billable_rate: Option<u32>,
        user: Option<String>,
    ) -> Result<crate::task::TimeEntry, VaultError> {
        if end <= start {
            return Err(VaultError::ParseError(
                "end must be after start".into(),
            ));
        }
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let mut task = tasks
            .into_iter()
            .find(|t| {
                t.id.as_deref() == Some(task_ref) || t.title.eq_ignore_ascii_case(task_ref)
            })
            .ok_or_else(|| VaultError::NotFound(task_ref.to_string()))?;

        let entry = crate::task::TimeEntry {
            id: Uuid::new_v4().to_string(),
            user,
            start_time: start,
            end_time: Some(end),
            description,
            billable,
            billable_rate,
            ..Default::default()
        };
        task.time_entries.push(entry.clone());
        task.date_modified = Some(Utc::now());
        vault.save_task(&task)?;
        Ok(entry)
    }

    /// Return the currently-running timer across the vault, if any.
    pub async fn active_timer(&self) -> Option<(String, crate::task::TimeEntry)> {
        let tasks = self.vault.read().await.load_tasks();
        for t in tasks {
            if let Some(e) = t.running_timer().cloned() {
                return Some((t.title, e));
            }
        }
        None
    }

    /// List time entries across the vault, each tagged with its task title.
    /// Pass filters to scope by user, task, or date range.
    pub async fn list_time_entries(
        &self,
        filter: TimeEntryFilter,
    ) -> Vec<(String, crate::task::TimeEntry)> {
        let tasks = self.vault.read().await.load_tasks();
        let mut out = Vec::new();
        for t in tasks {
            if let Some(ref r) = filter.task_ref {
                let matches =
                    t.id.as_deref() == Some(r.as_str()) || t.title.eq_ignore_ascii_case(r);
                if !matches {
                    continue;
                }
            }
            for e in &t.time_entries {
                if let Some(ref u) = filter.user {
                    if e.user.as_deref() != Some(u.as_str()) {
                        continue;
                    }
                }
                if let Some(from) = filter.from {
                    if e.start_time < from {
                        continue;
                    }
                }
                if let Some(to) = filter.to {
                    if e.start_time > to {
                        continue;
                    }
                }
                if filter.billable_only && !e.billable {
                    continue;
                }
                out.push((t.title.clone(), e.clone()));
            }
        }
        out
    }

    // ── Conflict log ────────────────────────────────────────────────────────

    /// List conflict rows from the SQLite index. `open_only` filters out
    /// resolved entries.
    pub async fn list_conflicts(
        &self,
        open_only: bool,
        limit: u32,
    ) -> Result<Vec<crate::index::ConflictRow>, VaultError> {
        let guard = self.index.lock().map_err(|_| {
            VaultError::IoError("index poisoned".into())
        })?;
        match &*guard {
            Some(idx) => idx.list_conflicts(open_only, limit),
            None => Ok(Vec::new()),
        }
    }

    /// Mark a conflict resolved.
    pub async fn resolve_conflict(
        &self,
        conflict_id: i64,
        resolver: Option<&str>,
        how: &str,
    ) -> Result<(), VaultError> {
        let guard = self.index.lock().map_err(|_| {
            VaultError::IoError("index poisoned".into())
        })?;
        match &*guard {
            Some(idx) => idx.resolve_conflict(conflict_id, resolver, how),
            None => Err(VaultError::IoError("index unavailable".into())),
        }
    }

    /// Manually record a conflict (used by agents / the future sync
    /// subscriber). Returns the new row id.
    pub async fn record_conflict(
        &self,
        entity_type: &str,
        entity_id: &str,
        field: &str,
        winning_value: Option<&str>,
        losing_value: Option<&str>,
        winning_actor: Option<&str>,
        losing_actor: Option<&str>,
        file_path: Option<&str>,
        kind: &str,
    ) -> Result<i64, VaultError> {
        let guard = self.index.lock().map_err(|_| {
            VaultError::IoError("index poisoned".into())
        })?;
        match &*guard {
            Some(idx) => idx.record_conflict(
                entity_type,
                entity_id,
                field,
                winning_value,
                losing_value,
                winning_actor,
                losing_actor,
                file_path,
                kind,
            ),
            None => Err(VaultError::IoError("index unavailable".into())),
        }
    }

    // ── Activity feed ───────────────────────────────────────────────────────

    /// Recent changes across the vault (audit trail).
    pub async fn recent_activity(
        &self,
        limit: u32,
    ) -> Result<Vec<crate::index::ChangeRow>, VaultError> {
        let guard = self.index.lock().map_err(|_| {
            VaultError::IoError("index poisoned".into())
        })?;
        match &*guard {
            Some(idx) => idx.recent_changes(limit),
            None => Ok(Vec::new()),
        }
    }

    /// Delete a time entry by id.
    pub async fn delete_time_entry(&self, entry_id: &str) -> Result<(), VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        for mut t in tasks {
            let before = t.time_entries.len();
            t.time_entries.retain(|e| e.id != entry_id);
            if t.time_entries.len() != before {
                t.date_modified = Some(Utc::now());
                vault.save_task(&t)?;
                return Ok(());
            }
        }
        Err(VaultError::NotFound(format!(
            "time entry {entry_id}"
        )))
    }
}

/// Filter for [`VaultServiceImpl::list_time_entries`].
#[derive(Debug, Clone, Default)]
pub struct TimeEntryFilter {
    pub task_ref: Option<String>,
    pub user: Option<String>,
    pub from: Option<chrono::DateTime<Utc>>,
    pub to: Option<chrono::DateTime<Utc>>,
    pub billable_only: bool,
}

// ── VaultService trait implementation ────────────────────────────────────────
// Formally implements the #[vox::service] trait so that VaultServiceDispatcher
// can wrap VaultServiceImpl for Vox RPC serving.

impl crate::service::VaultService for VaultServiceImpl {
    async fn list_tasks(&self) -> Vec<Task> { self.list_tasks().await }
    async fn execute_query(&self, query: Query) -> Vec<Task> { self.execute_query(query).await }
    async fn urgency_score(&self, task: Task) -> i32 { self.urgency_score(task).await }
    async fn create_task(&self, task: Task) -> Result<Task, VaultError> { self.create_task(task).await }
    async fn update_task(&self, task: Task) -> Result<Task, VaultError> { self.update_task(task).await }
    async fn complete_task(&self, title: String) -> Result<Task, VaultError> { self.complete_task(title).await }
    async fn delete_task(&self, title: String) -> Result<(), VaultError> { self.delete_task(title).await }
    async fn search_tasks(&self, query: String) -> Vec<Task> { self.search_tasks(query).await }
    async fn tasks_for_user(&self, username: String) -> Vec<Task> { self.tasks_for_user(username).await }
    async fn tasks_due_by(&self, date: String) -> Vec<Task> { self.tasks_due_by(date).await }
    async fn list_projects(&self) -> Vec<Project> { self.list_projects().await }
    async fn project_stats(&self, project_title: String) -> ProjectStats { self.project_stats(project_title).await }
    async fn next_task(&self, project_title: String) -> Option<Task> { self.next_task(project_title).await }
    async fn tasks_for_project(&self, project_title: String) -> Vec<Task> { self.tasks_for_project(project_title).await }
    async fn trigger_sync(&self) -> Result<crate::service::SyncStats, VaultError> { self.trigger_sync().await }
    async fn sync_status(&self) -> Option<crate::service::SyncStats> { self.sync_status().await }
}
