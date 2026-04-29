// r[impl api.service]
use std::path::Path;
use std::sync::Arc;

use base64::{engine::general_purpose::STANDARD as BASE64, Engine as _};
use chrono::{NaiveDate, Utc};
use tokio::sync::RwLock;
use uuid::Uuid;

use crate::index::TaskIndex;
use crate::project::{next_task as find_next_task, Project, ProjectStats};
use crate::query::Query;
use crate::rrule;
use crate::service::{
    CalDavDeleteObjectRequest, CalDavDiscovery, CalDavFreeBusyInterval, CalDavFreeBusyRequest,
    CalDavMultigetRequest, CalDavObject, CalDavPutObjectRequest, CalDavSyncCollectionRequest,
    CalDavSyncCollectionResponse, CalendarEventPatch, FileCopyMoveRequest, FileEntry,
    FileReadResponse, FileWriteRequest, InvoiceCreateRequest, InvoicePaymentRequest, ProjectPatch,
    RemoteDeckBoard, RemoteDeckStack, SyncStats,
    TimeEntryContext, TimeEntryFilter, TimeEntryPatch, TimeLogRequest, TimeStartRequest,
    TimedTaskEntry, VaultError,
};
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

#[derive(Debug, Default)]
struct RemoteMergeResult {
    blocked_push_keys: std::collections::HashSet<String>,
}

#[derive(Debug, Clone)]
struct NextcloudRuntimeConfig {
    url: String,
    username: String,
    password: String,
    projects_path: String,
    calendar: String,
    event_calendar: Option<String>,
    deck_enabled: bool,
}

impl NextcloudRuntimeConfig {
    fn load() -> Result<Option<Self>, VaultError> {
        let file_cfg = load_nextcloud_config_file();
        let url = env_or_toml("NEXTCLOUD_URL", &file_cfg, "url");
        let username = env_or_toml("NEXTCLOUD_USER", &file_cfg, "username")
            .or_else(|| env_or_toml("NEXTCLOUD_USERNAME", &file_cfg, "username"))
            .unwrap_or_else(|| "agent".to_string());
        let password = std::env::var("NEXTCLOUD_PASSWORD")
            .ok()
            .or_else(|| read_secret_file_var("NEXTCLOUD_PASSWORD_FILE"))
            .or_else(|| env_or_toml("NEXTCLOUD_PASSWORD", &file_cfg, "password"))
            .or_else(|| {
                toml_string(&file_cfg, "password_file")
                    .and_then(|path| std::fs::read_to_string(path).ok())
                    .map(|s| s.trim().to_string())
            });

        let Some(url) = url else {
            return Ok(None);
        };
        let Some(password) = password.filter(|p| !p.is_empty()) else {
            return Ok(None);
        };

        let projects_path = env_or_toml("NEXTCLOUD_PROJECTS_PATH", &file_cfg, "projects_path")
            .unwrap_or_else(|| "Projects/".to_string());
        let calendar = env_or_toml("NEXTCLOUD_CALENDAR", &file_cfg, "calendar")
            .unwrap_or_else(|| "tasks".to_string())
            .to_ascii_lowercase();
        let event_calendar = env_or_toml("NEXTCLOUD_EVENT_CALENDAR", &file_cfg, "event_calendar")
            .or_else(|| env_or_toml("NEXTCLOUD_EVENTS_CALENDAR", &file_cfg, "events_calendar"))
            .map(|s| s.to_ascii_lowercase());
        let deck_enabled = std::env::var("NEXTCLOUD_DECK_ENABLED")
            .ok()
            .map(|v| env_truthy(&v))
            .or_else(|| toml_bool(&file_cfg, "deck_enabled"))
            .unwrap_or(true);

        Ok(Some(Self {
            url,
            username,
            password,
            projects_path,
            calendar,
            event_calendar,
            deck_enabled,
        }))
    }
}

fn load_nextcloud_config_file() -> Option<toml::Value> {
    let path = std::env::var("TASK_NEXTCLOUD_CONFIG")
        .ok()
        .map(std::path::PathBuf::from)
        .or_else(|| {
            std::env::var("HOME")
                .ok()
                .map(|home| std::path::PathBuf::from(home).join(".config/task/nextcloud.toml"))
        })?;
    let content = std::fs::read_to_string(path).ok()?;
    content.parse::<toml::Value>().ok()
}

fn env_or_toml(env: &str, cfg: &Option<toml::Value>, key: &str) -> Option<String> {
    std::env::var(env)
        .ok()
        .filter(|s| !s.is_empty())
        .or_else(|| toml_string(cfg, key))
}

fn toml_string(cfg: &Option<toml::Value>, key: &str) -> Option<String> {
    cfg.as_ref()
        .and_then(|v| v.get("nextcloud"))
        .and_then(|v| v.get(key))
        .and_then(|v| v.as_str())
        .map(|s| s.to_string())
}

fn toml_bool(cfg: &Option<toml::Value>, key: &str) -> Option<bool> {
    cfg.as_ref()
        .and_then(|v| v.get("nextcloud"))
        .and_then(|v| v.get(key))
        .and_then(|v| v.as_bool())
}

fn read_secret_file_var(env: &str) -> Option<String> {
    let path = std::env::var(env).ok()?;
    std::fs::read_to_string(path)
        .ok()
        .map(|s| s.trim().to_string())
}

fn env_truthy(value: &str) -> bool {
    matches!(
        value.to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on"
    )
}

fn nextcloud_webdav_provider(config: &NextcloudRuntimeConfig) -> crate::provider::WebDavProvider {
    crate::provider::WebDavProvider::new(
        "nextcloud-files",
        "Nextcloud Files",
        crate::provider::WebDavConfig {
            url: format!(
                "{}/remote.php/dav/files/{}/",
                config.url.trim_end_matches('/'),
                config.username
            ),
            username: config.username.clone(),
            password: config.password.clone(),
            projects_path: config.projects_path.clone(),
        },
    )
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
    last_sync: Arc<std::sync::Mutex<Option<SyncStats>>>,
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
            last_sync: Arc::new(std::sync::Mutex::new(None)),
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
    pub async fn update_task(&self, task: Task) -> Result<Task, VaultError> {
        self.update_task_as(task, None).await
    }

    /// Update a task and emit audit rows for every changed scalar field.
    /// The `actor` is stamped on each change row so the activity feed can
    /// show who made the edit.
    pub async fn update_task_as(
        &self,
        mut task: Task,
        actor: Option<&str>,
    ) -> Result<Task, VaultError> {
        // Snapshot the prior state for diffing. Lookup prefers id; if the
        // task has none, fall back to title match.
        let vault = self.vault.read().await;
        let prior = if let Some(id) = task.id.as_deref() {
            vault.load_tasks().into_iter().find(|t| t.id.as_deref() == Some(id))
        } else {
            vault.load_tasks().into_iter().find(|t| t.title == task.title)
        };

        task.date_modified = Some(Utc::now());
        vault.save_task(&task)?;

        let file_path = format!("{}.md", task.title);
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.index_task(&task, &file_path);
                if let Some(ref old) = prior {
                    record_task_diff(index, old, &task, actor, &file_path);
                }
            }
        }
        Ok(task)
    }

    // r[impl api.service.complete-task]
    pub async fn complete_task(&self, title: String) -> Result<Task, VaultError> {
        self.complete_task_as(title, None).await
    }

    /// Complete a task and stamp the audit row with the acting user.
    pub async fn complete_task_as(
        &self,
        title: String,
        actor: Option<&str>,
    ) -> Result<Task, VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let mut task = tasks
            .into_iter()
            .find(|t| t.title == title)
            .ok_or_else(|| VaultError::NotFound(title.clone()))?;

        let today = chrono::Local::now().date_naive();
        let prior_status = format!("{:?}", task.status);
        task.date_modified = Some(Utc::now());

        if task.recurrence.is_some() {
            // r[impl task.recurrence.instances]
            let today_str = today.format("%Y-%m-%d").to_string();
            if !task.completed_instances.contains(&today_str) {
                task.completed_instances.push(today_str);
            }
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
        let new_status = format!("{:?}", task.status);
        let file_path = format!("{}.md", task.title);
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.index_task(&task, &file_path);
                let _ = index.record_change(
                    "task",
                    &task.title,
                    Some("status"),
                    Some(&prior_status),
                    Some(&new_status),
                    actor,
                    Some(&file_path),
                );
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
        self.delete_task_as(title, None).await
    }

    /// Delete a task and emit an audit row tagged with the acting user.
    pub async fn delete_task_as(
        &self,
        title: String,
        actor: Option<&str>,
    ) -> Result<(), VaultError> {
        let path = self.root.join(format!("{}.md", title));
        if path.exists() {
            std::fs::remove_file(&path).map_err(|e| VaultError::IoError(e.to_string()))?;
            if let Ok(guard) = self.index.lock() {
                if let Some(ref index) = *guard {
                    let _ = index.record_change(
                        "task",
                        &title,
                        Some("deleted"),
                        Some("present"),
                        Some("deleted"),
                        actor,
                        Some(&format!("{}.md", title)),
                    );
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

    pub async fn trigger_sync(&self) -> Result<SyncStats, VaultError> {
        let mut stats = SyncStats {
            timestamp: Utc::now().to_rfc3339(),
            ..Default::default()
        };

        let Some(config) = NextcloudRuntimeConfig::load()? else {
            stats.errors.push(
                "Nextcloud sync is not configured; set NEXTCLOUD_URL, NEXTCLOUD_USER, and NEXTCLOUD_PASSWORD or ~/.config/task/nextcloud.toml".to_string(),
            );
            *self.last_sync.lock().unwrap() = Some(stats.clone());
            return Ok(stats);
        };

        let sync = crate::provider::nextcloud_sync::NextcloudSync::new(
            &config.url,
            &config.username,
            &config.password,
        );

        let local_tasks = self.list_tasks().await;
        let mut blocked_calendar_push = std::collections::HashSet::new();

        match sync.pull_tasks_from_calendar(&config.calendar).await {
            Ok(remote_tasks) => {
                stats.calendar_pulled = remote_tasks.len() as u32;
                let result = self
                    .merge_remote_tasks("caldav", remote_tasks, &local_tasks, &mut stats)
                    .await?;
                blocked_calendar_push.extend(result.blocked_push_keys);
            }
            Err(e) => stats.errors.push(format!("CalDAV pull: {e}")),
        }

        if let Some(event_calendar) = config.event_calendar.as_deref() {
            match sync.pull_events_from_calendar(event_calendar).await {
                Ok(remote_events) => {
                    self.merge_remote_events(remote_events, &mut stats).await?;
                }
                Err(e) => stats.errors.push(format!("CalDAV event pull: {e}")),
            }

            for event in self.list_calendar_events().await {
                match sync.push_event_to_calendar(event_calendar, &event).await {
                    Ok(()) => stats.calendar_pushed += 1,
                    Err(e) => stats
                        .errors
                        .push(format!("CalDAV event push '{}': {e}", event.title)),
                }
            }
        }

        for task in &local_tasks {
            if blocked_calendar_push.contains(&task_sync_key(task)) {
                continue;
            }
            match sync.push_task_to_calendar(&config.calendar, task).await {
                Ok(()) => stats.calendar_pushed += 1,
                Err(e) => stats
                    .errors
                    .push(format!("CalDAV push '{}': {e}", task.title)),
            }
        }

        if config.deck_enabled {
            match sync.list_boards().await {
                Ok(boards) => {
                    for board in boards.into_iter().filter(|b| !b.archived) {
                        for task in local_tasks
                            .iter()
                            .filter(|task| task.projects.iter().any(|p| p.0 == board.title))
                        {
                            match sync.push_task_to_deck(board.id, task, &task.body).await {
                                Ok(()) => stats.deck_pushed += 1,
                                Err(e) => stats.errors.push(format!(
                                    "Deck board {} push '{}': {e}",
                                    board.id, task.title
                                )),
                            }
                        }

                        match sync.deck_board_to_tasks(board.id).await {
                            Ok((project, remote_tasks)) => {
                                stats.deck_pulled += remote_tasks.len() as u32;
                                if let Err(e) = self.ensure_project_from_remote(&project).await {
                                    stats.errors.push(format!(
                                        "WebDAV/local project '{}': {e}",
                                        project.title
                                    ));
                                }
                                self.merge_remote_tasks(
                                    "deck",
                                    remote_tasks,
                                    &local_tasks,
                                    &mut stats,
                                )
                                .await?;
                            }
                            Err(e) => stats
                                .errors
                                .push(format!("Deck board {} pull: {e}", board.id)),
                        }
                    }
                }
                Err(e) => stats.errors.push(format!("Deck list boards: {e}")),
            }
        }

        match self.sync_webdav_projects(&config).await {
            Ok((created, updated)) => {
                stats.files_created += created;
                stats.files_updated += updated;
            }
            Err(e) => stats.errors.push(format!("WebDAV project sync: {e}")),
        }

        *self.last_sync.lock().unwrap() = Some(stats.clone());
        Ok(stats)
    }

    pub async fn sync_status(&self) -> Option<SyncStats> {
        self.last_sync.lock().unwrap().clone()
    }

    fn nextcloud_sync_from_config(
        config: &NextcloudRuntimeConfig,
    ) -> crate::provider::nextcloud_sync::NextcloudSync {
        crate::provider::nextcloud_sync::NextcloudSync::new(
            &config.url,
            &config.username,
            &config.password,
        )
    }

    pub async fn discover_caldav(&self) -> Result<CalDavDiscovery, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CalDAV is not configured".into()))?;
        Self::nextcloud_sync_from_config(&config)
            .discover_calendars()
            .await
    }

    pub async fn calendar_multiget(
        &self,
        request: CalDavMultigetRequest,
    ) -> Result<Vec<CalDavObject>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CalDAV is not configured".into()))?;
        let calendar = if request.calendar.is_empty() {
            config.calendar.as_str()
        } else {
            request.calendar.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .calendar_multiget(calendar, &request.hrefs)
            .await
    }

    pub async fn calendar_sync_collection(
        &self,
        request: CalDavSyncCollectionRequest,
    ) -> Result<CalDavSyncCollectionResponse, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CalDAV is not configured".into()))?;
        let calendar = if request.calendar.is_empty() {
            config.calendar.as_str()
        } else {
            request.calendar.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .sync_calendar_collection(calendar, request.sync_token.as_deref())
            .await
    }

    pub async fn put_calendar_object(
        &self,
        request: CalDavPutObjectRequest,
    ) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CalDAV is not configured".into()))?;
        let calendar = if request.calendar.is_empty() {
            config.calendar.as_str()
        } else {
            request.calendar.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .put_calendar_object(
                calendar,
                &request.href,
                &request.calendar_data,
                request.if_match.as_deref(),
                request.if_none_match.as_deref(),
            )
            .await
    }

    pub async fn delete_calendar_object(
        &self,
        request: CalDavDeleteObjectRequest,
    ) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CalDAV is not configured".into()))?;
        let calendar = if request.calendar.is_empty() {
            config.calendar.as_str()
        } else {
            request.calendar.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .delete_calendar_object(calendar, &request.href, request.if_match.as_deref())
            .await
    }

    pub async fn calendar_free_busy(
        &self,
        request: CalDavFreeBusyRequest,
    ) -> Result<Vec<CalDavFreeBusyInterval>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CalDAV is not configured".into()))?;
        let calendar = if request.calendar.is_empty() {
            config
                .event_calendar
                .as_deref()
                .unwrap_or(config.calendar.as_str())
        } else {
            request.calendar.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .calendar_free_busy(calendar, request.start, request.end)
            .await
    }

    pub async fn list_calendar_events(&self) -> Vec<crate::CalendarEvent> {
        self.vault.read().await.load_calendar_events()
    }

    pub async fn calendar_events_between(
        &self,
        from: chrono::DateTime<Utc>,
        to: chrono::DateTime<Utc>,
    ) -> Vec<crate::CalendarEvent> {
        self.list_calendar_events()
            .await
            .into_iter()
            .filter(|event| event_overlaps(event, from, to))
            .collect()
    }

    pub async fn create_calendar_event(
        &self,
        mut event: crate::CalendarEvent,
    ) -> Result<crate::CalendarEvent, VaultError> {
        if event.id.is_none() {
            event.id = Some(Uuid::new_v4().to_string());
        }
        let now = Utc::now();
        event.date_created.get_or_insert(now);
        event.date_modified = Some(now);
        self.vault.read().await.save_calendar_event(&event)?;
        Ok(event)
    }

    pub async fn update_calendar_event(
        &self,
        event_ref: &str,
        patch: CalendarEventPatch,
    ) -> Result<crate::CalendarEvent, VaultError> {
        let mut event = self
            .list_calendar_events()
            .await
            .into_iter()
            .find(|e| e.id.as_deref() == Some(event_ref) || e.title == event_ref)
            .ok_or_else(|| VaultError::NotFound(event_ref.to_string()))?;
        if let Some(title) = patch.title {
            event.title = title;
        }
        if let Some(description) = patch.description {
            event.description = description;
        }
        if let Some(location) = patch.location {
            event.location = location;
        }
        if let Some(start) = patch.start {
            event.start = start;
        }
        if let Some(end) = patch.end {
            event.end = end;
        }
        if let Some(all_day) = patch.all_day {
            event.all_day = all_day;
        }
        if let Some(status) = patch.status {
            event.status = status;
        }
        if let Some(recurrence) = patch.recurrence {
            event.recurrence = recurrence;
        }
        if let Some(attendees) = patch.attendees {
            event.attendees = attendees;
        }
        if let Some(body) = patch.body {
            event.body = body;
        }
        event.date_modified = Some(Utc::now());
        self.vault.read().await.save_calendar_event(&event)?;
        Ok(event)
    }

    pub async fn delete_calendar_event(&self, event_ref: &str) -> Result<(), VaultError> {
        self.vault.read().await.delete_calendar_event(event_ref)
    }

    pub async fn list_remote_deck_boards(&self) -> Result<Vec<RemoteDeckBoard>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud sync is not configured".into()))?;
        let sync = crate::provider::nextcloud_sync::NextcloudSync::new(
            &config.url,
            &config.username,
            &config.password,
        );
        Ok(sync
            .list_boards()
            .await?
            .into_iter()
            .map(|b| RemoteDeckBoard {
                id: b.id,
                title: b.title,
                archived: b.archived,
            })
            .collect())
    }

    pub async fn list_remote_deck_stacks(
        &self,
        board_id: u64,
    ) -> Result<Vec<RemoteDeckStack>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud sync is not configured".into()))?;
        let sync = crate::provider::nextcloud_sync::NextcloudSync::new(
            &config.url,
            &config.username,
            &config.password,
        );
        Ok(sync
            .list_stacks(board_id)
            .await?
            .into_iter()
            .map(|s| RemoteDeckStack {
                id: s.id,
                title: s.title,
                card_count: s.cards.len() as u32,
            })
            .collect())
    }

    async fn merge_remote_tasks(
        &self,
        source: &str,
        remote_tasks: Vec<Task>,
        local_tasks: &[Task],
        stats: &mut SyncStats,
    ) -> Result<RemoteMergeResult, VaultError> {
        let mut result = RemoteMergeResult::default();
        for remote in remote_tasks {
            if let Some(local) = find_matching_local_task(&remote, local_tasks) {
                let conflicts = task_sync_conflicts(local, &remote);
                if !conflicts.is_empty() {
                    result.blocked_push_keys.insert(task_sync_key(local));
                    for conflict in conflicts {
                        if let Err(e) = self
                            .record_conflict(
                                "task",
                                local
                                    .id
                                    .as_deref()
                                    .or(remote.id.as_deref())
                                    .unwrap_or(&local.title),
                                conflict.field,
                                conflict.local_value.as_deref(),
                                conflict.remote_value.as_deref(),
                                Some("local"),
                                Some(source),
                                None,
                                "sync",
                            )
                            .await
                        {
                            stats.errors.push(format!(
                                "{source} conflict log '{}': {e}",
                                local.title
                            ));
                        }
                    }
                    continue;
                }

                if remote_is_newer(local, &remote) {
                    self.vault.read().await.save_task(&remote)?;
                    stats.files_updated += 1;
                }
                continue;
            }
            self.vault.read().await.save_task(&remote)?;
            stats.files_created += 1;
        }
        Ok(result)
    }

    async fn merge_remote_events(
        &self,
        remote_events: Vec<crate::CalendarEvent>,
        stats: &mut SyncStats,
    ) -> Result<(), VaultError> {
        let local_events = self.list_calendar_events().await;
        for remote in remote_events {
            if let Some(local) = local_events.iter().find(|local| {
                (remote.id.is_some() && local.id == remote.id) || local.title == remote.title
            }) {
                if calendar_event_conflicts(local, &remote) {
                    let id = local
                        .id
                        .as_deref()
                        .or(remote.id.as_deref())
                        .unwrap_or(&local.title);
                    if let Err(e) = self
                        .record_conflict(
                            "calendar_event",
                            id,
                            "event",
                            Some(&format!("{:?}", local)),
                            Some(&format!("{:?}", remote)),
                            Some("local"),
                            Some("caldav"),
                            None,
                            "sync",
                        )
                        .await
                    {
                        stats
                            .errors
                            .push(format!("CalDAV event conflict log '{}': {e}", local.title));
                    }
                    continue;
                }
                if calendar_event_remote_is_newer(local, &remote) {
                    self.vault.read().await.save_calendar_event(&remote)?;
                    stats.files_updated += 1;
                }
                continue;
            }
            self.vault.read().await.save_calendar_event(&remote)?;
            stats.files_created += 1;
        }
        Ok(())
    }

    async fn ensure_project_from_remote(&self, project: &Project) -> Result<(), VaultError> {
        let exists = self
            .list_projects()
            .await
            .into_iter()
            .any(|p| p.title == project.title);
        if !exists {
            self.vault.read().await.save_project(project)?;
        }
        Ok(())
    }

    async fn sync_webdav_projects(
        &self,
        config: &NextcloudRuntimeConfig,
    ) -> Result<(u32, u32), VaultError> {
        let provider = nextcloud_webdav_provider(config);

        let bundles = crate::provider::ProjectProvider::list_all(&provider).await?;
        let mut created = 0;
        let mut updated = 0;
        for bundle in bundles {
            let project_path = self.root.join(&bundle.project.title).join("project.md");
            if project_path.exists() {
                updated += 1;
            } else {
                created += 1;
            }
            self.vault.read().await.save_project(&bundle.project)?;
            for task in bundle.tasks {
                self.vault.read().await.save_task(&task)?;
            }
        }
        Ok((created, updated))
    }

    // ── Invoicing ───────────────────────────────────────────────────────────

    /// Aggregate uninvoiced billable time entries for a client into a new
    /// markdown-backed Invoice. Saves the invoice to `invoices/` and flips
    /// `invoiced_at` on every TimeEntry consumed.
    ///
    /// - Entries with `invoiced_at: Some(_)` are skipped.
    /// - Running timers (`end_time: None`) are skipped.
    /// - Lines are grouped by (task_title, resolved_rate) so mixed-rate
    ///   work on one task appears as separate lines.
    /// - Due date defaults to `issue_date + client.payment_terms_days` or
    ///   `issue_date + 30` if terms are unset.
    pub async fn create_invoice_from_entries(
        &self,
        client_name: &str,
        from: Option<chrono::DateTime<Utc>>,
        to: Option<chrono::DateTime<Utc>>,
        fallback_rate: Option<u32>,
        tax_rate_percent: Option<f64>,
        discount_percent: Option<f64>,
        po_number: Option<String>,
        public_notes: Option<String>,
        actor: Option<&str>,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        let client = self
            .find_client(client_name)
            .await
            .ok_or_else(|| VaultError::NotFound(format!("client '{client_name}'")))?;

        let ctxs = self
            .list_time_entries(TimeEntryFilter {
                client: Some(client_name.to_string()),
                from,
                to,
                billable_only: true,
                ..Default::default()
            })
            .await;

        use std::collections::BTreeMap;
        let mut by_task_rate: BTreeMap<(String, u32), f64> = BTreeMap::new();
        let mut entry_ids = Vec::new();
        for ctx in ctxs {
            let e = &ctx.entry;
            if e.invoiced_at.is_some() || e.end_time.is_none() {
                continue;
            }
            let mins = e.duration_minutes() as u64;
            if mins == 0 {
                continue;
            }
            let rate = ctx.effective_rate(fallback_rate);
            *by_task_rate
                .entry((ctx.task_title.clone(), rate))
                .or_insert(0.0) += mins as f64 / 60.0;
            entry_ids.push(e.id.clone());
        }

        if by_task_rate.is_empty() {
            return Err(VaultError::NotFound(format!(
                "no uninvoiced billable entries for client '{client_name}' in range"
            )));
        }

        let line_items: Vec<crate::invoice::InvoiceLine> = by_task_rate
            .into_iter()
            .map(|((task, rate), hours)| crate::invoice::InvoiceLine {
                id: Uuid::new_v4().to_string(),
                task_title: task.clone(),
                description: task,
                hours,
                rate_cents: rate,
                tax_rate_percent: None,
                discount_percent: None,
            })
            .collect();

        let today = chrono::Local::now().date_naive();
        let terms = client.payment_terms_days.unwrap_or(30) as i64;
        let due_date = today + chrono::Duration::days(terms);
        let year = today.format("%Y").to_string().parse::<i32>().unwrap_or(2026);
        let number = self.next_invoice_number(year).await?;
        let id = crate::invoice::format_invoice_id(year, number);

        let now = Utc::now();
        let invoice = crate::invoice::Invoice {
            id: id.clone(),
            number,
            status: crate::invoice::InvoiceStatus::Draft,
            client: crate::task::WikiLink(client.name.clone()),
            issue_date: today,
            due_date,
            currency_code: if client.currency_code.is_empty() {
                "USD".to_string()
            } else {
                client.currency_code.clone()
            },
            line_items,
            tax_rate_percent,
            discount_percent,
            po_number,
            public_notes,
            private_notes: None,
            payments: Vec::new(),
            entry_ids: entry_ids.clone(),
            sent_at: None,
            paid_at: None,
            cancelled_at: None,
            cancelled_reason: None,
            created_by: actor.map(String::from),
            date_created: Some(now),
            date_modified: Some(now),
        };

        self.vault.read().await.save_invoice(&invoice)?;
        self.mark_entries_invoiced(&entry_ids, &id).await?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "invoice",
                    &id,
                    Some("status"),
                    None,
                    Some("Draft"),
                    actor,
                    Some(&format!("invoices/{id}.md")),
                );
            }
        }

        Ok(invoice)
    }

    /// Next invoice number for a given year. Scans existing invoices and
    /// returns max+1 so we don't need a separate counter file. Invoices
    /// are few per year for a single company — scanning is cheap.
    async fn next_invoice_number(&self, year: i32) -> Result<u32, VaultError> {
        let invoices = self.vault.read().await.load_invoices();
        let prefix = format!("INV-{year:04}-");
        let max = invoices
            .iter()
            .filter(|i| i.id.starts_with(&prefix))
            .map(|i| i.number)
            .max()
            .unwrap_or(0);
        Ok(max + 1)
    }

    pub async fn list_invoices(&self) -> Vec<crate::invoice::Invoice> {
        let mut invoices = self.vault.read().await.load_invoices();
        // Refresh derived status for display (non-destructive — the file
        // still has whatever we last wrote).
        let today = chrono::Local::now().date_naive();
        for inv in &mut invoices {
            inv.status = inv.derive_status(today);
        }
        invoices.sort_by(|a, b| b.number.cmp(&a.number));
        invoices
    }

    pub async fn get_invoice(&self, invoice_id: &str) -> Option<crate::invoice::Invoice> {
        self.vault
            .read()
            .await
            .load_invoices()
            .into_iter()
            .find(|i| i.id.eq_ignore_ascii_case(invoice_id))
    }

    /// Flip an invoice to Sent. Idempotent: re-sending a Sent invoice is a
    /// no-op.
    pub async fn send_invoice(
        &self,
        invoice_id: &str,
        actor: Option<&str>,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        let vault = self.vault.read().await;
        let mut invoice = vault
            .load_invoices()
            .into_iter()
            .find(|i| i.id.eq_ignore_ascii_case(invoice_id))
            .ok_or_else(|| VaultError::NotFound(invoice_id.to_string()))?;

        if matches!(
            invoice.status,
            crate::invoice::InvoiceStatus::Cancelled | crate::invoice::InvoiceStatus::Refunded
        ) {
            return Err(VaultError::IoError(format!(
                "cannot send {:?} invoice",
                invoice.status
            )));
        }

        let now = Utc::now();
        if invoice.sent_at.is_none() {
            invoice.sent_at = Some(now);
            invoice.status = crate::invoice::InvoiceStatus::Sent;
        }
        invoice.date_modified = Some(now);
        vault.save_invoice(&invoice)?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "invoice",
                    &invoice.id,
                    Some("status"),
                    Some("Draft"),
                    Some("Sent"),
                    actor,
                    Some(&format!("invoices/{}.md", invoice.id)),
                );
            }
        }
        Ok(invoice)
    }

    /// Convenience wrapper that builds a Payment and calls
    /// [`Self::add_invoice_payment`]. CLI-friendly.
    pub async fn record_invoice_payment(
        &self,
        invoice_id: &str,
        amount_cents: u64,
        method: Option<String>,
        reference: Option<String>,
        notes: Option<String>,
        actor: Option<&str>,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        let payment = crate::invoice::Payment {
            id: Uuid::new_v4().to_string(),
            amount_cents,
            received_at: Utc::now(),
            method: method.unwrap_or_default(),
            reference,
            recorded_by: actor.map(String::from),
            notes,
        };
        self.add_invoice_payment(invoice_id, payment, actor).await
    }

    /// Record a payment against an invoice. Status auto-derives
    /// (PartiallyPaid / Paid) based on totals.
    pub async fn add_invoice_payment(
        &self,
        invoice_id: &str,
        payment: crate::invoice::Payment,
        actor: Option<&str>,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        let vault = self.vault.read().await;
        let mut invoice = vault
            .load_invoices()
            .into_iter()
            .find(|i| i.id.eq_ignore_ascii_case(invoice_id))
            .ok_or_else(|| VaultError::NotFound(invoice_id.to_string()))?;

        let now = Utc::now();
        let amount = payment.amount_cents;
        invoice.payments.push(payment);
        invoice.date_modified = Some(now);

        let today = chrono::Local::now().date_naive();
        let new_status = invoice.derive_status(today);
        let old_status = format!("{:?}", invoice.status);
        invoice.status = new_status.clone();
        if matches!(new_status, crate::invoice::InvoiceStatus::Paid)
            && invoice.paid_at.is_none()
        {
            invoice.paid_at = Some(now);
        }
        vault.save_invoice(&invoice)?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "invoice",
                    &invoice.id,
                    Some("payment"),
                    Some(&old_status),
                    Some(&format!("+${:.2}", amount as f64 / 100.0)),
                    actor,
                    Some(&format!("invoices/{}.md", invoice.id)),
                );
            }
        }
        Ok(invoice)
    }

    /// Cancel an invoice — sets cancelled_at and flips status. Does NOT
    /// un-invoice the time entries (that's a separate call if you want
    /// to re-bill them) because cancellations usually mean "don't bill
    /// this at all" rather than "rebill later".
    pub async fn cancel_invoice(
        &self,
        invoice_id: &str,
        reason: Option<String>,
        actor: Option<&str>,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        let vault = self.vault.read().await;
        let mut invoice = vault
            .load_invoices()
            .into_iter()
            .find(|i| i.id.eq_ignore_ascii_case(invoice_id))
            .ok_or_else(|| VaultError::NotFound(invoice_id.to_string()))?;

        if matches!(invoice.status, crate::invoice::InvoiceStatus::Cancelled) {
            return Ok(invoice);
        }
        let now = Utc::now();
        let old_status = format!("{:?}", invoice.status);
        invoice.status = crate::invoice::InvoiceStatus::Cancelled;
        invoice.cancelled_at = Some(now);
        invoice.cancelled_reason = reason;
        invoice.date_modified = Some(now);
        vault.save_invoice(&invoice)?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "invoice",
                    &invoice.id,
                    Some("status"),
                    Some(&old_status),
                    Some("Cancelled"),
                    actor,
                    Some(&format!("invoices/{}.md", invoice.id)),
                );
            }
        }
        Ok(invoice)
    }

    /// Mark a set of time entries as invoiced against a specific invoice
    /// id. Idempotent: already-invoiced entries are left alone.
    pub async fn mark_entries_invoiced(
        &self,
        entry_ids: &[String],
        invoice_ninja_invoice_id: &str,
    ) -> Result<usize, VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let now = Utc::now();
        let wanted: std::collections::HashSet<&str> =
            entry_ids.iter().map(|s| s.as_str()).collect();
        let mut updated = 0usize;

        for mut t in tasks {
            let mut touched = false;
            for e in &mut t.time_entries {
                if wanted.contains(e.id.as_str()) && e.invoiced_at.is_none() {
                    e.invoiced_at = Some(now);
                    e.invoice_ninja_invoice_id = Some(invoice_ninja_invoice_id.to_string());
                    updated += 1;
                    touched = true;
                }
            }
            if touched {
                t.date_modified = Some(now);
                vault.save_task(&t)?;
            }
        }
        Ok(updated)
    }

    // ── Project editing ─────────────────────────────────────────────────────

    /// Find a project by case-insensitive title match.
    pub async fn find_project(&self, title: &str) -> Option<Project> {
        self.list_projects()
            .await
            .into_iter()
            .find(|p| p.title.eq_ignore_ascii_case(title))
    }

    /// Apply a patch to a project. Emits audit rows attributed to `actor`
    /// for every changed field. Returns the updated project.
    pub async fn update_project_as(
        &self,
        title: &str,
        patch: ProjectPatch,
        actor: Option<&str>,
    ) -> Result<Project, VaultError> {
        let vault = self.vault.read().await;
        let mut project = vault
            .load_projects()
            .into_iter()
            .find(|p| p.title.eq_ignore_ascii_case(title))
            .ok_or_else(|| VaultError::NotFound(title.to_string()))?;

        let mut changes: Vec<(&'static str, Option<String>, Option<String>)> = Vec::new();

        macro_rules! diff_opt_string {
            ($field:ident, $label:literal) => {
                if let Some(v) = patch.$field {
                    let before = project.$field.clone();
                    let new_val = if v.is_empty() || v == "clear" {
                        None
                    } else {
                        Some(v)
                    };
                    if before != new_val {
                        changes.push(($label, before.clone(), new_val.clone()));
                    }
                    project.$field = new_val;
                }
            };
        }

        diff_opt_string!(description, "description");
        diff_opt_string!(area, "area");
        diff_opt_string!(organization, "organization");
        diff_opt_string!(project_type, "type");
        diff_opt_string!(workflow, "workflow");
        diff_opt_string!(workflow_stage, "workflow_stage");
        diff_opt_string!(identifier, "identifier");
        diff_opt_string!(lead, "lead");
        diff_opt_string!(default_assignee, "default_assignee");
        diff_opt_string!(emoji, "emoji");
        diff_opt_string!(repo, "repo");
        diff_opt_string!(dev_path, "dev_path");

        if let Some(s) = patch.status {
            let before = format!("{:?}", project.status);
            let new_status = parse_project_status(&s).ok_or_else(|| {
                VaultError::ParseError(format!("unknown project status: {s}"))
            })?;
            if format!("{:?}", new_status) != before {
                changes.push((
                    "status",
                    Some(before.clone()),
                    Some(format!("{:?}", new_status)),
                ));
            }
            project.status = new_status;
        }
        if let Some(v) = patch.client {
            let before = project.client.as_ref().map(|w| w.0.clone());
            let new_val = if v.is_empty() || v == "clear" {
                None
            } else {
                Some(crate::task::WikiLink(v))
            };
            let new_str = new_val.as_ref().map(|w| w.0.clone());
            if before != new_str {
                changes.push(("client", before, new_str));
            }
            project.client = new_val;
        }
        if let Some(r) = patch.default_rate {
            let before = project.default_rate;
            let new_val = if r == 0 { None } else { Some(r) };
            if before != new_val {
                changes.push((
                    "default_rate",
                    before.map(|n| n.to_string()),
                    new_val.map(|n| n.to_string()),
                ));
            }
            project.default_rate = new_val;
        }
        if let Some(d) = patch.due {
            let before = project.due.map(|d| d.to_string());
            let new_val = if d.is_empty() || d == "clear" {
                None
            } else {
                Some(
                    d.parse::<chrono::NaiveDate>()
                        .map_err(|e| VaultError::ParseError(format!("invalid due: {e}")))?,
                )
            };
            let new_str = new_val.map(|d| d.to_string());
            if before != new_str {
                changes.push(("due", before, new_str));
            }
            project.due = new_val;
        }
        if let Some(d) = patch.start {
            let before = project.start.map(|d| d.to_string());
            let new_val = if d.is_empty() || d == "clear" {
                None
            } else {
                Some(
                    d.parse::<chrono::NaiveDate>()
                        .map_err(|e| VaultError::ParseError(format!("invalid start: {e}")))?,
                )
            };
            let new_str = new_val.map(|d| d.to_string());
            if before != new_str {
                changes.push(("start", before, new_str));
            }
            project.start = new_val;
        }

        // List fields — tags, email_tags, team.
        apply_list_edit(
            &mut project.tags,
            patch.add_tag,
            patch.remove_tag,
            "tags",
            &mut changes,
        );
        apply_list_edit(
            &mut project.email_tags,
            patch.add_email_tag,
            patch.remove_email_tag,
            "email_tags",
            &mut changes,
        );
        apply_list_edit(
            &mut project.team,
            patch.add_team,
            patch.remove_team,
            "team",
            &mut changes,
        );

        vault.save_project(&project)?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let file_path = format!("{}/project.md", project.title);
                for (field, old, new) in changes {
                    let _ = idx.record_change(
                        "project",
                        &project.title,
                        Some(field),
                        old.as_deref(),
                        new.as_deref(),
                        actor,
                        Some(&file_path),
                    );
                }
            }
        }
        Ok(project)
    }

    // ── Email linking ───────────────────────────────────────────────────────

    /// Attach an email to a task. De-duplicates by message_id (case-insensitive
    /// match on the bare form). Returns the updated task.
    pub async fn link_email_to_task(
        &self,
        task_ref: &str,
        email: crate::email::EmailRef,
        actor: Option<&str>,
    ) -> Result<crate::task::Task, VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let mut task = tasks
            .into_iter()
            .find(|t| t.id.as_deref() == Some(task_ref) || t.title.eq_ignore_ascii_case(task_ref))
            .ok_or_else(|| VaultError::NotFound(task_ref.to_string()))?;

        let bare = email.bare_message_id().to_lowercase();
        if !task
            .emails
            .iter()
            .any(|e| e.bare_message_id().to_lowercase() == bare)
        {
            task.emails.push(email);
        }
        task.date_modified = Some(Utc::now());
        vault.save_task(&task)?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let id = task.id.as_deref().unwrap_or(task.title.as_str());
                let _ = idx.record_change(
                    "task",
                    id,
                    Some("email:linked"),
                    None,
                    task.emails.last().map(|e| e.message_id.as_str()),
                    actor,
                    Some(&format!("{}.md", task.title)),
                );
            }
        }
        Ok(task)
    }

    /// Attach an email to a project. Matches by project title.
    pub async fn link_email_to_project(
        &self,
        project_title: &str,
        email: crate::email::EmailRef,
        actor: Option<&str>,
    ) -> Result<crate::project::Project, VaultError> {
        let vault = self.vault.read().await;
        let projects = vault.load_projects();
        let mut project = projects
            .into_iter()
            .find(|p| p.title.eq_ignore_ascii_case(project_title))
            .ok_or_else(|| VaultError::NotFound(project_title.to_string()))?;

        let bare = email.bare_message_id().to_lowercase();
        if !project
            .emails
            .iter()
            .any(|e| e.bare_message_id().to_lowercase() == bare)
        {
            project.emails.push(email);
        }
        vault.save_project(&project)?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "project",
                    &project.title,
                    Some("email:linked"),
                    None,
                    project.emails.last().map(|e| e.message_id.as_str()),
                    actor,
                    Some(&format!("{}.md", project.title)),
                );
            }
        }
        Ok(project)
    }

    /// Remove an email link from a task by message_id (case-insensitive
    /// bare-id match).
    pub async fn unlink_email_from_task(
        &self,
        task_ref: &str,
        message_id: &str,
        actor: Option<&str>,
    ) -> Result<crate::task::Task, VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let mut task = tasks
            .into_iter()
            .find(|t| t.id.as_deref() == Some(task_ref) || t.title.eq_ignore_ascii_case(task_ref))
            .ok_or_else(|| VaultError::NotFound(task_ref.to_string()))?;

        let target = strip_angle_brackets(message_id).to_lowercase();
        let before = task.emails.len();
        task.emails
            .retain(|e| e.bare_message_id().to_lowercase() != target);
        if task.emails.len() == before {
            return Err(VaultError::NotFound(format!(
                "no email with message-id {message_id} on '{task_ref}'"
            )));
        }
        task.date_modified = Some(Utc::now());
        vault.save_task(&task)?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let id = task.id.as_deref().unwrap_or(task.title.as_str());
                let _ = idx.record_change(
                    "task",
                    id,
                    Some("email:unlinked"),
                    Some(message_id),
                    None,
                    actor,
                    Some(&format!("{}.md", task.title)),
                );
            }
        }
        Ok(task)
    }

    /// Remove an email link from a project by message_id.
    pub async fn unlink_email_from_project(
        &self,
        project_title: &str,
        message_id: &str,
        actor: Option<&str>,
    ) -> Result<crate::project::Project, VaultError> {
        let vault = self.vault.read().await;
        let projects = vault.load_projects();
        let mut project = projects
            .into_iter()
            .find(|p| p.title.eq_ignore_ascii_case(project_title))
            .ok_or_else(|| VaultError::NotFound(project_title.to_string()))?;

        let target = strip_angle_brackets(message_id).to_lowercase();
        let before = project.emails.len();
        project
            .emails
            .retain(|e| e.bare_message_id().to_lowercase() != target);
        if project.emails.len() == before {
            return Err(VaultError::NotFound(format!(
                "no email with message-id {message_id} on project '{project_title}'"
            )));
        }
        vault.save_project(&project)?;

        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "project",
                    &project.title,
                    Some("email:unlinked"),
                    Some(message_id),
                    None,
                    actor,
                    Some(&format!("{}.md", project.title)),
                );
            }
        }
        Ok(project)
    }

    /// List emails linked to a task.
    pub async fn emails_for_task(&self, task_ref: &str) -> Option<Vec<crate::email::EmailRef>> {
        let tasks = self.vault.read().await.load_tasks();
        tasks
            .into_iter()
            .find(|t| t.id.as_deref() == Some(task_ref) || t.title.eq_ignore_ascii_case(task_ref))
            .map(|t| t.emails)
    }

    /// List emails linked to a project.
    pub async fn emails_for_project(
        &self,
        project_title: &str,
    ) -> Option<Vec<crate::email::EmailRef>> {
        let projects = self.vault.read().await.load_projects();
        projects
            .into_iter()
            .find(|p| p.title.eq_ignore_ascii_case(project_title))
            .map(|p| p.emails)
    }

    /// All RFC-2822 Message-Ids that are currently linked to any task or
    /// project. Used by the curator's inbox sweep to decide which
    /// messages still need triage. Normalized to lowercase and stripped
    /// of angle brackets.
    pub async fn linked_message_ids(&self) -> std::collections::HashSet<String> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let projects = vault.load_projects();
        drop(vault);
        let mut set = std::collections::HashSet::new();
        let norm = |s: &str| -> String {
            s.trim().trim_start_matches('<').trim_end_matches('>').to_ascii_lowercase()
        };
        for t in tasks {
            for e in t.emails {
                set.insert(norm(&e.message_id));
            }
        }
        for p in projects {
            for e in p.emails {
                set.insert(norm(&e.message_id));
            }
        }
        set
    }

    // ── Clients ─────────────────────────────────────────────────────────────

    /// List all clients from the vault's `clients/` directory.
    pub async fn list_clients(&self) -> Vec<crate::client::Client> {
        self.vault.read().await.load_clients()
    }

    /// Create or update a client note.
    pub async fn save_client(
        &self,
        client: crate::client::Client,
    ) -> Result<crate::client::Client, VaultError> {
        self.vault.read().await.save_client(&client)?;
        Ok(client)
    }

    /// Find a client by case-insensitive name.
    pub async fn find_client(&self, name: &str) -> Option<crate::client::Client> {
        self.vault
            .read()
            .await
            .load_clients()
            .into_iter()
            .find(|c| c.name.eq_ignore_ascii_case(name))
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
            user: user.clone(),
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

        self.record_time_event(
            "time:started",
            &task,
            &entry,
            None,
            Some(&entry.id),
            user.as_deref(),
        );
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

        self.record_time_event(
            "time:stopped",
            &task,
            &stopped,
            None,
            Some(&format!("{} min", stopped.duration_minutes())),
            stopped.user.as_deref(),
        );
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
            user: user.clone(),
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

        self.record_time_event(
            "time:logged",
            &task,
            &entry,
            None,
            Some(&format!("{} min", entry.duration_minutes())),
            user.as_deref(),
        );
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

    /// List time entries across the vault, each attached to its owning task's
    /// title and projects. Pass filters to scope by user, task, project, or
    /// date range. Projects come from the task's frontmatter.
    ///
    /// Each returned context carries the cascade inputs (project_rate,
    /// client_rate) so callers can pick an effective rate without a second
    /// lookup.
    pub async fn list_time_entries(
        &self,
        filter: TimeEntryFilter,
    ) -> Vec<TimeEntryContext> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let projects = vault.load_projects();
        let clients = vault.load_clients();

        // Index projects by title (case-insensitive) for O(1) lookup.
        let project_by_title: std::collections::HashMap<String, &crate::project::Project> =
            projects
                .iter()
                .map(|p| (p.title.to_lowercase(), p))
                .collect();
        let client_by_name: std::collections::HashMap<String, &crate::client::Client> =
            clients.iter().map(|c| (c.name.to_lowercase(), c)).collect();

        let mut out = Vec::new();
        for t in &tasks {
            if let Some(ref r) = filter.task_ref {
                let matches =
                    t.id.as_deref() == Some(r.as_str()) || t.title.eq_ignore_ascii_case(r);
                if !matches {
                    continue;
                }
            }
            if let Some(ref p) = filter.project {
                if !t.projects.iter().any(|w| w.0.eq_ignore_ascii_case(p)) {
                    continue;
                }
            }
            let task_projects: Vec<String> =
                t.projects.iter().map(|w| w.0.clone()).collect();

            // Walk linked projects to find the first one carrying a rate and
            // the first one carrying a client. We intentionally pick the
            // first match — mixing multiple clients on one task is a data
            // error the user should fix upstream.
            let mut project_rate: Option<u32> = None;
            let mut client_name: Option<String> = None;
            let mut client_rate: Option<u32> = None;
            for pname in &task_projects {
                if let Some(p) = project_by_title.get(&pname.to_lowercase()) {
                    if project_rate.is_none() {
                        project_rate = p.default_rate;
                    }
                    if client_name.is_none() {
                        if let Some(cref) = &p.client {
                            // WikiLinks round-trip as "[[Name]]" in YAML —
                            // strip the brackets before the lookup.
                            let raw = strip_wikilink_brackets(&cref.0);
                            if let Some(c) = client_by_name.get(&raw.to_lowercase()) {
                                client_name = Some(c.name.clone());
                                client_rate = c.default_hourly_rate;
                            } else {
                                client_name = Some(raw);
                            }
                        }
                    }
                }
            }

            // Client-name filter: if requested, skip entries whose task
            // doesn't resolve to that client.
            if let Some(ref want) = filter.client {
                let matches = client_name
                    .as_deref()
                    .map(|n| n.eq_ignore_ascii_case(want))
                    .unwrap_or(false);
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
                if let Some(ref tag) = filter.tag {
                    if !e.tags.iter().any(|t| t.eq_ignore_ascii_case(tag)) {
                        continue;
                    }
                }
                out.push(TimeEntryContext {
                    task_title: t.title.clone(),
                    task_projects: task_projects.clone(),
                    client_name: client_name.clone(),
                    project_rate,
                    client_rate,
                    entry: e.clone(),
                });
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
        self.delete_time_entry_as(entry_id, None).await
    }

    /// Delete a time entry and stamp the audit row with the acting user.
    pub async fn delete_time_entry_as(
        &self,
        entry_id: &str,
        actor: Option<&str>,
    ) -> Result<(), VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        for mut t in tasks {
            if let Some(entry) = t.time_entries.iter().find(|e| e.id == entry_id).cloned() {
                t.time_entries.retain(|e| e.id != entry_id);
                t.date_modified = Some(Utc::now());
                vault.save_task(&t)?;

                self.record_time_event(
                    "time:deleted",
                    &t,
                    &entry,
                    Some(&format!("{} min", entry.duration_minutes())),
                    None,
                    actor,
                );
                return Ok(());
            }
        }
        Err(VaultError::NotFound(format!("time entry {entry_id}")))
    }

    /// Edit an existing time entry. Only fields present in `patch` are updated.
    /// Returns the (task_title, updated_entry) pair.
    pub async fn edit_time_entry(
        &self,
        entry_id: &str,
        patch: TimeEntryPatch,
        actor: Option<&str>,
    ) -> Result<(String, crate::task::TimeEntry), VaultError> {
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();

        for mut t in tasks {
            let Some(idx) = t.time_entries.iter().position(|e| e.id == entry_id) else {
                continue;
            };
            let before = t.time_entries[idx].clone();
            let entry = &mut t.time_entries[idx];

            if let Some(s) = patch.start_time {
                entry.start_time = s;
            }
            if let Some(end_opt) = patch.end_time {
                entry.end_time = end_opt;
            }
            if let Some(d) = patch.description {
                entry.description = if d.is_empty() { None } else { Some(d) };
            }
            if let Some(b) = patch.billable {
                entry.billable = b;
            }
            if let Some(r) = patch.billable_rate {
                entry.billable_rate = if r == 0 { None } else { Some(r) };
            }
            if let Some(u) = patch.user {
                entry.user = if u.is_empty() { None } else { Some(u) };
            }
            if let Some(tags) = patch.tags {
                entry.tags = tags;
            }

            // Sanity: end after start if both set.
            if let (Some(end), start) = (entry.end_time, entry.start_time) {
                if end <= start {
                    return Err(VaultError::ParseError(
                        "end must be after start".into(),
                    ));
                }
            }

            let after = entry.clone();
            t.date_modified = Some(Utc::now());
            let title = t.title.clone();
            vault.save_task(&t)?;

            self.record_time_event(
                "time:edited",
                &t,
                &after,
                Some(&format!("{} min", before.duration_minutes())),
                Some(&format!("{} min", after.duration_minutes())),
                actor,
            );
            return Ok((title, after));
        }

        Err(VaultError::NotFound(format!("time entry {entry_id}")))
    }

    /// Write a row to the audit log for a timer-related event. Uses the
    /// existing `changes` table so `task activity` surfaces timer events
    /// alongside other edits.
    fn record_time_event(
        &self,
        field: &str,
        task: &Task,
        entry: &crate::task::TimeEntry,
        old_value: Option<&str>,
        new_value: Option<&str>,
        actor: Option<&str>,
    ) {
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let id = task.id.as_deref().unwrap_or(task.title.as_str());
                // Include the entry id in the new_value so the audit row is
                // self-describing.
                let payload = match new_value {
                    Some(v) => format!("{v} (entry {})", entry.id),
                    None => format!("entry {}", entry.id),
                };
                let _ = index.record_change(
                    "task",
                    id,
                    Some(field),
                    old_value,
                    Some(&payload),
                    actor,
                    Some(&format!("{}.md", task.title)),
                );
            }
        }
    }
}

fn parse_project_status(s: &str) -> Option<crate::project::ProjectStatus> {
    match s.to_lowercase().replace('-', "").as_str() {
        "planning" => Some(crate::project::ProjectStatus::Planning),
        "active" => Some(crate::project::ProjectStatus::Active),
        "onhold" | "hold" => Some(crate::project::ProjectStatus::OnHold),
        "completed" | "done" => Some(crate::project::ProjectStatus::Completed),
        "archived" => Some(crate::project::ProjectStatus::Archived),
        _ => None,
    }
}

fn apply_list_edit(
    list: &mut Vec<String>,
    add: Vec<String>,
    remove: Vec<String>,
    field: &'static str,
    changes: &mut Vec<(&'static str, Option<String>, Option<String>)>,
) {
    if add.is_empty() && remove.is_empty() {
        return;
    }
    let before = list.join(",");
    for r in &remove {
        list.retain(|x| x != r);
    }
    for a in add {
        if !list.contains(&a) {
            list.push(a);
        }
    }
    let after = list.join(",");
    if before != after {
        changes.push((field, Some(before), Some(after)));
    }
}

/// Strip `<...>` angle brackets from a Message-Id if present.
fn strip_angle_brackets(s: &str) -> &str {
    s.strip_prefix('<')
        .and_then(|s| s.strip_suffix('>'))
        .unwrap_or(s)
}

/// Strip `[[...]]` if present, otherwise return the string unchanged.
fn strip_wikilink_brackets(s: &str) -> String {
    s.strip_prefix("[[")
        .and_then(|s| s.strip_suffix("]]"))
        .unwrap_or(s)
        .to_string()
}

// ── Vox service trait implementations ────────────────────────────────────────

impl crate::service::TaskService for VaultServiceImpl {
    async fn list_tasks(&self) -> Vec<Task> { self.list_tasks().await }
    async fn execute_query(&self, query: Query) -> Vec<Task> { self.execute_query(query).await }
    async fn urgency_score(&self, task: Task) -> i32 { self.urgency_score(task).await }
    async fn create_task(&self, task: Task) -> Result<Task, VaultError> { self.create_task(task).await }
    async fn update_task(&self, task: Task) -> Result<Task, VaultError> { self.update_task(task).await }
    async fn complete_task(&self, title: String) -> Result<Task, VaultError> { self.complete_task(title).await }
    async fn delete_task(&self, title: String) -> Result<(), VaultError> { self.delete_task(title).await }
    async fn search_tasks(&self, query: String) -> Vec<Task> { self.search_tasks(query).await }
    async fn tasks_for_user(&self, username: String) -> Vec<Task> { self.tasks_for_user(username).await }
}

impl crate::service::ProjectService for VaultServiceImpl {
    async fn list_projects(&self) -> Vec<Project> { self.list_projects().await }
    async fn update_project(
        &self,
        title: String,
        patch: ProjectPatch,
        actor: Option<String>,
    ) -> Result<Project, VaultError> {
        self.update_project_as(&title, patch, actor.as_deref()).await
    }
    async fn project_stats(&self, project_title: String) -> ProjectStats { self.project_stats(project_title).await }
    async fn next_task(&self, project_title: String) -> Option<Task> { self.next_task(project_title).await }
    async fn tasks_for_project(&self, project_title: String) -> Vec<Task> { self.tasks_for_project(project_title).await }
}

impl crate::service::TimeService for VaultServiceImpl {
    async fn start_timer(&self, request: TimeStartRequest) -> Result<crate::task::TimeEntry, VaultError> {
        VaultServiceImpl::start_timer(
            self,
            &request.task_ref,
            request.description,
            request.billable,
            request.billable_rate,
            request.user,
        )
        .await
    }

    async fn stop_timer(&self, task_ref: Option<String>) -> Result<TimedTaskEntry, VaultError> {
        VaultServiceImpl::stop_timer(self, task_ref.as_deref())
            .await
            .map(|(task_title, entry)| TimedTaskEntry { task_title, entry })
    }

    async fn log_time(&self, request: TimeLogRequest) -> Result<crate::task::TimeEntry, VaultError> {
        VaultServiceImpl::log_time(
            self,
            &request.task_ref,
            request.start,
            request.end,
            request.description,
            request.billable,
            request.billable_rate,
            request.user,
        )
        .await
    }

    async fn active_timer(&self) -> Option<TimedTaskEntry> {
        VaultServiceImpl::active_timer(self)
            .await
            .map(|(task_title, entry)| TimedTaskEntry { task_title, entry })
    }

    async fn list_time_entries(&self, filter: TimeEntryFilter) -> Vec<TimeEntryContext> {
        VaultServiceImpl::list_time_entries(self, filter).await
    }

    async fn edit_time_entry(
        &self,
        entry_id: String,
        patch: TimeEntryPatch,
        actor: Option<String>,
    ) -> Result<TimedTaskEntry, VaultError> {
        VaultServiceImpl::edit_time_entry(self, &entry_id, patch, actor.as_deref())
            .await
            .map(|(task_title, entry)| TimedTaskEntry { task_title, entry })
    }

    async fn delete_time_entry(&self, entry_id: String, actor: Option<String>) -> Result<(), VaultError> {
        VaultServiceImpl::delete_time_entry_as(self, &entry_id, actor.as_deref()).await
    }
}

impl crate::service::ClientService for VaultServiceImpl {
    async fn list_clients(&self) -> Vec<crate::client::Client> {
        VaultServiceImpl::list_clients(self).await
    }

    async fn save_client(
        &self,
        client: crate::client::Client,
    ) -> Result<crate::client::Client, VaultError> {
        VaultServiceImpl::save_client(self, client).await
    }

    async fn find_client(&self, name: String) -> Option<crate::client::Client> {
        VaultServiceImpl::find_client(self, &name).await
    }
}

impl crate::service::InvoiceService for VaultServiceImpl {
    async fn create_invoice_from_entries(
        &self,
        request: InvoiceCreateRequest,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        VaultServiceImpl::create_invoice_from_entries(
            self,
            &request.client_name,
            request.from,
            request.to,
            request.fallback_rate,
            request.tax_rate_percent,
            request.discount_percent,
            request.po_number,
            request.public_notes,
            request.actor.as_deref(),
        )
        .await
    }

    async fn list_invoices(&self) -> Vec<crate::invoice::Invoice> {
        VaultServiceImpl::list_invoices(self).await
    }

    async fn get_invoice(&self, invoice_id: String) -> Option<crate::invoice::Invoice> {
        VaultServiceImpl::get_invoice(self, &invoice_id).await
    }

    async fn send_invoice(
        &self,
        invoice_id: String,
        actor: Option<String>,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        VaultServiceImpl::send_invoice(self, &invoice_id, actor.as_deref()).await
    }

    async fn record_invoice_payment(
        &self,
        request: InvoicePaymentRequest,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        VaultServiceImpl::record_invoice_payment(
            self,
            &request.invoice_id,
            request.amount_cents,
            request.method,
            request.reference,
            request.notes,
            request.actor.as_deref(),
        )
        .await
    }

    async fn cancel_invoice(
        &self,
        invoice_id: String,
        reason: Option<String>,
        actor: Option<String>,
    ) -> Result<crate::invoice::Invoice, VaultError> {
        VaultServiceImpl::cancel_invoice(self, &invoice_id, reason, actor.as_deref()).await
    }
}

impl crate::service::ActivityService for VaultServiceImpl {
    async fn recent_activity(&self, limit: u32) -> Result<Vec<crate::index::ChangeRow>, VaultError> {
        VaultServiceImpl::recent_activity(self, limit).await
    }

    async fn list_conflicts(
        &self,
        open_only: bool,
        limit: u32,
    ) -> Result<Vec<crate::index::ConflictRow>, VaultError> {
        VaultServiceImpl::list_conflicts(self, open_only, limit).await
    }

    async fn resolve_conflict(
        &self,
        conflict_id: i64,
        resolver: Option<String>,
        how: String,
    ) -> Result<(), VaultError> {
        VaultServiceImpl::resolve_conflict(self, conflict_id, resolver.as_deref(), &how).await
    }
}

impl crate::service::CalendarService for VaultServiceImpl {
    async fn tasks_due_by(&self, date: String) -> Vec<Task> { self.tasks_due_by(date).await }

    async fn scheduled_between(&self, from: String, to: String) -> Result<Vec<Task>, VaultError> {
        let from = chrono::NaiveDate::parse_from_str(&from, "%Y-%m-%d")
            .map_err(|e| VaultError::ParseError(e.to_string()))?;
        let to = chrono::NaiveDate::parse_from_str(&to, "%Y-%m-%d")
            .map_err(|e| VaultError::ParseError(e.to_string()))?;
        Ok(self
            .list_tasks()
            .await
            .into_iter()
            .filter(|task| task.scheduled.map_or(false, |d| d >= from && d <= to))
            .collect())
    }

    async fn events_between(
        &self,
        from: String,
        to: String,
    ) -> Result<Vec<crate::CalendarEvent>, VaultError> {
        let from = chrono::DateTime::parse_from_rfc3339(&from)
            .map_err(|e| VaultError::ParseError(e.to_string()))?
            .to_utc();
        let to = chrono::DateTime::parse_from_rfc3339(&to)
            .map_err(|e| VaultError::ParseError(e.to_string()))?
            .to_utc();
        Ok(self.calendar_events_between(from, to).await)
    }

    async fn create_event(
        &self,
        event: crate::CalendarEvent,
    ) -> Result<crate::CalendarEvent, VaultError> {
        self.create_calendar_event(event).await
    }

    async fn update_event(
        &self,
        event_ref: String,
        patch: CalendarEventPatch,
    ) -> Result<crate::CalendarEvent, VaultError> {
        self.update_calendar_event(&event_ref, patch).await
    }

    async fn delete_event(&self, event_ref: String) -> Result<(), VaultError> {
        self.delete_calendar_event(&event_ref).await
    }

    async fn trigger_sync(&self) -> Result<SyncStats, VaultError> { self.trigger_sync().await }
    async fn sync_status(&self) -> Option<SyncStats> { self.sync_status().await }
    async fn discover_caldav(&self) -> Result<CalDavDiscovery, VaultError> {
        VaultServiceImpl::discover_caldav(self).await
    }
    async fn calendar_multiget(
        &self,
        request: CalDavMultigetRequest,
    ) -> Result<Vec<CalDavObject>, VaultError> {
        VaultServiceImpl::calendar_multiget(self, request).await
    }
    async fn calendar_sync_collection(
        &self,
        request: CalDavSyncCollectionRequest,
    ) -> Result<CalDavSyncCollectionResponse, VaultError> {
        VaultServiceImpl::calendar_sync_collection(self, request).await
    }
    async fn put_calendar_object(&self, request: CalDavPutObjectRequest) -> Result<(), VaultError> {
        VaultServiceImpl::put_calendar_object(self, request).await
    }
    async fn delete_calendar_object(&self, request: CalDavDeleteObjectRequest) -> Result<(), VaultError> {
        VaultServiceImpl::delete_calendar_object(self, request).await
    }
    async fn calendar_free_busy(
        &self,
        request: CalDavFreeBusyRequest,
    ) -> Result<Vec<CalDavFreeBusyInterval>, VaultError> {
        VaultServiceImpl::calendar_free_busy(self, request).await
    }
    async fn list_deck_boards(&self) -> Result<Vec<RemoteDeckBoard>, VaultError> {
        self.list_remote_deck_boards().await
    }
    async fn list_deck_stacks(&self, board_id: u64) -> Result<Vec<RemoteDeckStack>, VaultError> {
        self.list_remote_deck_stacks(board_id).await
    }
}

impl crate::service::FileService for VaultServiceImpl {
    async fn list_files(&self, path: String, depth: String) -> Result<Vec<FileEntry>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud file provider is not configured".into()))?;
        let provider = nextcloud_webdav_provider(&config);
        provider
            .list(&path, if depth.is_empty() { "1" } else { &depth })
            .await
            .map(|entries| entries.into_iter().map(file_entry_from_webdav).collect())
    }

    async fn stat_file(&self, path: String) -> Result<Option<FileEntry>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud file provider is not configured".into()))?;
        let provider = nextcloud_webdav_provider(&config);
        provider
            .stat(&path)
            .await
            .map(|entry| entry.map(file_entry_from_webdav))
    }

    async fn read_file(&self, path: String) -> Result<Option<FileReadResponse>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud file provider is not configured".into()))?;
        let provider = nextcloud_webdav_provider(&config);
        let stat = provider.stat(&path).await?;
        let Some(content) = provider.read(&path).await? else {
            return Ok(None);
        };
        Ok(Some(FileReadResponse {
            content_base64: BASE64.encode(content),
            content_type: stat.as_ref().and_then(|entry| entry.content_type.clone()),
            etag: stat.and_then(|entry| entry.etag),
        }))
    }

    async fn write_file(&self, request: FileWriteRequest) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud file provider is not configured".into()))?;
        let provider = nextcloud_webdav_provider(&config);
        let content = BASE64
            .decode(request.content_base64)
            .map_err(|e| VaultError::ParseError(format!("invalid base64 content: {e}")))?;
        provider
            .write(
                &request.path,
                content,
                crate::provider::WebDavPutOptions {
                    content_type: request.content_type,
                    if_match: request.if_match,
                    if_none_match: request.if_none_match,
                },
            )
            .await
    }

    async fn create_dir(&self, path: String) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud file provider is not configured".into()))?;
        nextcloud_webdav_provider(&config).create_dir(&path).await
    }

    async fn delete_file(&self, path: String) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud file provider is not configured".into()))?;
        nextcloud_webdav_provider(&config).remove(&path).await
    }

    async fn copy_file(&self, request: FileCopyMoveRequest) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud file provider is not configured".into()))?;
        nextcloud_webdav_provider(&config)
            .copy(&request.from, &request.to, request.overwrite, Some("infinity"))
            .await
    }

    async fn move_file(&self, request: FileCopyMoveRequest) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud file provider is not configured".into()))?;
        nextcloud_webdav_provider(&config)
            .move_resource(&request.from, &request.to, request.overwrite)
            .await
    }
}

fn file_entry_from_webdav(entry: crate::provider::WebDavEntry) -> FileEntry {
    FileEntry {
        path: entry.path,
        name: entry.name,
        kind: match entry.kind {
            crate::provider::WebDavResourceKind::File => "file".to_string(),
            crate::provider::WebDavResourceKind::Collection => "directory".to_string(),
        },
        content_type: entry.content_type,
        content_length: entry.content_length,
        etag: entry.etag,
        last_modified: entry.last_modified,
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TaskSyncConflict {
    field: &'static str,
    local_value: Option<String>,
    remote_value: Option<String>,
}

fn task_sync_key(task: &Task) -> String {
    task.id
        .as_deref()
        .map(|id| format!("id:{id}"))
        .unwrap_or_else(|| format!("title:{}", task.title))
}

fn find_matching_local_task<'a>(remote: &Task, local_tasks: &'a [Task]) -> Option<&'a Task> {
    local_tasks.iter().find(|local| {
        (remote.id.is_some() && local.id == remote.id) || local.title == remote.title
    })
}

fn remote_is_newer(local: &Task, remote: &Task) -> bool {
    matches!(
        (local.date_modified, remote.date_modified),
        (Some(local_modified), Some(remote_modified)) if remote_modified > local_modified
    )
}

fn both_sides_modified(local: &Task, remote: &Task) -> bool {
    match (local.date_modified, remote.date_modified) {
        (Some(local_modified), Some(remote_modified)) => local_modified != remote_modified,
        _ => false,
    }
}

fn task_sync_conflicts(local: &Task, remote: &Task) -> Vec<TaskSyncConflict> {
    if !both_sides_modified(local, remote) {
        return Vec::new();
    }

    let mut conflicts = Vec::new();
    push_conflict(
        &mut conflicts,
        "title",
        Some(local.title.clone()),
        Some(remote.title.clone()),
    );
    push_conflict(
        &mut conflicts,
        "status",
        Some(format!("{:?}", local.status)),
        Some(format!("{:?}", remote.status)),
    );
    push_conflict(
        &mut conflicts,
        "priority",
        Some(format!("{:?}", local.priority)),
        Some(format!("{:?}", remote.priority)),
    );
    push_conflict(&mut conflicts, "due", fmt_date(local.due), fmt_date(remote.due));
    push_conflict(
        &mut conflicts,
        "scheduled",
        fmt_date(local.scheduled),
        fmt_date(remote.scheduled),
    );
    push_conflict(
        &mut conflicts,
        "assignee",
        local.assignee.clone(),
        remote.assignee.clone(),
    );
    push_conflict(
        &mut conflicts,
        "tags",
        Some(local.tags.join(",")),
        Some(remote.tags.join(",")),
    );
    push_conflict(
        &mut conflicts,
        "projects",
        Some(local.projects.iter().map(|p| p.0.as_str()).collect::<Vec<_>>().join(",")),
        Some(remote.projects.iter().map(|p| p.0.as_str()).collect::<Vec<_>>().join(",")),
    );
    push_conflict(
        &mut conflicts,
        "body",
        Some(local.body.clone()),
        Some(remote.body.clone()),
    );
    conflicts
}

fn push_conflict(
    conflicts: &mut Vec<TaskSyncConflict>,
    field: &'static str,
    local_value: Option<String>,
    remote_value: Option<String>,
) {
    if local_value != remote_value {
        conflicts.push(TaskSyncConflict {
            field,
            local_value,
            remote_value,
        });
    }
}

fn fmt_date(date: Option<NaiveDate>) -> Option<String> {
    date.map(|d| d.to_string())
}

fn event_overlaps(
    event: &crate::CalendarEvent,
    from: chrono::DateTime<Utc>,
    to: chrono::DateTime<Utc>,
) -> bool {
    let end = event.end.unwrap_or(event.start);
    event.start <= to && end >= from
}

fn calendar_event_remote_is_newer(
    local: &crate::CalendarEvent,
    remote: &crate::CalendarEvent,
) -> bool {
    matches!(
        (local.date_modified, remote.date_modified),
        (Some(local_modified), Some(remote_modified)) if remote_modified > local_modified
    )
}

fn calendar_event_conflicts(local: &crate::CalendarEvent, remote: &crate::CalendarEvent) -> bool {
    match (local.date_modified, remote.date_modified) {
        (Some(local_modified), Some(remote_modified)) if local_modified != remote_modified => {
            local.title != remote.title
                || local.description != remote.description
                || local.location != remote.location
                || local.start != remote.start
                || local.end != remote.end
                || local.status != remote.status
                || local.recurrence != remote.recurrence
                || local.attendees != remote.attendees
        }
        _ => false,
    }
}

/// Diff two versions of a task and write one audit row per changed scalar
/// field. Lists (tags/projects/contexts) emit one row tagged with the diff
/// summary rather than per-element rows — noisy enough as one entry.
fn record_task_diff(
    index: &crate::index::TaskIndex,
    old: &Task,
    new: &Task,
    actor: Option<&str>,
    file_path: &str,
) {
    let id = new.id.as_deref().or(Some(new.title.as_str())).unwrap();

    // Scalar fields worth surfacing in the activity feed.
    let mut rows: Vec<(&str, Option<String>, Option<String>)> = Vec::new();
    if old.title != new.title {
        rows.push(("title", Some(old.title.clone()), Some(new.title.clone())));
    }
    if old.status != new.status {
        rows.push((
            "status",
            Some(format!("{:?}", old.status)),
            Some(format!("{:?}", new.status)),
        ));
    }
    if old.priority != new.priority {
        rows.push((
            "priority",
            Some(format!("{:?}", old.priority)),
            Some(format!("{:?}", new.priority)),
        ));
    }
    if old.assignee != new.assignee {
        rows.push(("assignee", old.assignee.clone(), new.assignee.clone()));
    }
    if old.due != new.due {
        rows.push((
            "due",
            old.due.map(|d| d.to_string()),
            new.due.map(|d| d.to_string()),
        ));
    }
    if old.scheduled != new.scheduled {
        rows.push((
            "scheduled",
            old.scheduled.map(|d| d.to_string()),
            new.scheduled.map(|d| d.to_string()),
        ));
    }
    if old.recurrence != new.recurrence {
        rows.push(("recurrence", old.recurrence.clone(), new.recurrence.clone()));
    }
    if old.deleted_at.is_some() != new.deleted_at.is_some() {
        rows.push((
            "deleted_at",
            old.deleted_at.map(|d| d.to_rfc3339()),
            new.deleted_at.map(|d| d.to_rfc3339()),
        ));
    }

    // List fields as one summary row each, naming the net diff.
    if old.tags != new.tags {
        rows.push((
            "tags",
            Some(old.tags.join(",")),
            Some(new.tags.join(",")),
        ));
    }
    if old.projects != new.projects {
        rows.push((
            "projects",
            Some(old.projects.iter().map(|p| p.0.as_str()).collect::<Vec<_>>().join(",")),
            Some(new.projects.iter().map(|p| p.0.as_str()).collect::<Vec<_>>().join(",")),
        ));
    }
    if old.contexts != new.contexts {
        rows.push((
            "contexts",
            Some(old.contexts.join(",")),
            Some(new.contexts.join(",")),
        ));
    }

    for (field, from, to) in rows {
        let _ = index.record_change(
            "task",
            id,
            Some(field),
            from.as_deref(),
            to.as_deref(),
            actor,
            Some(file_path),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::task::{Priority, WikiLink};

    fn modified_at(ts: &str) -> chrono::DateTime<Utc> {
        chrono::DateTime::parse_from_rfc3339(ts).unwrap().to_utc()
    }

    #[test]
    fn caldav_conflict_detection_blocks_same_field_overwrite() {
        let local = Task {
            id: Some("task-1".to_string()),
            title: "Shared task".to_string(),
            status: Status::InProgress,
            priority: Priority::Normal,
            date_modified: Some(modified_at("2026-04-29T10:00:00Z")),
            projects: vec![WikiLink("Personal".to_string())],
            ..Default::default()
        };
        let remote = Task {
            id: Some("task-1".to_string()),
            title: "Shared task".to_string(),
            status: Status::Done,
            priority: Priority::Normal,
            date_modified: Some(modified_at("2026-04-29T10:05:00Z")),
            projects: vec![WikiLink("Personal".to_string())],
            ..Default::default()
        };

        let conflicts = task_sync_conflicts(&local, &remote);
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].field, "status");
        assert_eq!(task_sync_key(&local), "id:task-1");
    }

    #[test]
    fn caldav_remote_newer_without_field_delta_is_not_a_conflict() {
        let local = Task {
            id: Some("task-2".to_string()),
            title: "Same task".to_string(),
            status: Status::Open,
            priority: Priority::High,
            date_modified: Some(modified_at("2026-04-29T10:00:00Z")),
            ..Default::default()
        };
        let remote = Task {
            date_modified: Some(modified_at("2026-04-29T11:00:00Z")),
            ..local.clone()
        };

        assert!(remote_is_newer(&local, &remote));
        assert!(task_sync_conflicts(&local, &remote).is_empty());
    }

    #[test]
    fn calendar_event_overlap_and_conflict_detection() {
        let start = modified_at("2026-05-01T10:00:00Z");
        let end = modified_at("2026-05-01T11:00:00Z");
        let local = crate::CalendarEvent {
            id: Some("event-1".to_string()),
            title: "Planning".to_string(),
            start,
            end: Some(end),
            location: Some("Room A".to_string()),
            date_modified: Some(modified_at("2026-04-29T10:00:00Z")),
            ..Default::default()
        };
        let remote = crate::CalendarEvent {
            location: Some("Room B".to_string()),
            date_modified: Some(modified_at("2026-04-29T10:05:00Z")),
            ..local.clone()
        };

        assert!(event_overlaps(
            &local,
            modified_at("2026-05-01T10:30:00Z"),
            modified_at("2026-05-01T12:00:00Z")
        ));
        assert!(calendar_event_remote_is_newer(&local, &remote));
        assert!(calendar_event_conflicts(&local, &remote));
    }
}
