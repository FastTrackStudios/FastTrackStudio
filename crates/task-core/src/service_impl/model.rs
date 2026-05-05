// r[impl api.service]
use std::collections::HashSet;
use std::path::Path;
use std::sync::Arc;

use base64::{Engine as _, engine::general_purpose::STANDARD as BASE64};
use chrono::{DateTime, Datelike, NaiveDate, Utc};
use crudcrate::{CrudStorage, InMemoryQuery};
use serde::Serialize;
use tokio::sync::RwLock;
use uuid::Uuid;

use crate::asset::{
    Asset, AssetConflict, AssetCreateRequest, AssetFilter, AssetMaintenanceRecord,
    AssetMaintenanceRequest, AssetPatch, AssetRepairRequest, AssetRepairResponse, AssetReport,
    AssetReservationRecord, AssetReservationResponse, AssetReserveRequest, AssetStatus,
    build_asset_report, collect_asset_conflicts, conflicts_for_reservation, format_asset_id,
    matches_asset_filter, parse_asset_status,
};
use crate::expense::{
    Expense, ExpenseCreateRequest, ExpenseFilter, ExpensePatch, ExpenseReport, ExpenseStatus,
    build_expense_report, format_expense_id, matches_expense_filter, parse_expense_status,
};
use crate::index::TaskIndex;
use crate::location::{Location, Space, VenueDefault};
use crate::people::{
    CommunicationRef, ContactMethod, OrganizationContext, OrganizationRecord, Person,
    PersonContext, ProviderConflict, ProviderConflictField, ProviderRef,
};
use crate::project::{
    Project, ProjectDashboardEntry, ProjectStats, next_task as find_next_task,
    project_dashboard as build_project_dashboard,
};
use crate::provider::{
    ChannelConversation, ChannelMessage, ChannelSendMessageRequest, CommunicationChannelProvider,
    TalkClient, TalkConfig,
};
use crate::query::Query;
use crate::revenue::{
    Revenue, RevenueCreateRequest, RevenueFilter, RevenueReport, build_revenue_report,
    format_revenue_id, matches_revenue_filter,
};
use crate::rrule;
use crate::service::{
    BusinessFinanceClientSummary, BusinessFinanceReport, CalDavDeleteObjectRequest,
    CalDavDiscovery, CalDavFreeBusyInterval, CalDavFreeBusyRequest, CalDavMultigetRequest,
    CalDavObject, CalDavPutObjectRequest, CalDavScheduleRequest, CalDavScheduleResponse,
    CalDavSyncCollectionRequest, CalDavSyncCollectionResponse, CalendarEventPatch,
    CardDavDeleteObjectRequest, CardDavDiscovery, CardDavMultigetRequest, CardDavObject,
    CardDavPutObjectRequest, CardDavSyncCollectionRequest, CardDavSyncCollectionResponse,
    EmailLinkRequest, EmailLinkResponse, EmailListRequest, EmailUnlinkRequest, FileCopyMoveRequest,
    FileEntry, FileReadResponse, FileWriteRequest, InboxCaptureRequest, InboxItem,
    InboxPromoteRequest, InvoiceAgingBucket, InvoiceCreateRequest, InvoicePaymentRequest,
    MailCreateMailboxRequest, MailCreateTagRequest, MailDeleteTagRequest, MailListMessagesRequest,
    MailMessageTagRequest, MailMoveMessageRequest, NextcloudCapability, OperatingAreaStatus,
    OperatingGoal, OperatingModelReport, OperatingRoutine, ProjectFileSummary,
    ProjectKnowledgeContext, ProjectPatch, ProviderSyncState, RemoteDeckBoard, RemoteDeckStack,
    ReviewReport, SyncPlan, SyncPlanItem, SyncStats, SystemCapabilities, SystemHealth,
    TimeEntryContext, TimeEntryFilter, TimeEntryPatch, TimeLogRequest, TimeStartRequest,
    TimedTaskEntry, VaultCapability, VaultError,
};
use crate::task::{Priority, Status, Task, TaskApi, TaskApiCreate, TaskApiUpdate, WikiLink};
use crate::vault::{Vault, VaultStorage};
use crate::watch::{WatchHandle, start_watch};

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

fn nextcloud_talk_provider(config: &NextcloudRuntimeConfig) -> TalkClient {
    TalkClient::new(TalkConfig {
        url: config.url.clone(),
        username: config.username.clone(),
        password: config.password.clone(),
    })
}

fn nextcloud_mail_client() -> Result<crate::provider::MailClient, VaultError> {
    let config = NextcloudRuntimeConfig::load()?
        .ok_or_else(|| VaultError::IoError("Nextcloud Mail is not configured".into()))?;
    Ok(crate::provider::MailClient::new(
        crate::provider::MailConfig {
            url: config.url,
            username: config.username,
            password: config.password,
        },
    ))
}

fn health_check(
    name: &str,
    code: &str,
    configured: bool,
    ok: bool,
    detail: String,
    hint: Option<&str>,
) -> crate::service::HealthCheck {
    health_check_with_severity(
        name,
        code,
        configured,
        ok,
        if ok { "ok" } else { "error" },
        detail,
        hint,
    )
}

fn health_check_with_severity(
    name: &str,
    code: &str,
    configured: bool,
    ok: bool,
    severity: &str,
    detail: String,
    hint: Option<&str>,
) -> crate::service::HealthCheck {
    crate::service::HealthCheck {
        name: name.into(),
        code: code.into(),
        severity: severity.into(),
        ok,
        configured,
        detail,
        hint: hint.map(String::from),
    }
}

fn system_health(deep: bool, checks: Vec<crate::service::HealthCheck>) -> SystemHealth {
    let degraded = checks
        .iter()
        .any(|check| check.configured && !check.ok && check.severity == "warning");
    SystemHealth {
        ok: checks
            .iter()
            .all(|check| check.ok || !check.configured || check.severity == "warning"),
        degraded,
        deep,
        checks,
    }
}

#[derive(Clone)]
pub struct VaultServiceImpl {
    /// The primary vault root (personal or first-registered vault).
    root: std::path::PathBuf,
    vault: Arc<RwLock<Vault>>,
    task_storage: VaultStorage,
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
                        tracing::info!(
                            tasks = stats.tasks,
                            files = stats.files_scanned,
                            "Index rebuilt"
                        );
                    }
                    Some(idx)
                }
                Err(e) => {
                    tracing::warn!(error = %e, "Failed to open index, queries will scan files");
                    None
                }
            }
        };

        let vault = Arc::new(RwLock::new(Vault::new(&root)));
        let task_storage = VaultStorage::new(Arc::clone(&vault));

        Self {
            vault,
            task_storage,
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
        match CrudStorage::<TaskApi>::get_all(&self.task_storage, InMemoryQuery::all()).await {
            Ok(tasks) => dedupe_tasks(tasks.into_iter().filter_map(task_from_json_model).collect()),
            Err(_) => dedupe_tasks(self.vault.read().await.load_tasks()),
        }
    }

    // r[impl api.service.execute-query]
    pub async fn execute_query(&self, query: Query) -> Vec<Task> {
        let tasks = dedupe_tasks(self.vault.read().await.load_tasks());
        query.execute(&tasks).into_iter().cloned().collect()
    }

    pub async fn urgency_score(&self, task: Task) -> i32 {
        task.urgency_score()
    }

    // r[impl api.service.create-task]
    pub async fn create_task(&self, mut task: Task) -> Result<Task, VaultError> {
        let now = Utc::now();
        task.date_created = Some(now);
        task.date_modified = Some(now);
        let task = if task.id == Uuid::nil() {
            let create = task_to_create_model(&task)?;
            let created = CrudStorage::<TaskApi>::create(&self.task_storage, create)
                .await
                .map_err(api_error_to_vault)?;
            task_from_api(created)?
        } else {
            self.vault.read().await.save_task(&task)?;
            task
        };
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.index_task(&task, &format!("{}.md", task.title));
            }
        }
        Ok(task)
    }

    pub async fn capture_inbox(
        &self,
        request: InboxCaptureRequest,
    ) -> Result<InboxItem, VaultError> {
        let parsed = crate::capture::parse_capture(&request.text);
        let title = if parsed.title.trim().is_empty() {
            request.text.trim().to_string()
        } else {
            parsed.title.trim().to_string()
        };
        if title.is_empty() {
            return Err(VaultError::ParseError("capture text is empty".to_string()));
        }

        let kind = normalize_inbox_kind(request.kind.as_deref());
        let mut tags = parsed.tags;
        push_unique(&mut tags, "inbox".to_string());

        let task = Task {
            id: Uuid::new_v4(),
            title,
            status: Status::Open,
            priority: parsed.priority.unwrap_or(Priority::Normal),
            projects: parsed.projects.into(),
            contexts: parsed.contexts.into(),
            tags: tags.into(),
            due: parsed.due,
            issue_type: Some(kind),
            created_by: request.actor,
            external_source: request.source.map(|source| format!("inbox:{source}")),
            body: request.text,
            ..Default::default()
        };

        self.vault.read().await.save_task(&task)?;
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.index_task(&task, &format!("{}.md", task.title));
            }
        }
        Ok(inbox_item_from_task(&task))
    }

    pub async fn list_inbox_items(&self) -> Vec<InboxItem> {
        self.vault
            .read()
            .await
            .load_tasks()
            .into_iter()
            .filter(is_inbox_task)
            .map(|task| inbox_item_from_task(&task))
            .collect()
    }

    pub async fn daily_review_report(&self) -> ReviewReport {
        let tasks = self.vault.read().await.load_tasks();
        build_review_report(tasks, 7, 7)
    }

    pub async fn weekly_review_report(&self) -> ReviewReport {
        let tasks = self.vault.read().await.load_tasks();
        build_review_report(tasks, 30, 30)
    }

    pub async fn monthly_review_report(&self) -> ReviewReport {
        let tasks = self.vault.read().await.load_tasks();
        build_review_report(tasks, 90, 45)
    }

    pub async fn project_review_report(&self, project_title: String) -> ReviewReport {
        let tasks = self.tasks_for_project(project_title).await;
        build_review_report(tasks, 30, 21)
    }

    pub async fn operating_model_report(&self) -> OperatingModelReport {
        let tasks = self.vault.read().await.load_tasks();
        let projects = self.list_projects().await;
        let events = self.list_calendar_events().await;
        build_operating_model_report(tasks, projects, events)
    }

    pub async fn promote_inbox(
        &self,
        request: InboxPromoteRequest,
    ) -> Result<InboxItem, VaultError> {
        let vault = self.vault.read().await;
        let mut task = vault
            .load_tasks()
            .into_iter()
            .find(|task| task_matches_reference(task, &request.reference))
            .ok_or_else(|| VaultError::NotFound(request.reference.clone()))?;

        if let Some(kind) = request.kind {
            task.issue_type = Some(normalize_inbox_kind(Some(&kind)));
        }
        if let Some(project) = request.project {
            if !project.is_empty() && project != "clear" {
                push_unique(&mut task.projects, WikiLink(project));
            }
        }
        if let Some(status) = request.status {
            task.status = parse_task_status(&status)
                .ok_or_else(|| VaultError::ParseError(format!("unknown status: {status}")))?;
        }
        if let Some(assignee) = request.assignee {
            task.assignee = if assignee.is_empty() || assignee == "clear" {
                None
            } else {
                Some(assignee)
            };
        }
        if let Some(due) = request.due {
            task.due = parse_optional_naive_date(&due, "due")?;
        }
        if let Some(scheduled) = request.scheduled {
            task.scheduled = parse_optional_naive_date(&scheduled, "scheduled")?;
        }
        for tag in request.add_tags {
            push_unique(&mut task.tags, tag);
        }
        task.tags.retain(|tag| tag != "inbox");
        if task.issue_type.as_deref() == Some("inbox") {
            task.issue_type = Some("commitment".to_string());
        }
        if task.created_by.is_none() {
            task.created_by = request.actor;
        }

        vault.save_task(&task)?;
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.index_task(&task, &format!("{}.md", task.title));
            }
        }
        Ok(inbox_item_from_task(&task))
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
        let prior_id = task.id;
        let prior = vault.load_tasks().into_iter().find(|t| t.id == prior_id);

        task.date_modified = Some(Utc::now());
        drop(vault);
        let update = task_to_update_model(&task)?;
        let updated = CrudStorage::<TaskApi>::update(&self.task_storage, prior_id, update)
            .await
            .map_err(api_error_to_vault)?;
        let task = task_from_api(updated)?;

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
    pub async fn create_project(
        &self,
        mut project: Project,
        vault_name: Option<&str>,
    ) -> Result<Project, VaultError> {
        if project.id == Uuid::nil() {
            project.id = Uuid::new_v4();
        }
        if let Some(name) = vault_name {
            let extras = self.extra_vaults.read().await;
            let src = extras
                .iter()
                .find(|s| s.name == name)
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

    pub async fn project_dashboard(&self) -> Vec<ProjectDashboardEntry> {
        let projects = self.list_projects().await;
        let tasks = self.list_tasks().await;
        build_project_dashboard(&projects, &tasks)
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
        let task = self
            .vault
            .read()
            .await
            .load_tasks()
            .into_iter()
            .find(|task| task.matches_reference(&title))
            .ok_or_else(|| VaultError::NotFound(title.clone()))?;
        let task_title = task.title.clone();
        let task_id = task.id;
        CrudStorage::<TaskApi>::delete(&self.task_storage, task_id)
            .await
            .map_err(api_error_to_vault)?;
        if let Ok(guard) = self.index.lock() {
            if let Some(ref index) = *guard {
                let _ = index.record_change(
                    "task",
                    &task_title,
                    Some("deleted"),
                    Some("present"),
                    Some("deleted"),
                    actor,
                    Some(&format!("{}.md", task_title)),
                );
            }
        }
        Ok(())
    }

    pub async fn search_tasks(&self, query: String) -> Vec<Task> {
        // Try index first — extract matching titles under the mutex, then drop it
        // before hitting any .await so the MutexGuard doesn't cross an await point.
        let index_titles: Option<std::collections::HashSet<String>> =
            self.index.lock().ok().and_then(|guard| {
                guard.as_ref().and_then(|index| {
                    index
                        .search(&query)
                        .ok()
                        .map(|rows| rows.iter().map(|r| r.title.clone()).collect())
                })
            });

        if let Some(matching_titles) = index_titles {
            let all_tasks = self.vault.read().await.load_tasks();
            return all_tasks
                .into_iter()
                .filter(|t| matching_titles.contains(&t.title))
                .collect();
        }

        // Fallback: scan all tasks
        let tasks = self.vault.read().await.load_tasks();
        let q = query.to_lowercase();
        tasks
            .into_iter()
            .filter(|t| t.title.to_lowercase().contains(&q) || t.body.to_lowercase().contains(&q))
            .collect()
    }

    pub async fn tasks_for_user(&self, username: String) -> Vec<Task> {
        let tasks = self.vault.read().await.load_tasks();
        tasks
            .into_iter()
            .filter(|t| t.assignee.as_deref() == Some(&username))
            .collect()
    }

    pub async fn tasks_due_by(&self, date: String) -> Vec<Task> {
        let due_date = match chrono::NaiveDate::parse_from_str(&date, "%Y-%m-%d") {
            Ok(d) => d,
            Err(_) => return vec![],
        };
        let tasks = self.vault.read().await.load_tasks();
        tasks
            .into_iter()
            .filter(|t| t.due.map(|d| d <= due_date).unwrap_or(false) && !t.is_complete())
            .collect()
    }

    pub async fn tasks_for_project(&self, project_title: String) -> Vec<Task> {
        let tasks = self.vault.read().await.load_tasks();
        tasks
            .into_iter()
            .filter(|t| t.projects.iter().any(|p| p.0 == project_title))
            .collect()
    }

    pub async fn project_knowledge_context(
        &self,
        project_title: String,
        include_files: bool,
        depth: String,
    ) -> Result<Option<ProjectKnowledgeContext>, VaultError> {
        let project = self
            .list_projects()
            .await
            .into_iter()
            .find(|project| project.title.eq_ignore_ascii_case(&project_title));
        let Some(project) = project else {
            return Ok(None);
        };
        let tasks = self.tasks_for_project(project.title.clone()).await;
        let next_action = find_next_task(&project.title, &tasks).cloned();
        let project_path = project_storage_path(&project);
        let mut files = Vec::new();
        if include_files {
            if let Some(config) = NextcloudRuntimeConfig::load()? {
                let provider = nextcloud_webdav_provider(&config);
                files = provider
                    .list(&project_path, if depth.is_empty() { "1" } else { &depth })
                    .await?
                    .into_iter()
                    .map(project_file_summary_from_webdav)
                    .collect();
            }
        }
        let notes = files
            .iter()
            .filter(|file| file.role == "note")
            .cloned()
            .collect();
        let decisions = files
            .iter()
            .filter(|file| file.role == "decision")
            .cloned()
            .collect();
        let deliverables = files
            .iter()
            .filter(|file| file.role == "deliverable")
            .cloned()
            .collect();
        Ok(Some(ProjectKnowledgeContext {
            references: project
                .references
                .iter()
                .map(|link| link.0.clone())
                .collect(),
            project,
            project_path,
            tasks,
            next_action,
            files,
            notes,
            decisions,
            deliverables,
        }))
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
                self.record_provider_sync_state(
                    "caldav",
                    Some(&config.username),
                    &config.calendar,
                    None,
                    None,
                    None,
                    None,
                );
                let result = self
                    .merge_remote_tasks("caldav", remote_tasks, &local_tasks, &mut stats)
                    .await?;
                blocked_calendar_push.extend(result.blocked_push_keys);
            }
            Err(e) => {
                self.record_provider_sync_state(
                    "caldav",
                    Some(&config.username),
                    &config.calendar,
                    None,
                    None,
                    None,
                    Some(e.to_string()),
                );
                stats.errors.push(format!("CalDAV pull: {e}"));
            }
        }

        if let Some(event_calendar) = config.event_calendar.as_deref() {
            match sync.pull_events_from_calendar(event_calendar).await {
                Ok(remote_events) => {
                    self.record_provider_sync_state(
                        "caldav-events",
                        Some(&config.username),
                        event_calendar,
                        None,
                        None,
                        None,
                        None,
                    );
                    self.merge_remote_events(remote_events, &mut stats).await?;
                }
                Err(e) => {
                    self.record_provider_sync_state(
                        "caldav-events",
                        Some(&config.username),
                        event_calendar,
                        None,
                        None,
                        None,
                        Some(e.to_string()),
                    );
                    stats.errors.push(format!("CalDAV event pull: {e}"));
                }
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
                    self.record_provider_sync_state(
                        "deck",
                        Some(&config.username),
                        "boards",
                        None,
                        None,
                        None,
                        None,
                    );
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
                Err(e) => {
                    self.record_provider_sync_state(
                        "deck",
                        Some(&config.username),
                        "boards",
                        None,
                        None,
                        None,
                        Some(e.to_string()),
                    );
                    stats.errors.push(format!("Deck list boards: {e}"));
                }
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

    pub async fn sync_plan(&self) -> SyncPlan {
        build_sync_plan(NextcloudRuntimeConfig::load().ok().flatten())
    }

    pub async fn list_provider_sync_states(&self) -> Result<Vec<ProviderSyncState>, VaultError> {
        let guard = self
            .index
            .lock()
            .map_err(|e| VaultError::IoError(e.to_string()))?;
        match guard.as_ref() {
            Some(index) => index.list_sync_states(),
            None => Ok(Vec::new()),
        }
    }

    fn record_provider_sync_state(
        &self,
        provider: &str,
        account: Option<&str>,
        collection: &str,
        sync_token: Option<String>,
        cursor: Option<String>,
        etag: Option<String>,
        error: Option<String>,
    ) {
        let now = Utc::now().to_rfc3339();
        let state = ProviderSyncState {
            provider: provider.to_string(),
            account: account.map(str::to_string),
            collection: collection.to_string(),
            sync_token,
            cursor,
            etag,
            last_success_at: if error.is_none() {
                Some(now.clone())
            } else {
                None
            },
            last_failure_at: if error.is_some() { Some(now) } else { None },
            last_error: error,
            updated_at: String::new(),
        };
        if let Ok(guard) = self.index.lock() {
            if let Some(index) = guard.as_ref() {
                let _ = index.upsert_sync_state(&state);
            }
        }
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

    pub async fn discover_carddav(&self) -> Result<CardDavDiscovery, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CardDAV is not configured".into()))?;
        Self::nextcloud_sync_from_config(&config)
            .discover_addressbooks()
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

    pub async fn addressbook_multiget(
        &self,
        request: CardDavMultigetRequest,
    ) -> Result<Vec<CardDavObject>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CardDAV is not configured".into()))?;
        let addressbook = if request.addressbook.is_empty() {
            "contacts"
        } else {
            request.addressbook.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .addressbook_multiget(addressbook, &request.hrefs)
            .await
    }

    pub async fn addressbook_sync_collection(
        &self,
        request: CardDavSyncCollectionRequest,
    ) -> Result<CardDavSyncCollectionResponse, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CardDAV is not configured".into()))?;
        let addressbook = if request.addressbook.is_empty() {
            "contacts"
        } else {
            request.addressbook.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .sync_addressbook_collection(addressbook, request.sync_token.as_deref())
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

    pub async fn put_addressbook_object(
        &self,
        request: CardDavPutObjectRequest,
    ) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CardDAV is not configured".into()))?;
        let addressbook = if request.addressbook.is_empty() {
            "contacts"
        } else {
            request.addressbook.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .put_addressbook_object(
                addressbook,
                &request.href,
                &request.address_data,
                request.if_match.as_deref(),
                request.if_none_match.as_deref(),
            )
            .await
    }

    pub async fn delete_addressbook_object(
        &self,
        request: CardDavDeleteObjectRequest,
    ) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CardDAV is not configured".into()))?;
        let addressbook = if request.addressbook.is_empty() {
            "contacts"
        } else {
            request.addressbook.as_str()
        };
        Self::nextcloud_sync_from_config(&config)
            .delete_addressbook_object(addressbook, &request.href, request.if_match.as_deref())
            .await
    }

    pub async fn list_people_from_carddav(
        &self,
        addressbook: Option<String>,
    ) -> Result<Vec<Person>, VaultError> {
        let objects = self.sync_carddav_people_objects(addressbook).await?;
        Ok(objects
            .into_iter()
            .filter(|object| !object.deleted)
            .filter_map(person_from_carddav_object)
            .collect())
    }

    pub async fn list_organizations_from_carddav(
        &self,
        addressbook: Option<String>,
    ) -> Result<Vec<OrganizationRecord>, VaultError> {
        Ok(organizations_from_people(
            &self.list_people_from_carddav(addressbook).await?,
        ))
    }

    pub async fn person_context_from_carddav(
        &self,
        reference: String,
        addressbook: Option<String>,
    ) -> Result<Option<PersonContext>, VaultError> {
        let people = self.list_people_from_carddav(addressbook).await?;
        let Some(person) = people
            .into_iter()
            .find(|person| person_matches_reference(person, &reference))
        else {
            return Ok(None);
        };
        let tokens = person_context_tokens(&person);
        let tasks = self.vault.read().await.load_tasks();
        let projects = self.list_projects().await;
        let events = self.list_calendar_events().await;
        let mut communications = communication_refs_for_tokens(&tasks, &events, &tokens);
        communications.extend(self.channel_communication_refs_for_tokens(&tokens).await);
        Ok(Some(PersonContext {
            communications,
            tasks: tasks
                .into_iter()
                .filter(|task| task_matches_tokens(task, &tokens))
                .collect(),
            projects: projects
                .into_iter()
                .filter(|project| project_matches_tokens(project, &tokens))
                .collect(),
            calendar_events: events
                .into_iter()
                .filter(|event| event_matches_tokens(event, &tokens))
                .collect(),
            person,
        }))
    }

    pub async fn organization_context_from_carddav(
        &self,
        reference: String,
        addressbook: Option<String>,
    ) -> Result<Option<OrganizationContext>, VaultError> {
        let people = self.list_people_from_carddav(addressbook).await?;
        let organizations = organizations_from_people(&people);
        let Some(organization) = organizations
            .into_iter()
            .find(|organization| organization_matches_reference(organization, &reference))
        else {
            return Ok(None);
        };
        let org_people: Vec<Person> = people
            .into_iter()
            .filter(|person| person.organization.as_deref() == Some(organization.name.as_str()))
            .collect();
        let tokens = organization_context_tokens(&organization, &org_people);
        let tasks = self.vault.read().await.load_tasks();
        let projects = self.list_projects().await;
        let events = self.list_calendar_events().await;
        let mut communications = communication_refs_for_tokens(&tasks, &events, &tokens);
        communications.extend(self.channel_communication_refs_for_tokens(&tokens).await);
        Ok(Some(OrganizationContext {
            communications,
            tasks: tasks
                .into_iter()
                .filter(|task| task_matches_tokens(task, &tokens))
                .collect(),
            projects: projects
                .into_iter()
                .filter(|project| project_matches_tokens(project, &tokens))
                .collect(),
            calendar_events: events
                .into_iter()
                .filter(|event| event_matches_tokens(event, &tokens))
                .collect(),
            people: org_people,
            organization,
        }))
    }

    async fn channel_communication_refs_for_tokens(
        &self,
        tokens: &[String],
    ) -> Vec<CommunicationRef> {
        let Some(config) = NextcloudRuntimeConfig::load().ok().flatten() else {
            return Vec::new();
        };
        let provider = nextcloud_talk_provider(&config);
        let Ok(conversations) = provider.list_conversations().await else {
            return Vec::new();
        };
        let mut refs = Vec::new();
        for conversation in conversations.iter().take(25) {
            let conversation_matches = text_matches_tokens(
                &format!(
                    "{}\n{}",
                    conversation.name,
                    conversation.last_message.clone().unwrap_or_default()
                ),
                tokens,
            );
            if conversation_matches {
                refs.push(CommunicationRef {
                    kind: "conversation".to_string(),
                    external_id: conversation.id.clone(),
                    summary: Some(conversation.name.clone()),
                    occurred_at: conversation
                        .last_activity
                        .and_then(|ts| DateTime::<Utc>::from_timestamp(ts, 0)),
                    provider: Some(conversation.provider.clone()),
                });
            }
            let Ok(messages) =
                CommunicationChannelProvider::recent_messages(&provider, &conversation.id, 25)
                    .await
            else {
                continue;
            };
            for message in messages
                .into_iter()
                .filter(|message| text_matches_tokens(&message.body, tokens))
                .take(10)
            {
                refs.push(CommunicationRef {
                    kind: "message".to_string(),
                    external_id: message.id,
                    summary: Some(format!("{}: {}", message.actor_display_name, message.body)),
                    occurred_at: DateTime::<Utc>::from_timestamp(message.timestamp, 0),
                    provider: Some(message.provider),
                });
            }
        }
        refs
    }

    async fn sync_carddav_people_objects(
        &self,
        addressbook: Option<String>,
    ) -> Result<Vec<CardDavObject>, VaultError> {
        let discovery = self.discover_carddav().await?;
        let addressbook = addressbook
            .filter(|name| !name.is_empty())
            .or_else(|| {
                discovery
                    .addressbooks
                    .iter()
                    .find(|book| book.name == "contacts")
                    .map(|book| book.name.clone())
            })
            .or_else(|| discovery.addressbooks.first().map(|book| book.name.clone()))
            .ok_or_else(|| {
                VaultError::NotFound("no CardDAV addressbooks discovered".to_string())
            })?;
        let sync = self
            .addressbook_sync_collection(CardDavSyncCollectionRequest {
                addressbook: addressbook.clone(),
                sync_token: None,
            })
            .await?;
        self.record_provider_sync_state(
            "carddav",
            None,
            &addressbook,
            sync.sync_token.clone(),
            None,
            None,
            None,
        );
        Ok(sync.objects)
    }

    pub async fn send_calendar_schedule(
        &self,
        request: CalDavScheduleRequest,
    ) -> Result<CalDavScheduleResponse, VaultError> {
        let config = NextcloudRuntimeConfig::load()?
            .ok_or_else(|| VaultError::IoError("Nextcloud CalDAV is not configured".into()))?;
        let sync = Self::nextcloud_sync_from_config(&config);
        let outbox_url = match request.outbox_url {
            Some(url) if !url.is_empty() => url,
            _ => sync
                .discover_calendars()
                .await?
                .schedule_outbox_url
                .ok_or_else(|| {
                    VaultError::IoError("CalDAV scheduling outbox is not available".into())
                })?,
        };
        sync.send_calendar_schedule(&outbox_url, &request.calendar_data)
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
        if let Some(venue) = patch.venue {
            event.venue = venue;
        }
        if let Some(spaces) = patch.spaces {
            event.spaces = spaces.into();
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
            event.attendees = attendees.into();
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
                                &local.id_ref(),
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
                            stats
                                .errors
                                .push(format!("{source} conflict log '{}': {e}", local.title));
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
        let year = today
            .format("%Y")
            .to_string()
            .parse::<i32>()
            .unwrap_or(2026);
        let number = self.next_invoice_number(year).await?;
        let id = crate::invoice::format_invoice_id(year, number);

        let now = Utc::now();
        let invoice = crate::invoice::Invoice {
            uuid: Uuid::new_v4(),
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
            line_items: line_items.into(),
            tax_rate_percent,
            discount_percent,
            po_number,
            public_notes,
            private_notes: None,
            payments: Vec::new().into(),
            entry_ids: entry_ids.clone().into(),
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

    pub async fn finance_report(&self) -> BusinessFinanceReport {
        let today = chrono::Local::now().date_naive();
        let time_entries = self.list_time_entries(TimeEntryFilter::default()).await;
        let invoices = self.list_invoices().await;
        build_finance_report(time_entries, invoices, today)
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
        if matches!(new_status, crate::invoice::InvoiceStatus::Paid) && invoice.paid_at.is_none() {
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

    async fn next_expense_number(&self, year: i32) -> Result<u32, VaultError> {
        let prefix = format!("EXP-{year:04}-");
        let max = self
            .vault
            .read()
            .await
            .load_expenses()
            .into_iter()
            .filter(|expense| expense.id.starts_with(&prefix))
            .map(|expense| expense.number)
            .max()
            .unwrap_or(0);
        Ok(max + 1)
    }

    async fn next_asset_number(&self, year: i32) -> Result<u32, VaultError> {
        let prefix = format!("AST-{year:04}-");
        let max = self
            .vault
            .read()
            .await
            .load_assets()
            .into_iter()
            .filter(|asset| asset.id.starts_with(&prefix))
            .map(|asset| asset.number)
            .max()
            .unwrap_or(0);
        Ok(max + 1)
    }

    pub async fn list_assets(&self, filter: AssetFilter) -> Vec<Asset> {
        let mut assets = self.vault.read().await.load_assets();
        assets.retain(|asset| matches_asset_filter(asset, &filter));
        assets.sort_by(|a, b| a.name.cmp(&b.name).then_with(|| a.number.cmp(&b.number)));
        assets
    }

    pub async fn get_asset(&self, asset_id: &str) -> Option<Asset> {
        self.vault
            .read()
            .await
            .load_assets()
            .into_iter()
            .find(|asset| {
                asset.id.eq_ignore_ascii_case(asset_id) || asset.name.eq_ignore_ascii_case(asset_id)
            })
    }

    pub async fn create_asset(&self, request: AssetCreateRequest) -> Result<Asset, VaultError> {
        let now = Utc::now();
        let number = self.next_asset_number(now.year()).await?;
        let id = format_asset_id(now.year(), number);
        let asset = Asset {
            uuid: Uuid::new_v4(),
            id,
            number,
            name: request.name,
            status: request
                .status
                .as_deref()
                .and_then(parse_asset_status)
                .unwrap_or(AssetStatus::Available),
            manufacturer: request.manufacturer,
            model: request.model,
            serial_number: request.serial_number,
            category: request.category,
            organization: request.organization,
            location: request.location.map(WikiLink),
            space: request.space.map(WikiLink),
            rack_or_case: request.rack_or_case,
            assigned_to: request.assigned_to,
            purchase_date: request.purchase_date,
            warranty_until: request.warranty_until,
            vendor: request.vendor,
            cost_cents: request.cost_cents.map(|value| value as i64),
            maintenance: Vec::new().into(),
            reservations: Vec::new().into(),
            linked_tasks: Vec::new().into(),
            notes: request.notes,
            created_by: request.actor,
            date_created: Some(now),
            date_modified: Some(now),
            body: String::new(),
        };
        self.vault.read().await.save_asset(&asset)?;
        Ok(asset)
    }

    pub async fn update_asset(
        &self,
        asset_id: &str,
        patch: AssetPatch,
        actor: Option<&str>,
    ) -> Result<Asset, VaultError> {
        let vault = self.vault.read().await;
        let mut asset = vault
            .load_assets()
            .into_iter()
            .find(|asset| {
                asset.id.eq_ignore_ascii_case(asset_id) || asset.name.eq_ignore_ascii_case(asset_id)
            })
            .ok_or_else(|| VaultError::NotFound(asset_id.to_string()))?;
        let now = Utc::now();
        if let Some(name) = patch.name {
            asset.name = name;
        }
        if let Some(status) = patch.status.as_deref() {
            asset.status = parse_asset_status(status)
                .ok_or_else(|| VaultError::ParseError(format!("invalid asset status: {status}")))?;
        }
        if let Some(v) = patch.manufacturer {
            asset.manufacturer = if v.trim().is_empty() { None } else { Some(v) };
        }
        if let Some(v) = patch.model {
            asset.model = if v.trim().is_empty() { None } else { Some(v) };
        }
        if let Some(v) = patch.serial_number {
            asset.serial_number = if v.trim().is_empty() { None } else { Some(v) };
        }
        if let Some(v) = patch.category {
            asset.category = if v.trim().is_empty() { None } else { Some(v) };
        }
        if let Some(v) = patch.organization {
            asset.organization = if v.trim().is_empty() { None } else { Some(v) };
        }
        if let Some(v) = patch.location {
            asset.location = if v.trim().is_empty() || v == "clear" {
                None
            } else {
                Some(WikiLink(v))
            };
        }
        if let Some(v) = patch.space {
            asset.space = if v.trim().is_empty() || v == "clear" {
                None
            } else {
                Some(WikiLink(v))
            };
        }
        if let Some(v) = patch.rack_or_case {
            asset.rack_or_case = if v.trim().is_empty() { None } else { Some(v) };
        }
        if let Some(v) = patch.assigned_to {
            asset.assigned_to = if v.trim().is_empty() { None } else { Some(v) };
        }
        if let Some(v) = patch.purchase_date {
            asset.purchase_date = if v.trim().is_empty() || v == "clear" {
                None
            } else {
                Some(
                    NaiveDate::parse_from_str(&v, "%Y-%m-%d")
                        .map_err(|e| VaultError::ParseError(e.to_string()))?,
                )
            };
        }
        if let Some(v) = patch.warranty_until {
            asset.warranty_until = if v.trim().is_empty() || v == "clear" {
                None
            } else {
                Some(
                    NaiveDate::parse_from_str(&v, "%Y-%m-%d")
                        .map_err(|e| VaultError::ParseError(e.to_string()))?,
                )
            };
        }
        if let Some(v) = patch.vendor {
            asset.vendor = if v.trim().is_empty() { None } else { Some(v) };
        }
        if let Some(v) = patch.cost_cents {
            asset.cost_cents = v.map(|value| value as i64);
        }
        if let Some(v) = patch.notes {
            asset.notes = if v.trim().is_empty() { None } else { Some(v) };
        }
        asset.date_modified = Some(now);
        vault.save_asset(&asset)?;
        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "asset",
                    &asset.id,
                    Some("update"),
                    None,
                    None,
                    actor,
                    Some(&format!("assets/{}.md", asset.name)),
                );
            }
        }
        Ok(asset)
    }

    pub async fn log_asset_maintenance(
        &self,
        asset_id: &str,
        request: AssetMaintenanceRequest,
        actor: Option<&str>,
    ) -> Result<Asset, VaultError> {
        let vault = self.vault.read().await;
        let mut asset = vault
            .load_assets()
            .into_iter()
            .find(|asset| {
                asset.id.eq_ignore_ascii_case(asset_id) || asset.name.eq_ignore_ascii_case(asset_id)
            })
            .ok_or_else(|| VaultError::NotFound(asset_id.to_string()))?;
        let now = Utc::now();
        asset.maintenance.push(AssetMaintenanceRecord {
            date: request.date.unwrap_or_else(|| now.date_naive()),
            issue: request.issue,
            vendor: request.vendor,
            contact: request.contact,
            cost_cents: request.cost_cents.map(|value| value as i64),
            warranty: request.warranty,
            rma: request.rma,
            task: request.task.map(WikiLink),
            notes: request.notes,
        });
        asset.status = if asset.status == AssetStatus::Available {
            AssetStatus::NeedsRepair
        } else {
            asset.status.clone()
        };
        asset.date_modified = Some(now);
        vault.save_asset(&asset)?;
        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "asset",
                    &asset.id,
                    Some("update"),
                    None,
                    None,
                    actor,
                    Some(&format!("assets/{}.md", asset.name)),
                );
            }
        }
        Ok(asset)
    }

    pub async fn open_asset_repair(
        &self,
        asset_id: &str,
        request: AssetRepairRequest,
    ) -> Result<AssetRepairResponse, VaultError> {
        let mut asset = self
            .vault
            .read()
            .await
            .load_assets()
            .into_iter()
            .find(|asset| {
                asset.id.eq_ignore_ascii_case(asset_id) || asset.name.eq_ignore_ascii_case(asset_id)
            })
            .ok_or_else(|| VaultError::NotFound(asset_id.to_string()))?;

        let task = self
            .create_task(Task {
                title: request.title.clone(),
                status: Status::Open,
                priority: Priority::Normal,
                tags: vec!["asset".into(), "repair".into()].into(),
                body: request.notes.clone().unwrap_or_default(),
                created_by: request.actor.clone(),
                ..Task::default()
            })
            .await?;

        let now = Utc::now();
        let task_link = WikiLink(task.title.clone());
        if !asset.linked_tasks.iter().any(|link| link.0 == task_link.0) {
            asset.linked_tasks.push(task_link.clone());
        }
        asset.maintenance.push(AssetMaintenanceRecord {
            date: now.date_naive(),
            issue: request.title,
            vendor: request.vendor,
            contact: request.contact,
            cost_cents: request.cost_cents.map(|value| value as i64),
            warranty: request.warranty,
            rma: request.rma,
            task: Some(task_link),
            notes: request.notes,
        });
        if matches!(
            asset.status,
            AssetStatus::Available
                | AssetStatus::Reserved
                | AssetStatus::InUse
                | AssetStatus::MaintenanceDue
        ) {
            asset.status = AssetStatus::NeedsRepair;
        }
        asset.date_modified = Some(now);
        self.vault.read().await.save_asset(&asset)?;

        Ok(AssetRepairResponse { asset, task })
    }

    pub async fn reserve_asset(
        &self,
        asset_id: &str,
        request: AssetReserveRequest,
    ) -> Result<AssetReservationResponse, VaultError> {
        let vault = self.vault.read().await;
        let mut asset = vault
            .load_assets()
            .into_iter()
            .find(|asset| {
                asset.id.eq_ignore_ascii_case(asset_id) || asset.name.eq_ignore_ascii_case(asset_id)
            })
            .ok_or_else(|| VaultError::NotFound(asset_id.to_string()))?;
        let reservation = AssetReservationRecord {
            id: Uuid::new_v4().to_string(),
            reference: WikiLink(request.reference),
            starts_at: request.starts_at,
            ends_at: request.ends_at,
            reserved_by: request.reserved_by,
            notes: request.notes,
        };
        let conflicts = conflicts_for_reservation(&asset, &reservation);
        if !request.force && !conflicts.is_empty() {
            return Err(VaultError::ParseError(format!(
                "asset reservation has {} conflict(s); rerun with --force to record anyway",
                conflicts.len()
            )));
        }

        asset.reservations.push(reservation.clone());
        if asset.status == AssetStatus::Available {
            asset.status = AssetStatus::Reserved;
        }
        asset.date_modified = Some(Utc::now());
        vault.save_asset(&asset)?;
        Ok(AssetReservationResponse {
            asset,
            reservation,
            conflicts,
        })
    }

    pub async fn release_asset_reservation(
        &self,
        asset_id: &str,
        reservation_ref: &str,
    ) -> Result<Asset, VaultError> {
        let vault = self.vault.read().await;
        let mut asset = vault
            .load_assets()
            .into_iter()
            .find(|asset| {
                asset.id.eq_ignore_ascii_case(asset_id) || asset.name.eq_ignore_ascii_case(asset_id)
            })
            .ok_or_else(|| VaultError::NotFound(asset_id.to_string()))?;
        let before = asset.reservations.len();
        asset.reservations.retain(|reservation| {
            reservation.id != reservation_ref
                && !reservation
                    .reference
                    .0
                    .eq_ignore_ascii_case(reservation_ref)
        });
        if asset.reservations.len() == before {
            return Err(VaultError::NotFound(reservation_ref.to_string()));
        }
        if asset.status == AssetStatus::Reserved && asset.reservations.is_empty() {
            asset.status = AssetStatus::Available;
        }
        asset.date_modified = Some(Utc::now());
        vault.save_asset(&asset)?;
        Ok(asset)
    }

    pub async fn asset_conflicts(&self, filter: AssetFilter) -> Vec<AssetConflict> {
        let assets = self.list_assets(filter).await;
        collect_asset_conflicts(&assets)
    }

    pub async fn delete_asset(&self, asset_id: &str) -> Result<(), VaultError> {
        self.vault.read().await.delete_asset(asset_id)
    }

    pub async fn asset_report(&self, filter: AssetFilter) -> AssetReport {
        let today = Utc::now().date_naive();
        let assets = self.list_assets(filter).await;
        build_asset_report(&assets, today)
    }

    pub async fn list_locations(&self) -> Vec<Location> {
        let mut locations = self.vault.read().await.load_locations();
        locations.retain(|location| !location.is_deleted());
        locations.sort_by(|a, b| a.name.cmp(&b.name));
        locations
    }

    pub async fn get_location(&self, reference: &str) -> Option<Location> {
        self.vault
            .read()
            .await
            .load_locations()
            .into_iter()
            .find(|location| {
                location.id.as_deref() == Some(reference)
                    || location.name.eq_ignore_ascii_case(reference)
            })
    }

    pub async fn save_location_record(
        &self,
        mut location: Location,
    ) -> Result<Location, VaultError> {
        let now = Utc::now();
        if location.id.as_deref().unwrap_or("").is_empty() {
            location.id = Some(Uuid::new_v4().to_string());
        }
        if location.date_created.is_none() {
            location.date_created = Some(now);
        }
        location.date_modified = Some(now);
        self.vault.read().await.save_location(&location)?;
        Ok(location)
    }

    pub async fn update_location_record(
        &self,
        reference: &str,
        mut patch: Location,
    ) -> Result<Location, VaultError> {
        let mut location = self
            .get_location(reference)
            .await
            .ok_or_else(|| VaultError::NotFound(reference.to_string()))?;
        if !patch.name.trim().is_empty() {
            location.name = patch.name;
        }
        macro_rules! patch_opt {
            ($field:ident) => {
                if patch.$field.is_some() {
                    location.$field = patch.$field.take();
                }
            };
        }
        patch_opt!(address1);
        patch_opt!(address2);
        patch_opt!(city);
        patch_opt!(state);
        patch_opt!(postal_code);
        patch_opt!(country_code);
        patch_opt!(contact_name);
        patch_opt!(contact_email);
        patch_opt!(contact_phone);
        patch_opt!(access_notes);
        patch_opt!(parking_load_in);
        patch_opt!(network_power);
        patch_opt!(venue_type);
        if !patch.tags.is_empty() {
            location.tags = patch.tags;
        }
        if !patch.body.is_empty() {
            location.body = patch.body;
        }
        self.save_location_record(location).await
    }

    pub async fn add_location_space(
        &self,
        reference: &str,
        mut space: Space,
    ) -> Result<Location, VaultError> {
        let mut location = self
            .get_location(reference)
            .await
            .ok_or_else(|| VaultError::NotFound(reference.to_string()))?;
        if space.id.as_deref().unwrap_or("").is_empty() {
            space.id = Some(Uuid::new_v4().to_string());
        }
        if let Some(existing) = location
            .spaces
            .iter_mut()
            .find(|existing| existing.name.eq_ignore_ascii_case(&space.name))
        {
            *existing = space;
        } else {
            location.spaces.push(space);
        }
        self.save_location_record(location).await
    }

    pub async fn add_location_default(
        &self,
        reference: &str,
        default: VenueDefault,
        space_name: Option<String>,
    ) -> Result<Location, VaultError> {
        let mut location = self
            .get_location(reference)
            .await
            .ok_or_else(|| VaultError::NotFound(reference.to_string()))?;
        if let Some(space_name) = space_name {
            let Some(space) = location
                .spaces
                .iter_mut()
                .find(|space| space.name.eq_ignore_ascii_case(&space_name))
            else {
                return Err(VaultError::NotFound(space_name));
            };
            space.default_files.retain(|item| item.kind != default.kind);
            space.default_files.push(default);
        } else {
            location
                .default_files
                .retain(|item| item.kind != default.kind);
            location.default_files.push(default);
        }
        self.save_location_record(location).await
    }

    pub async fn delete_location_record(&self, reference: &str) -> Result<(), VaultError> {
        self.vault.read().await.delete_location(reference)
    }

    pub async fn list_expenses(&self, filter: ExpenseFilter) -> Vec<Expense> {
        let mut expenses = self.vault.read().await.load_expenses();
        expenses.retain(|expense| matches_expense_filter(expense, &filter));
        expenses.sort_by(|a, b| b.date.cmp(&a.date).then_with(|| b.number.cmp(&a.number)));
        expenses
    }

    pub async fn get_expense(&self, expense_id: &str) -> Option<Expense> {
        self.vault
            .read()
            .await
            .load_expenses()
            .into_iter()
            .find(|expense| expense.id.eq_ignore_ascii_case(expense_id))
    }

    pub async fn create_expense(
        &self,
        request: ExpenseCreateRequest,
    ) -> Result<Expense, VaultError> {
        let now = Utc::now();
        let date = request.date.unwrap_or_else(|| now.date_naive());
        let number = self.next_expense_number(date.year()).await?;
        let id = format_expense_id(date.year(), number);
        let status = request
            .status
            .as_deref()
            .and_then(parse_expense_status)
            .unwrap_or(ExpenseStatus::Draft);
        let expense = Expense {
            uuid: Uuid::new_v4(),
            id,
            number,
            status,
            date,
            amount_cents: request.amount_cents,
            currency_code: request.currency_code.unwrap_or_else(|| "USD".into()),
            project: request.project.map(crate::task::WikiLink),
            client: request.client.map(crate::task::WikiLink),
            deliverable: request.deliverable,
            category: request.category,
            vendor: request.vendor,
            description: request.description,
            receipt: request.receipt,
            reference: request.reference,
            reimbursable: request.reimbursable,
            notes: request.notes,
            created_by: request.actor,
            date_created: Some(now),
            date_modified: Some(now),
            body: String::new(),
        };
        self.vault.read().await.save_expense(&expense)?;
        Ok(expense)
    }

    pub async fn update_expense(
        &self,
        expense_id: &str,
        patch: ExpensePatch,
        actor: Option<&str>,
    ) -> Result<Expense, VaultError> {
        let vault = self.vault.read().await;
        let mut expense = vault
            .load_expenses()
            .into_iter()
            .find(|expense| expense.id.eq_ignore_ascii_case(expense_id))
            .ok_or_else(|| VaultError::NotFound(expense_id.to_string()))?;
        let now = Utc::now();
        if let Some(status) = patch.status.as_deref() {
            expense.status = parse_expense_status(status).ok_or_else(|| {
                VaultError::ParseError(format!("invalid expense status: {status}"))
            })?;
        }
        if let Some(date) = patch.date.as_deref() {
            expense.date = NaiveDate::parse_from_str(date, "%Y-%m-%d")
                .map_err(|e| VaultError::ParseError(e.to_string()))?;
        }
        if let Some(amount) = patch.amount_cents {
            expense.amount_cents = amount;
        }
        if let Some(currency) = patch.currency_code {
            expense.currency_code = currency;
        }
        if let Some(project) = patch.project {
            expense.project = if project.trim().is_empty() {
                None
            } else {
                Some(crate::task::WikiLink(project))
            };
        }
        if let Some(client) = patch.client {
            expense.client = if client.trim().is_empty() {
                None
            } else {
                Some(crate::task::WikiLink(client))
            };
        }
        if let Some(deliverable) = patch.deliverable {
            expense.deliverable = if deliverable.trim().is_empty() {
                None
            } else {
                Some(deliverable)
            };
        }
        if let Some(category) = patch.category {
            expense.category = if category.trim().is_empty() {
                None
            } else {
                Some(category)
            };
        }
        if let Some(vendor) = patch.vendor {
            expense.vendor = if vendor.trim().is_empty() {
                None
            } else {
                Some(vendor)
            };
        }
        if let Some(description) = patch.description {
            expense.description = description;
        }
        if let Some(receipt) = patch.receipt {
            expense.receipt = if receipt.trim().is_empty() {
                None
            } else {
                Some(receipt)
            };
        }
        if let Some(reference) = patch.reference {
            expense.reference = if reference.trim().is_empty() {
                None
            } else {
                Some(reference)
            };
        }
        if let Some(reimbursable) = patch.reimbursable {
            expense.reimbursable = reimbursable;
        }
        if let Some(notes) = patch.notes {
            expense.notes = if notes.trim().is_empty() {
                None
            } else {
                Some(notes)
            };
        }
        expense.date_modified = Some(now);
        vault.save_expense(&expense)?;
        if let Ok(guard) = self.index.lock() {
            if let Some(ref idx) = *guard {
                let _ = idx.record_change(
                    "expense",
                    &expense.id,
                    Some("update"),
                    None,
                    None,
                    actor,
                    Some(&format!("expenses/{}.md", expense.id)),
                );
            }
        }
        Ok(expense)
    }

    pub async fn delete_expense(&self, expense_id: &str) -> Result<(), VaultError> {
        self.vault.read().await.delete_expense(expense_id)
    }

    pub async fn expense_report(&self, filter: ExpenseFilter) -> ExpenseReport {
        let today = Utc::now().date_naive();
        let expenses = self.list_expenses(filter).await;
        build_expense_report(&expenses, today)
    }

    async fn next_revenue_number(&self, year: i32) -> Result<u32, VaultError> {
        let prefix = format!("REV-{year:04}-");
        let max = self
            .vault
            .read()
            .await
            .load_revenues()
            .into_iter()
            .filter(|revenue| revenue.id.starts_with(&prefix))
            .map(|revenue| revenue.number)
            .max()
            .unwrap_or(0);
        Ok(max + 1)
    }

    pub async fn create_revenue(
        &self,
        request: RevenueCreateRequest,
    ) -> Result<Revenue, VaultError> {
        let now = Utc::now();
        let date = request.date.unwrap_or_else(|| now.date_naive());
        let number = self.next_revenue_number(date.year()).await?;
        let revenue = Revenue {
            uuid: Uuid::new_v4(),
            id: format_revenue_id(date.year(), number),
            number,
            date,
            amount_cents: request.amount_cents,
            currency_code: request.currency_code.unwrap_or_else(|| "USD".into()),
            project: request.project.map(WikiLink),
            client: request.client.map(WikiLink),
            deliverable: request.deliverable,
            invoice_id: request.invoice_id,
            invoice_line_id: request.invoice_line_id,
            category: request.category,
            payment_method: request.payment_method,
            payment_reference: request.payment_reference,
            description: request.description,
            notes: request.notes,
            created_by: request.actor,
            date_created: Some(now),
            date_modified: Some(now),
            body: String::new(),
        };
        self.vault.read().await.save_revenue(&revenue)?;
        Ok(revenue)
    }

    pub async fn list_revenues(&self, filter: RevenueFilter) -> Vec<Revenue> {
        let mut revenues = self.vault.read().await.load_revenues();
        revenues.retain(|revenue| matches_revenue_filter(revenue, &filter));
        revenues.sort_by(|a, b| b.date.cmp(&a.date).then_with(|| b.number.cmp(&a.number)));
        revenues
    }

    pub async fn get_revenue(&self, revenue_id: &str) -> Option<Revenue> {
        self.vault
            .read()
            .await
            .load_revenues()
            .into_iter()
            .find(|revenue| revenue.id.eq_ignore_ascii_case(revenue_id))
    }

    pub async fn delete_revenue(&self, revenue_id: &str) -> Result<(), VaultError> {
        self.vault.read().await.delete_revenue(revenue_id)
    }

    pub async fn revenue_report(&self, filter: RevenueFilter) -> RevenueReport {
        let today = Utc::now().date_naive();
        let revenues = self.list_revenues(filter).await;
        let invoices = self.vault.read().await.load_invoices();
        build_revenue_report(&revenues, &invoices, today)
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
        diff_opt_string!(body, "body");
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
            let new_status = parse_project_status(&s)
                .ok_or_else(|| VaultError::ParseError(format!("unknown project status: {s}")))?;
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
            .find(|t| t.matches_reference(task_ref))
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
                let id = task.id_ref();
                let _ = idx.record_change(
                    "task",
                    &id,
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
            .find(|t| t.matches_reference(task_ref))
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
                let id = task.id_ref();
                let _ = idx.record_change(
                    "task",
                    &id,
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
            .find(|t| t.matches_reference(task_ref))
            .map(|t| t.emails.into_inner())
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
            .map(|p| p.emails.into_inner())
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
            s.trim()
                .trim_start_matches('<')
                .trim_end_matches('>')
                .to_ascii_lowercase()
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

        if let Some(active) = tasks
            .iter()
            .find_map(|t| t.running_timer().map(|e| (t.title.clone(), e.id.clone())))
        {
            return Err(VaultError::IoError(format!(
                "timer already running on '{}' (id {})",
                active.0, active.1
            )));
        }

        let mut task = tasks
            .into_iter()
            .find(|t| t.matches_reference(task_ref))
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

        let target = tasks.into_iter().find(|t| match task_ref {
            Some(r) => t.matches_reference(r) && t.running_timer().is_some(),
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
            return Err(VaultError::ParseError("end must be after start".into()));
        }
        let vault = self.vault.read().await;
        let tasks = vault.load_tasks();
        let mut task = tasks
            .into_iter()
            .find(|t| t.matches_reference(task_ref))
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
    pub async fn list_time_entries(&self, filter: TimeEntryFilter) -> Vec<TimeEntryContext> {
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
                let matches = t.matches_reference(r);
                if !matches {
                    continue;
                }
            }
            if let Some(ref p) = filter.project {
                if !t.projects.iter().any(|w| w.0.eq_ignore_ascii_case(p)) {
                    continue;
                }
            }
            let task_projects: Vec<String> = t.projects.iter().map(|w| w.0.clone()).collect();

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
        let guard = self
            .index
            .lock()
            .map_err(|_| VaultError::IoError("index poisoned".into()))?;
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
        let guard = self
            .index
            .lock()
            .map_err(|_| VaultError::IoError("index poisoned".into()))?;
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
        let guard = self
            .index
            .lock()
            .map_err(|_| VaultError::IoError("index poisoned".into()))?;
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
        let guard = self
            .index
            .lock()
            .map_err(|_| VaultError::IoError("index poisoned".into()))?;
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
                    return Err(VaultError::ParseError("end must be after start".into()));
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
                let id = task.id_ref();
                // Include the entry id in the new_value so the audit row is
                // self-describing.
                let payload = match new_value {
                    Some(v) => format!("{v} (entry {})", entry.id),
                    None => format!("entry {}", entry.id),
                };
                let _ = index.record_change(
                    "task",
                    &id,
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

fn normalize_inbox_kind(kind: Option<&str>) -> String {
    match kind.unwrap_or("inbox").trim().to_ascii_lowercase().as_str() {
        "commitment" | "committed" => "commitment".to_string(),
        "idea" | "someday" | "maybe" => "idea".to_string(),
        "task" | "action" => "task".to_string(),
        "waiting" | "waiting-on" => "waiting".to_string(),
        "reference" | "note" => "reference".to_string(),
        "inbox" | "" => "inbox".to_string(),
        other => other.to_string(),
    }
}

fn inbox_item_from_task(task: &Task) -> InboxItem {
    InboxItem {
        id: Some(task.id_ref()),
        title: task.title.clone(),
        kind: task
            .issue_type
            .clone()
            .unwrap_or_else(|| "task".to_string()),
        status: status_label(&task.status).to_string(),
        priority: priority_label(&task.priority).to_string(),
        projects: task
            .projects
            .iter()
            .map(|project| project.0.clone())
            .collect(),
        tags: task.tags.to_vec(),
        contexts: task.contexts.to_vec(),
        due: task.due.map(|date| date.to_string()),
        scheduled: task.scheduled.map(|date| date.to_string()),
        assignee: task.assignee.clone(),
        source: task
            .external_source
            .as_deref()
            .and_then(|source| source.strip_prefix("inbox:"))
            .map(str::to_string),
        body: task.body.clone(),
    }
}

fn dedupe_tasks(tasks: Vec<Task>) -> Vec<Task> {
    let mut seen = HashSet::new();
    tasks
        .into_iter()
        .filter(|task| seen.insert(task.id))
        .collect()
}

fn task_to_create_model(task: &Task) -> Result<TaskApiCreate, VaultError> {
    serde_json::to_value(task)
        .and_then(serde_json::from_value)
        .map_err(|e| VaultError::ParseError(format!("failed to build task create model: {e}")))
}

fn task_to_update_model(task: &Task) -> Result<TaskApiUpdate, VaultError> {
    serde_json::to_value(task)
        .and_then(serde_json::from_value)
        .map_err(|e| VaultError::ParseError(format!("failed to build task update model: {e}")))
}

fn task_from_api(task: TaskApi) -> Result<Task, VaultError> {
    task_from_json_model(task)
        .ok_or_else(|| VaultError::ParseError("failed to decode task repo model".to_string()))
}

fn task_from_json_model<T>(task: T) -> Option<Task>
where
    T: Serialize,
{
    let mut base = serde_json::to_value(Task::default()).ok()?;
    let patch = serde_json::to_value(task).ok()?;
    merge_json_value(&mut base, patch)?;
    serde_json::from_value(base).ok()
}

fn merge_json_value(base: &mut serde_json::Value, patch: serde_json::Value) -> Option<()> {
    let base = base.as_object_mut()?;
    let serde_json::Value::Object(patch) = patch else {
        return None;
    };
    for (key, value) in patch {
        base.insert(key, value);
    }
    Some(())
}

fn api_error_to_vault(error: crudcrate::ApiError) -> VaultError {
    match error {
        crudcrate::ApiError::NotFound { id, .. } => {
            VaultError::NotFound(id.unwrap_or_else(|| "task".to_string()))
        }
        crudcrate::ApiError::BadRequest { message }
        | crudcrate::ApiError::Conflict { message }
        | crudcrate::ApiError::Unauthorized { message }
        | crudcrate::ApiError::Forbidden { message }
        | crudcrate::ApiError::Custom { message, .. } => VaultError::ParseError(message),
        crudcrate::ApiError::ValidationFailed { errors } => {
            VaultError::ParseError(errors.join("; "))
        }
        crudcrate::ApiError::Database { message, .. }
        | crudcrate::ApiError::Internal { message, .. } => VaultError::IoError(message),
    }
}

fn is_inbox_task(task: &Task) -> bool {
    !task.is_deleted()
        && (task.tags.iter().any(|tag| tag == "inbox")
            || task.issue_type.as_deref() == Some("inbox"))
}

fn build_review_report(
    mut tasks: Vec<Task>,
    horizon_days: i64,
    stale_after_days: u32,
) -> ReviewReport {
    let today = chrono::Local::now().date_naive();
    let horizon_end = today + chrono::Duration::days(horizon_days);
    let stale_before = today - chrono::Duration::days(stale_after_days as i64);
    sort_review_tasks(&mut tasks);

    ReviewReport {
        generated_at: Utc::now(),
        today: today.to_string(),
        horizon_end: horizon_end.to_string(),
        stale_after_days,
        inbox: tasks
            .iter()
            .filter(|task| is_inbox_task(task))
            .map(inbox_item_from_task)
            .collect(),
        commitments: review_tasks(&tasks, |task| {
            task.issue_type.as_deref() == Some("commitment")
        }),
        ideas: review_tasks(&tasks, |task| is_idea_task(task) && !is_someday_task(task)),
        someday: review_tasks(&tasks, is_someday_task),
        waiting: review_tasks(&tasks, is_waiting_task),
        overdue: review_tasks(&tasks, |task| {
            task.due.map(|due| due < today).unwrap_or(false)
        }),
        due_today: review_tasks(&tasks, |task| task.due == Some(today)),
        scheduled_today: review_tasks(&tasks, |task| task.scheduled == Some(today)),
        upcoming: review_tasks(&tasks, |task| {
            task.due
                .or(task.scheduled)
                .map(|date| date > today && date <= horizon_end)
                .unwrap_or(false)
        }),
        unscheduled: review_tasks(&tasks, |task| {
            task.due.is_none()
                && task.scheduled.is_none()
                && !is_inbox_task(task)
                && !is_waiting_task(task)
                && !is_idea_task(task)
                && !is_someday_task(task)
        }),
        stale: review_tasks(&tasks, |task| {
            task.date_modified
                .map(|date| date.date_naive() < stale_before)
                .unwrap_or(false)
        }),
    }
}

fn build_operating_model_report(
    mut tasks: Vec<Task>,
    projects: Vec<Project>,
    events: Vec<crate::CalendarEvent>,
) -> OperatingModelReport {
    let today = chrono::Local::now().date_naive();
    let stale_before = today - chrono::Duration::days(14);
    sort_review_tasks(&mut tasks);
    let review = build_review_report(tasks.clone(), 30, 14);
    let active_projects = projects
        .iter()
        .filter(|project| project.is_active() && !project.is_archived())
        .cloned()
        .collect::<Vec<_>>();
    let mut area_names = tasks
        .iter()
        .flat_map(|task| task.areas.iter().map(|area| area.0.clone()))
        .chain(projects.iter().filter_map(|project| project.area.clone()))
        .filter(|area| !area.trim().is_empty())
        .collect::<Vec<_>>();
    area_names.sort();
    area_names.dedup();
    if area_names.is_empty() {
        area_names.push("Unassigned".to_string());
    }

    let mut areas = area_names
        .iter()
        .map(|area| {
            let area_tasks = tasks
                .iter()
                .filter(|task| task_in_area(task, area))
                .collect::<Vec<_>>();
            OperatingAreaStatus {
                name: area.clone(),
                open_tasks: area_tasks
                    .iter()
                    .filter(|task| is_review_actionable(task))
                    .count() as u32,
                active_projects: projects
                    .iter()
                    .filter(|project| {
                        project.area.as_deref() == Some(area.as_str())
                            && project.is_active()
                            && !project.is_archived()
                    })
                    .count() as u32,
                overdue_tasks: area_tasks
                    .iter()
                    .filter(|task| task.due.map(|due| due < today).unwrap_or(false))
                    .count() as u32,
                due_today_tasks: area_tasks
                    .iter()
                    .filter(|task| task.due == Some(today))
                    .count() as u32,
                waiting_tasks: area_tasks
                    .iter()
                    .filter(|task| is_waiting_task(task))
                    .count() as u32,
                stale_tasks: area_tasks
                    .iter()
                    .filter(|task| {
                        is_review_actionable(task)
                            && task
                                .date_modified
                                .map(|date| date.date_naive() < stale_before)
                                .unwrap_or(false)
                    })
                    .count() as u32,
                routine_tasks: area_tasks
                    .iter()
                    .filter(|task| is_routine_task(task))
                    .count() as u32,
                habit_tasks: area_tasks.iter().filter(|task| is_habit_task(task)).count() as u32,
                goal_tasks: area_tasks.iter().filter(|task| is_goal_task(task)).count() as u32,
                next_action: area_tasks
                    .iter()
                    .filter(|task| is_review_actionable(task) && !is_waiting_task(task))
                    .next()
                    .map(|task| (*task).clone()),
            }
        })
        .collect::<Vec<_>>();
    areas.sort_by(|a, b| {
        b.overdue_tasks
            .cmp(&a.overdue_tasks)
            .then_with(|| b.due_today_tasks.cmp(&a.due_today_tasks))
            .then_with(|| b.open_tasks.cmp(&a.open_tasks))
            .then_with(|| a.name.cmp(&b.name))
    });

    let goals = tasks
        .iter()
        .filter(|task| is_goal_task(task))
        .map(|task| OperatingGoal {
            title: task.title.clone(),
            area: first_task_area(task),
            project: task.projects.first().map(|project| project.0.clone()),
            status: status_label(&task.status).to_string(),
            due: task.due.map(|date| date.to_string()),
            next_action: related_next_action(task, &tasks),
        })
        .collect::<Vec<_>>();
    let routines = tasks
        .iter()
        .filter(|task| is_routine_task(task) && !is_habit_task(task))
        .map(task_to_operating_routine)
        .collect::<Vec<_>>();
    let habits = tasks
        .iter()
        .filter(|task| is_habit_task(task))
        .map(task_to_operating_routine)
        .collect::<Vec<_>>();
    let active_timers = tasks
        .iter()
        .flat_map(|task| task.time_entries.iter())
        .filter(|entry| entry.is_running())
        .count() as u32;
    let upcoming_events = events
        .iter()
        .filter(|event| event.start.date_naive() >= today)
        .count() as u32;

    OperatingModelReport {
        generated_at: Utc::now(),
        today: today.to_string(),
        areas,
        goals,
        routines,
        habits,
        active_projects,
        inbox: review.inbox.clone(),
        open_tasks: tasks
            .iter()
            .filter(|task| is_review_actionable(task))
            .count() as u32,
        overdue_tasks: review.overdue.len() as u32,
        due_today_tasks: (review.due_today.len() + review.scheduled_today.len()) as u32,
        waiting_tasks: review.waiting.len() as u32,
        stale_tasks: review.stale.len() as u32,
        unscheduled_tasks: review.unscheduled.len() as u32,
        active_timers,
        upcoming_events,
        review,
    }
}

fn review_tasks(tasks: &[Task], predicate: impl Fn(&Task) -> bool) -> Vec<Task> {
    tasks
        .iter()
        .filter(|task| is_review_actionable(task) && predicate(task))
        .cloned()
        .collect()
}

fn sort_review_tasks(tasks: &mut [Task]) {
    tasks.sort_by(|a, b| {
        a.due
            .or(a.scheduled)
            .cmp(&b.due.or(b.scheduled))
            .then_with(|| b.priority.weight().cmp(&a.priority.weight()))
            .then_with(|| b.urgency_score().cmp(&a.urgency_score()))
            .then_with(|| a.title.cmp(&b.title))
    });
}

fn is_review_actionable(task: &Task) -> bool {
    !task.is_complete()
        && !matches!(task.status, Status::Cancelled | Status::Archived)
        && task.deleted_at.is_none()
}

fn is_waiting_task(task: &Task) -> bool {
    task.issue_type
        .as_deref()
        .map(|kind| matches!(kind, "waiting" | "waiting-on"))
        .unwrap_or(false)
        || matches!(task.status, Status::OnHold)
        || task
            .tags
            .iter()
            .any(|tag| matches!(tag.as_str(), "waiting" | "waiting-on"))
}

fn is_idea_task(task: &Task) -> bool {
    task.issue_type.as_deref() == Some("idea") || task.tags.iter().any(|tag| tag == "idea")
}

fn is_someday_task(task: &Task) -> bool {
    task.issue_type
        .as_deref()
        .map(|kind| matches!(kind, "someday" | "maybe"))
        .unwrap_or(false)
        || task
            .tags
            .iter()
            .any(|tag| matches!(tag.as_str(), "someday" | "maybe"))
}

fn is_goal_task(task: &Task) -> bool {
    task.issue_type
        .as_deref()
        .map(|kind| matches!(kind, "goal" | "objective" | "outcome"))
        .unwrap_or(false)
        || task
            .tags
            .iter()
            .any(|tag| matches!(tag.as_str(), "goal" | "objective" | "outcome"))
}

fn is_routine_task(task: &Task) -> bool {
    task.recurrence.is_some()
        || task
            .issue_type
            .as_deref()
            .map(|kind| matches!(kind, "routine" | "ritual" | "cadence"))
            .unwrap_or(false)
        || task
            .tags
            .iter()
            .any(|tag| matches!(tag.as_str(), "routine" | "ritual" | "cadence"))
}

fn is_habit_task(task: &Task) -> bool {
    task.issue_type.as_deref() == Some("habit") || task.tags.iter().any(|tag| tag == "habit")
}

fn first_task_area(task: &Task) -> Option<String> {
    task.areas.first().map(|area| area.0.clone())
}

fn task_in_area(task: &Task, area: &str) -> bool {
    if area == "Unassigned" {
        return task.areas.is_empty();
    }
    task.areas.iter().any(|candidate| candidate.0 == area)
}

fn task_to_operating_routine(task: &Task) -> OperatingRoutine {
    OperatingRoutine {
        title: task.title.clone(),
        area: first_task_area(task),
        kind: if is_habit_task(task) {
            "habit".to_string()
        } else {
            task.issue_type
                .clone()
                .unwrap_or_else(|| "routine".to_string())
        },
        recurrence: task.recurrence.clone(),
        due: task.due.map(|date| date.to_string()),
        scheduled: task.scheduled.map(|date| date.to_string()),
        status: status_label(&task.status).to_string(),
    }
}

fn related_next_action(goal: &Task, tasks: &[Task]) -> Option<Task> {
    goal.projects
        .first()
        .and_then(|project| {
            tasks
                .iter()
                .find(|task| {
                    task.title != goal.title
                        && is_review_actionable(task)
                        && !is_waiting_task(task)
                        && task.projects.iter().any(|candidate| candidate == project)
                })
                .cloned()
        })
        .or_else(|| {
            tasks
                .iter()
                .find(|task| {
                    task.title != goal.title
                        && is_review_actionable(task)
                        && !is_waiting_task(task)
                        && task.areas.iter().any(|area| goal.areas.contains(area))
                })
                .cloned()
        })
}

fn build_finance_report(
    time_entries: Vec<TimeEntryContext>,
    invoices: Vec<crate::invoice::Invoice>,
    today: NaiveDate,
) -> BusinessFinanceReport {
    let billable_entries = time_entries
        .into_iter()
        .filter(|entry| entry.entry.billable && !entry.entry.is_running())
        .collect::<Vec<_>>();
    let mut unbilled_entries = billable_entries
        .iter()
        .filter(|entry| entry.entry.invoiced_at.is_none())
        .cloned()
        .collect::<Vec<_>>();
    unbilled_entries.sort_by(|a, b| a.entry.start_time.cmp(&b.entry.start_time));
    let billable_minutes = billable_entries
        .iter()
        .map(|entry| entry.entry.duration_minutes())
        .sum::<u32>();
    let unbilled_minutes = unbilled_entries
        .iter()
        .map(|entry| entry.entry.duration_minutes())
        .sum::<u32>();
    let unbilled_cents = unbilled_entries
        .iter()
        .map(|entry| time_entry_cents(entry))
        .sum::<u64>();
    let invoiced_cents = invoices
        .iter()
        .map(|invoice| invoice.total_cents())
        .sum::<u64>();
    let paid_cents = invoices
        .iter()
        .map(|invoice| invoice.paid_cents())
        .sum::<u64>();
    let open_invoices = invoices
        .iter()
        .filter(|invoice| invoice.balance_cents() > 0 && invoice.cancelled_at.is_none())
        .cloned()
        .collect::<Vec<_>>();
    let open_invoice_cents = open_invoices
        .iter()
        .map(|invoice| invoice.balance_cents())
        .sum::<u64>();
    let overdue_invoice_cents = open_invoices
        .iter()
        .filter(|invoice| invoice.due_date < today)
        .map(|invoice| invoice.balance_cents())
        .sum::<u64>();
    let draft_invoices = invoices
        .iter()
        .filter(|invoice| matches!(invoice.status, crate::invoice::InvoiceStatus::Draft))
        .cloned()
        .collect::<Vec<_>>();
    let mut clients = std::collections::BTreeMap::<String, BusinessFinanceClientSummary>::new();
    for entry in &unbilled_entries {
        let client_name = entry
            .client_name
            .clone()
            .unwrap_or_else(|| "Unassigned".to_string());
        let summary =
            clients
                .entry(client_name.clone())
                .or_insert_with(|| BusinessFinanceClientSummary {
                    client_name,
                    ..Default::default()
                });
        summary.unbilled_minutes += entry.entry.duration_minutes();
        summary.unbilled_cents += time_entry_cents(entry);
    }
    for invoice in &open_invoices {
        let client_name = strip_wikilink_brackets(&invoice.client.0);
        let summary =
            clients
                .entry(client_name.clone())
                .or_insert_with(|| BusinessFinanceClientSummary {
                    client_name,
                    ..Default::default()
                });
        summary.open_invoice_cents += invoice.balance_cents();
        if invoice.due_date < today {
            summary.overdue_invoice_cents += invoice.balance_cents();
        }
    }

    BusinessFinanceReport {
        generated_at: Utc::now(),
        today: today.to_string(),
        billable_minutes,
        unbilled_minutes,
        unbilled_cents,
        invoiced_cents,
        paid_cents,
        open_invoice_cents,
        overdue_invoice_cents,
        clients: clients.into_values().collect(),
        aging: invoice_aging(&open_invoices, today),
        draft_invoices,
        open_invoices,
        unbilled_entries,
    }
}

fn time_entry_cents(entry: &TimeEntryContext) -> u64 {
    let minutes = entry.entry.duration_minutes() as u64;
    let rate = entry.effective_rate(None) as u64;
    ((minutes * rate) + 30) / 60
}

fn invoice_aging(
    invoices: &[crate::invoice::Invoice],
    today: NaiveDate,
) -> Vec<InvoiceAgingBucket> {
    let mut buckets = vec![
        InvoiceAgingBucket {
            name: "current".to_string(),
            ..Default::default()
        },
        InvoiceAgingBucket {
            name: "1-30".to_string(),
            ..Default::default()
        },
        InvoiceAgingBucket {
            name: "31-60".to_string(),
            ..Default::default()
        },
        InvoiceAgingBucket {
            name: "61-90".to_string(),
            ..Default::default()
        },
        InvoiceAgingBucket {
            name: "90+".to_string(),
            ..Default::default()
        },
    ];
    for invoice in invoices {
        let overdue_days = (today - invoice.due_date).num_days();
        let index = if overdue_days <= 0 {
            0
        } else if overdue_days <= 30 {
            1
        } else if overdue_days <= 60 {
            2
        } else if overdue_days <= 90 {
            3
        } else {
            4
        };
        buckets[index].invoice_count += 1;
        buckets[index].balance_cents += invoice.balance_cents();
    }
    buckets
}

fn build_sync_plan(config: Option<NextcloudRuntimeConfig>) -> SyncPlan {
    let configured = config.is_some();
    let mut warnings = Vec::new();
    if !configured {
        warnings.push(
            "Nextcloud is not configured; sync would be skipped until credentials are set"
                .to_string(),
        );
    }
    let (calendar, event_calendar, projects_path, deck_enabled, username) = config
        .map(|config| {
            (
                config.calendar,
                config
                    .event_calendar
                    .unwrap_or_else(|| "events".to_string()),
                config.projects_path,
                config.deck_enabled,
                config.username,
            )
        })
        .unwrap_or_else(|| {
            (
                "tasks".to_string(),
                "events".to_string(),
                "Projects/".to_string(),
                false,
                "unknown".to_string(),
            )
        });
    let mut items = vec![
        SyncPlanItem {
            provider: "caldav".to_string(),
            operation: "sync task calendar".to_string(),
            collection: calendar,
            direction: "bidirectional".to_string(),
            configured,
            destructive: false,
            detail: "Pull VTODO changes, merge conflicts, and push local task updates".to_string(),
        },
        SyncPlanItem {
            provider: "caldav-events".to_string(),
            operation: "sync event calendar".to_string(),
            collection: event_calendar,
            direction: "pull".to_string(),
            configured,
            destructive: false,
            detail: "Pull VEVENTs into first-class calendar events".to_string(),
        },
        SyncPlanItem {
            provider: "carddav".to_string(),
            operation: "sync contacts".to_string(),
            collection: "contacts".to_string(),
            direction: "pull".to_string(),
            configured,
            destructive: false,
            detail: "Read contacts for people and organization context".to_string(),
        },
        SyncPlanItem {
            provider: "webdav".to_string(),
            operation: "sync project files".to_string(),
            collection: projects_path,
            direction: "pull".to_string(),
            configured,
            destructive: false,
            detail: "Read project folders and markdown project metadata".to_string(),
        },
        SyncPlanItem {
            provider: "nextcloud-talk".to_string(),
            operation: "read conversations".to_string(),
            collection: username,
            direction: "pull/read".to_string(),
            configured,
            destructive: false,
            detail: "Read conversation lists and recent messages for relationship context"
                .to_string(),
        },
    ];
    items.push(SyncPlanItem {
        provider: "deck".to_string(),
        operation: "sync boards and cards".to_string(),
        collection: "boards".to_string(),
        direction: "bidirectional".to_string(),
        configured: configured && deck_enabled,
        destructive: false,
        detail: "Upsert cards by external id/title and move cards between stacks".to_string(),
    });
    SyncPlan {
        generated_at: Utc::now().to_rfc3339(),
        safe_to_run: configured,
        items,
        warnings,
    }
}

fn person_from_carddav_object(object: CardDavObject) -> Option<Person> {
    let contact = object.contact?;
    let display_name = contact
        .full_name
        .clone()
        .or_else(|| {
            match (
                contact.given_name.as_deref(),
                contact.family_name.as_deref(),
            ) {
                (Some(given), Some(family)) => Some(format!("{given} {family}")),
                (Some(given), None) => Some(given.to_string()),
                (None, Some(family)) => Some(family.to_string()),
                _ => None,
            }
        })
        .or_else(|| contact.emails.first().cloned())
        .or_else(|| contact.uid.clone())?;
    let mut contact_methods = Vec::new();
    for (index, email) in contact.emails.iter().enumerate() {
        contact_methods.push(ContactMethod {
            kind: "email".to_string(),
            value: email.clone(),
            primary: index == 0,
            ..Default::default()
        });
    }
    for (index, phone) in contact.phones.iter().enumerate() {
        contact_methods.push(ContactMethod {
            kind: "phone".to_string(),
            value: phone.clone(),
            primary: index == 0 && contact.emails.is_empty(),
            ..Default::default()
        });
    }
    for url in &contact.urls {
        contact_methods.push(ContactMethod {
            kind: "url".to_string(),
            value: url.clone(),
            ..Default::default()
        });
    }
    Some(Person {
        id: contact.uid.clone(),
        display_name,
        given_name: contact.given_name,
        family_name: contact.family_name,
        organization: contact.organization,
        title: contact.title,
        contact_methods,
        provider_refs: vec![ProviderRef {
            provider: "carddav".to_string(),
            collection: object
                .href
                .trim_end_matches('/')
                .rsplit_once('/')
                .map(|(collection, _)| collection.trim_end_matches('/').to_string()),
            href: Some(object.href),
            etag: object.etag,
            uid: contact.uid,
            ..Default::default()
        }],
        notes: contact.note,
        ..Default::default()
    })
}

fn organizations_from_people(people: &[Person]) -> Vec<OrganizationRecord> {
    let mut organizations: Vec<OrganizationRecord> = Vec::new();
    for person in people {
        let Some(name) = person
            .organization
            .as_deref()
            .filter(|name| !name.is_empty())
        else {
            continue;
        };
        if let Some(org) = organizations.iter_mut().find(|org| org.name == name) {
            push_unique(&mut org.people, person.display_name.clone());
        } else {
            organizations.push(OrganizationRecord {
                id: Some(slug_id(name)),
                name: name.to_string(),
                people: vec![person.display_name.clone()],
                ..Default::default()
            });
        }
    }
    organizations.sort_by(|a, b| a.name.cmp(&b.name));
    organizations
}

fn person_matches_reference(person: &Person, reference: &str) -> bool {
    let needle = reference.to_ascii_lowercase();
    person
        .id
        .as_deref()
        .map(|id| id.eq_ignore_ascii_case(reference))
        .unwrap_or(false)
        || person.display_name.eq_ignore_ascii_case(reference)
        || person
            .contact_methods
            .iter()
            .any(|method| method.value.eq_ignore_ascii_case(reference))
        || person.display_name.to_ascii_lowercase().contains(&needle)
}

fn organization_matches_reference(organization: &OrganizationRecord, reference: &str) -> bool {
    let needle = reference.to_ascii_lowercase();
    organization
        .id
        .as_deref()
        .map(|id| id.eq_ignore_ascii_case(reference))
        .unwrap_or(false)
        || organization.name.eq_ignore_ascii_case(reference)
        || organization.name.to_ascii_lowercase().contains(&needle)
}

fn person_context_tokens(person: &Person) -> Vec<String> {
    let mut tokens = vec![person.display_name.clone()];
    if let Some(given) = &person.given_name {
        tokens.push(given.clone());
    }
    if let Some(family) = &person.family_name {
        tokens.push(family.clone());
    }
    if let Some(org) = &person.organization {
        tokens.push(org.clone());
    }
    for method in &person.contact_methods {
        tokens.push(method.value.clone());
    }
    normalize_context_tokens(tokens)
}

fn organization_context_tokens(
    organization: &OrganizationRecord,
    people: &[Person],
) -> Vec<String> {
    let mut tokens = vec![organization.name.clone()];
    for person in people {
        tokens.extend(person_context_tokens(person));
    }
    normalize_context_tokens(tokens)
}

fn normalize_context_tokens(tokens: Vec<String>) -> Vec<String> {
    let mut normalized = Vec::new();
    for token in tokens {
        let token = token.trim();
        if token.len() >= 3 {
            push_unique(&mut normalized, token.to_ascii_lowercase());
        }
    }
    normalized
}

fn task_matches_tokens(task: &Task, tokens: &[String]) -> bool {
    let haystack = format!(
        "{}\n{}\n{}\n{}\n{}",
        task.title,
        task.body,
        task.assignee.clone().unwrap_or_default(),
        task.tags.join("\n"),
        task.contexts.join("\n")
    )
    .to_ascii_lowercase();
    tokens.iter().any(|token| haystack.contains(token))
}

fn text_matches_tokens(text: &str, tokens: &[String]) -> bool {
    let haystack = text.to_ascii_lowercase();
    tokens.iter().any(|token| haystack.contains(token))
}

fn project_matches_tokens(project: &Project, tokens: &[String]) -> bool {
    let haystack = format!(
        "{}\n{}\n{}\n{}\n{}",
        project.title,
        project.description.clone().unwrap_or_default(),
        project.organization.clone().unwrap_or_default(),
        project.team.join("\n"),
        project.tags.join("\n")
    )
    .to_ascii_lowercase();
    tokens.iter().any(|token| haystack.contains(token))
}

fn event_matches_tokens(event: &crate::CalendarEvent, tokens: &[String]) -> bool {
    let haystack = format!(
        "{}\n{}\n{}\n{}",
        event.title,
        event.description.clone().unwrap_or_default(),
        event.location.clone().unwrap_or_default(),
        event.attendees.join("\n")
    )
    .to_ascii_lowercase();
    tokens.iter().any(|token| haystack.contains(token))
}

fn communication_refs_for_tokens(
    tasks: &[Task],
    events: &[crate::CalendarEvent],
    tokens: &[String],
) -> Vec<CommunicationRef> {
    let mut refs = Vec::new();
    for task in tasks
        .iter()
        .filter(|task| task_matches_tokens(task, tokens))
    {
        for email in &task.emails {
            refs.push(CommunicationRef {
                kind: "email".to_string(),
                external_id: email.message_id.clone(),
                summary: Some(email.subject.clone()),
                occurred_at: Some(email.date),
                provider: Some("mail".to_string()),
            });
        }
    }
    for event in events
        .iter()
        .filter(|event| event_matches_tokens(event, tokens))
    {
        refs.push(CommunicationRef {
            kind: "calendar".to_string(),
            external_id: event.id.clone().unwrap_or_else(|| event.title.clone()),
            summary: Some(event.title.clone()),
            occurred_at: Some(event.start),
            provider: Some(
                event
                    .external_source
                    .clone()
                    .unwrap_or_else(|| "calendar".to_string()),
            ),
        });
    }
    refs
}

fn slug_id(value: &str) -> String {
    value
        .chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() {
                ch.to_ascii_lowercase()
            } else {
                '-'
            }
        })
        .collect::<String>()
        .split('-')
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join("-")
}

fn task_matches_reference(task: &Task, reference: &str) -> bool {
    task.matches_reference(reference)
}

fn push_unique<T: PartialEq>(items: &mut Vec<T>, item: T) {
    if !items.contains(&item) {
        items.push(item);
    }
}

fn parse_optional_naive_date(input: &str, field: &str) -> Result<Option<NaiveDate>, VaultError> {
    if input.is_empty() || input == "clear" {
        Ok(None)
    } else {
        input
            .parse::<NaiveDate>()
            .map(Some)
            .map_err(|e| VaultError::ParseError(format!("invalid {field}: {e}")))
    }
}

fn parse_task_status(status: &str) -> Option<Status> {
    match status.to_ascii_lowercase().as_str() {
        "none" => Some(Status::None),
        "open" => Some(Status::Open),
        "in-progress" | "in_progress" | "doing" => Some(Status::InProgress),
        "on-hold" | "on_hold" | "hold" | "waiting" => Some(Status::OnHold),
        "planned" => Some(Status::Planned),
        "done" | "complete" | "completed" => Some(Status::Done),
        "cancelled" | "canceled" => Some(Status::Cancelled),
        "archived" => Some(Status::Archived),
        _ => None,
    }
}

fn status_label(status: &Status) -> &'static str {
    match status {
        Status::None => "none",
        Status::Open => "open",
        Status::InProgress => "in-progress",
        Status::OnHold => "on-hold",
        Status::Planned => "planned",
        Status::Done => "done",
        Status::Cancelled => "cancelled",
        Status::Archived => "archived",
    }
}

fn priority_label(priority: &Priority) -> &'static str {
    match priority {
        Priority::None => "none",
        Priority::Low => "low",
        Priority::Normal => "normal",
        Priority::High => "high",
        Priority::Urgent => "urgent",
    }
}

// ── Vox service trait implementations ────────────────────────────────────────

impl crate::service::TaskService for VaultServiceImpl {
    async fn list_tasks(&self) -> Vec<Task> {
        self.list_tasks().await
    }
    async fn execute_query(&self, query: Query) -> Vec<Task> {
        self.execute_query(query).await
    }
    async fn urgency_score(&self, task: Task) -> i32 {
        self.urgency_score(task).await
    }
    async fn create_task(&self, task: Task) -> Result<Task, VaultError> {
        self.create_task(task).await
    }
    async fn update_task(&self, task: Task) -> Result<Task, VaultError> {
        self.update_task(task).await
    }
    async fn complete_task(&self, title: String) -> Result<Task, VaultError> {
        self.complete_task(title).await
    }
    async fn delete_task(&self, title: String) -> Result<(), VaultError> {
        self.delete_task(title).await
    }
    async fn search_tasks(&self, query: String) -> Vec<Task> {
        self.search_tasks(query).await
    }
    async fn tasks_for_user(&self, username: String) -> Vec<Task> {
        self.tasks_for_user(username).await
    }
}

impl crate::service::InboxService for VaultServiceImpl {
    async fn capture(&self, request: InboxCaptureRequest) -> Result<InboxItem, VaultError> {
        self.capture_inbox(request).await
    }

    async fn list_inbox(&self) -> Vec<InboxItem> {
        self.list_inbox_items().await
    }

    async fn promote(&self, request: InboxPromoteRequest) -> Result<InboxItem, VaultError> {
        self.promote_inbox(request).await
    }

    async fn daily_review(&self) -> ReviewReport {
        self.daily_review_report().await
    }

    async fn weekly_review(&self) -> ReviewReport {
        self.weekly_review_report().await
    }

    async fn monthly_review(&self) -> ReviewReport {
        self.monthly_review_report().await
    }

    async fn project_review(&self, project_title: String) -> ReviewReport {
        self.project_review_report(project_title).await
    }
}

impl crate::service::OperatingService for VaultServiceImpl {
    async fn operating_model(&self) -> OperatingModelReport {
        self.operating_model_report().await
    }
}

impl crate::service::ProjectService for VaultServiceImpl {
    async fn list_projects(&self) -> Vec<Project> {
        self.list_projects().await
    }
    async fn update_project(
        &self,
        title: String,
        patch: ProjectPatch,
        actor: Option<String>,
    ) -> Result<Project, VaultError> {
        self.update_project_as(&title, patch, actor.as_deref())
            .await
    }
    async fn project_stats(&self, project_title: String) -> ProjectStats {
        self.project_stats(project_title).await
    }
    async fn project_dashboard(&self) -> Vec<ProjectDashboardEntry> {
        VaultServiceImpl::project_dashboard(self).await
    }
    async fn next_task(&self, project_title: String) -> Option<Task> {
        self.next_task(project_title).await
    }
    async fn tasks_for_project(&self, project_title: String) -> Vec<Task> {
        self.tasks_for_project(project_title).await
    }

    async fn project_context(
        &self,
        project_title: String,
        include_files: bool,
        depth: String,
    ) -> Result<Option<ProjectKnowledgeContext>, VaultError> {
        self.project_knowledge_context(project_title, include_files, depth)
            .await
    }
}

impl crate::service::TimeService for VaultServiceImpl {
    async fn start_timer(
        &self,
        request: TimeStartRequest,
    ) -> Result<crate::task::TimeEntry, VaultError> {
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

    async fn log_time(
        &self,
        request: TimeLogRequest,
    ) -> Result<crate::task::TimeEntry, VaultError> {
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

    async fn delete_time_entry(
        &self,
        entry_id: String,
        actor: Option<String>,
    ) -> Result<(), VaultError> {
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

impl crate::service::PeopleService for VaultServiceImpl {
    async fn list_people(&self, addressbook: Option<String>) -> Result<Vec<Person>, VaultError> {
        self.list_people_from_carddav(addressbook).await
    }

    async fn list_organizations(
        &self,
        addressbook: Option<String>,
    ) -> Result<Vec<OrganizationRecord>, VaultError> {
        self.list_organizations_from_carddav(addressbook).await
    }

    async fn person_context(
        &self,
        reference: String,
        addressbook: Option<String>,
    ) -> Result<Option<PersonContext>, VaultError> {
        self.person_context_from_carddav(reference, addressbook)
            .await
    }

    async fn organization_context(
        &self,
        reference: String,
        addressbook: Option<String>,
    ) -> Result<Option<OrganizationContext>, VaultError> {
        self.organization_context_from_carddav(reference, addressbook)
            .await
    }

    async fn detect_person_conflict(
        &self,
        local: Person,
        remote: Person,
    ) -> Result<Option<ProviderConflict>, VaultError> {
        Ok(person_provider_conflicts(&local, &remote))
    }

    async fn detect_organization_conflict(
        &self,
        local: OrganizationRecord,
        remote: OrganizationRecord,
    ) -> Result<Option<ProviderConflict>, VaultError> {
        Ok(organization_provider_conflicts(&local, &remote))
    }
}

impl crate::service::ExpenseService for VaultServiceImpl {
    async fn create_expense(&self, request: ExpenseCreateRequest) -> Result<Expense, VaultError> {
        VaultServiceImpl::create_expense(self, request).await
    }

    async fn list_expenses(&self, filter: ExpenseFilter) -> Vec<Expense> {
        VaultServiceImpl::list_expenses(self, filter).await
    }

    async fn get_expense(&self, expense_id: String) -> Option<Expense> {
        VaultServiceImpl::get_expense(self, &expense_id).await
    }

    async fn update_expense(
        &self,
        expense_id: String,
        patch: ExpensePatch,
        actor: Option<String>,
    ) -> Result<Expense, VaultError> {
        VaultServiceImpl::update_expense(self, &expense_id, patch, actor.as_deref()).await
    }

    async fn delete_expense(&self, expense_id: String) -> Result<(), VaultError> {
        VaultServiceImpl::delete_expense(self, &expense_id).await
    }

    async fn expense_report(&self, filter: ExpenseFilter) -> ExpenseReport {
        VaultServiceImpl::expense_report(self, filter).await
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

    async fn finance_report(&self) -> BusinessFinanceReport {
        VaultServiceImpl::finance_report(self).await
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
    async fn recent_activity(
        &self,
        limit: u32,
    ) -> Result<Vec<crate::index::ChangeRow>, VaultError> {
        VaultServiceImpl::recent_activity(self, limit).await
    }

    async fn list_sync_states(&self) -> Result<Vec<ProviderSyncState>, VaultError> {
        VaultServiceImpl::list_provider_sync_states(self).await
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

impl crate::service::ConversationService for VaultServiceImpl {
    async fn list_conversations(&self) -> Result<Vec<ChannelConversation>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud Talk channel is not configured".into())
        })?;
        nextcloud_talk_provider(&config).list_conversations().await
    }

    async fn recent_messages(
        &self,
        conversation_id: String,
        limit: u32,
    ) -> Result<Vec<ChannelMessage>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud Talk channel is not configured".into())
        })?;
        let provider = nextcloud_talk_provider(&config);
        let messages =
            CommunicationChannelProvider::recent_messages(&provider, &conversation_id, limit)
                .await?;
        self.record_provider_sync_state(
            "nextcloud-talk",
            Some(&config.username),
            &conversation_id,
            None,
            Some(
                messages
                    .last()
                    .map(|message| message.id.clone())
                    .unwrap_or_default(),
            ),
            None,
            None,
        );
        Ok(messages)
    }

    async fn send_message(
        &self,
        request: ChannelSendMessageRequest,
    ) -> Result<ChannelMessage, VaultError> {
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud Talk channel is not configured".into())
        })?;
        let provider = nextcloud_talk_provider(&config);
        CommunicationChannelProvider::send_message(&provider, request).await
    }
}

impl crate::service::MailService for VaultServiceImpl {
    async fn list_accounts(&self) -> Result<Vec<crate::provider::MailAccount>, VaultError> {
        nextcloud_mail_client()?.list_accounts().await
    }

    async fn list_mailboxes(
        &self,
        account_id: i64,
    ) -> Result<Vec<crate::provider::Mailbox>, VaultError> {
        nextcloud_mail_client()?.list_mailboxes(account_id).await
    }

    async fn list_messages(
        &self,
        request: MailListMessagesRequest,
    ) -> Result<Vec<crate::provider::MailMessage>, VaultError> {
        nextcloud_mail_client()?
            .list_messages(
                request.mailbox_id,
                request.filter.as_deref(),
                request.limit,
                request.cursor.as_deref(),
            )
            .await
    }

    async fn get_message(&self, id: i64) -> Result<crate::provider::MailMessageDetail, VaultError> {
        nextcloud_mail_client()?.get_message(id).await
    }

    async fn get_body(&self, id: i64) -> Result<String, VaultError> {
        nextcloud_mail_client()?.get_body(id).await
    }

    async fn create_mailbox(
        &self,
        request: MailCreateMailboxRequest,
    ) -> Result<crate::provider::Mailbox, VaultError> {
        nextcloud_mail_client()?
            .create_mailbox(request.account_id, &request.name)
            .await
    }

    async fn delete_mailbox(&self, mailbox_id: i64) -> Result<(), VaultError> {
        nextcloud_mail_client()?.delete_mailbox(mailbox_id).await
    }

    async fn move_message(&self, request: MailMoveMessageRequest) -> Result<(), VaultError> {
        nextcloud_mail_client()?
            .move_message(request.message_id, request.dest_folder_id)
            .await
    }

    async fn list_tags(&self) -> Result<Vec<crate::provider::MailTag>, VaultError> {
        nextcloud_mail_client()?.list_tags().await
    }

    async fn create_tag(
        &self,
        request: MailCreateTagRequest,
    ) -> Result<crate::provider::MailTag, VaultError> {
        nextcloud_mail_client()?
            .create_tag(&request.display_name, &request.color)
            .await
    }

    async fn delete_tag(&self, request: MailDeleteTagRequest) -> Result<(), VaultError> {
        nextcloud_mail_client()?
            .delete_tag(request.account_id, request.tag_id)
            .await
    }

    async fn set_tag(&self, request: MailMessageTagRequest) -> Result<(), VaultError> {
        nextcloud_mail_client()?
            .set_tag(request.message_id, &request.imap_label)
            .await
    }

    async fn remove_tag(&self, request: MailMessageTagRequest) -> Result<(), VaultError> {
        nextcloud_mail_client()?
            .remove_tag(request.message_id, &request.imap_label)
            .await
    }

    async fn link_email(&self, request: EmailLinkRequest) -> Result<EmailLinkResponse, VaultError> {
        match request.target_type.as_str() {
            "task" => {
                let task = VaultServiceImpl::link_email_to_task(
                    self,
                    &request.reference,
                    request.email,
                    request.actor.as_deref(),
                )
                .await?;
                Ok(EmailLinkResponse {
                    target_type: "task".into(),
                    title: task.title,
                    email_count: task.emails.len() as u32,
                })
            }
            "project" => {
                let project = VaultServiceImpl::link_email_to_project(
                    self,
                    &request.reference,
                    request.email,
                    request.actor.as_deref(),
                )
                .await?;
                Ok(EmailLinkResponse {
                    target_type: "project".into(),
                    title: project.title,
                    email_count: project.emails.len() as u32,
                })
            }
            other => Err(VaultError::ParseError(format!(
                "target_type must be 'task' or 'project', got '{other}'"
            ))),
        }
    }

    async fn unlink_email(&self, request: EmailUnlinkRequest) -> Result<(), VaultError> {
        match request.target_type.as_str() {
            "task" => {
                VaultServiceImpl::unlink_email_from_task(
                    self,
                    &request.reference,
                    &request.message_id,
                    request.actor.as_deref(),
                )
                .await?;
                Ok(())
            }
            "project" => {
                VaultServiceImpl::unlink_email_from_project(
                    self,
                    &request.reference,
                    &request.message_id,
                    request.actor.as_deref(),
                )
                .await?;
                Ok(())
            }
            other => Err(VaultError::ParseError(format!(
                "target_type must be 'task' or 'project', got '{other}'"
            ))),
        }
    }

    async fn list_linked_emails(
        &self,
        request: EmailListRequest,
    ) -> Result<Vec<crate::email::EmailRef>, VaultError> {
        match request.target_type.as_str() {
            "task" => VaultServiceImpl::emails_for_task(self, &request.reference)
                .await
                .ok_or_else(|| VaultError::NotFound(request.reference)),
            "project" => VaultServiceImpl::emails_for_project(self, &request.reference)
                .await
                .ok_or_else(|| VaultError::NotFound(request.reference)),
            other => Err(VaultError::ParseError(format!(
                "target_type must be 'task' or 'project', got '{other}'"
            ))),
        }
    }

    async fn linked_message_ids(&self) -> Vec<String> {
        VaultServiceImpl::linked_message_ids(self)
            .await
            .into_iter()
            .collect()
    }
}

impl crate::service::SystemService for VaultServiceImpl {
    async fn capabilities(&self) -> SystemCapabilities {
        let nextcloud = NextcloudRuntimeConfig::load().ok().flatten();
        let index_available = self
            .index
            .lock()
            .ok()
            .and_then(|guard| guard.as_ref().map(|_| ()))
            .is_some();
        SystemCapabilities {
            package: "task-server".into(),
            version: env!("CARGO_PKG_VERSION").into(),
            protocol_version: 1,
            min_cli_version: "0.1.0".into(),
            min_server_version: "0.1.0".into(),
            services: vec![
                "TaskRepo".into(),
                "ProjectRepo".into(),
                "TaskService".into(),
                "InboxService".into(),
                "ProjectService".into(),
                "TimeService".into(),
                "ClientService".into(),
                "PeopleService".into(),
                "ConversationService".into(),
                "OperatingService".into(),
                "InvoiceService".into(),
                "ActivityService".into(),
                "MailService".into(),
                "CalendarService".into(),
                "FileService".into(),
                "SystemService".into(),
            ],
            features: vec![
                "inbox-capture".into(),
                "task-tracking".into(),
                "time-tracking".into(),
                "calendar-events".into(),
                "caldav".into(),
                "webdav-files".into(),
                "nextcloud-mail".into(),
                "deck-sync".into(),
                "invoicing".into(),
                "activity-log".into(),
                "conflict-log".into(),
            ],
            nextcloud: match nextcloud {
                Some(config) => NextcloudCapability {
                    configured: true,
                    url: Some(config.url),
                    username: Some(config.username),
                    projects_path: Some(config.projects_path),
                    task_calendar: Some(config.calendar),
                    event_calendar: config.event_calendar,
                    deck_enabled: config.deck_enabled,
                },
                None => NextcloudCapability {
                    configured: false,
                    ..Default::default()
                },
            },
            vault: VaultCapability {
                root: self.root.display().to_string(),
                exists: self.root.exists(),
                index_available,
            },
        }
    }

    async fn health(&self, deep: bool) -> SystemHealth {
        let mut checks = Vec::new();
        let index_available = self
            .index
            .lock()
            .ok()
            .and_then(|guard| guard.as_ref().map(|_| ()))
            .is_some();
        checks.push(health_check(
            "vault",
            if self.root.exists() {
                "VAULT_OK"
            } else {
                "VAULT_ROOT_MISSING"
            },
            true,
            self.root.exists(),
            if self.root.exists() {
                format!("vault root exists at {}", self.root.display())
            } else {
                format!("vault root is missing at {}", self.root.display())
            },
            Some("Set TASK_VAULT to an existing vault root or start task-server with a valid vault path."),
        ));
        checks.push(health_check_with_severity(
            "sqlite-index",
            if index_available {
                "INDEX_OK"
            } else {
                "INDEX_UNAVAILABLE"
            },
            true,
            index_available,
            if index_available { "ok" } else { "warning" },
            if index_available {
                "SQLite index is available".into()
            } else {
                "SQLite index is unavailable; queries will scan files".into()
            },
            Some(
                "File-scan mode is usable but slower. Run `task index rebuild` after verifying the vault path is writable, or restart task-server to reopen the index.",
            ),
        ));

        let nextcloud = match NextcloudRuntimeConfig::load() {
            Ok(config) => config,
            Err(err) => {
                checks.push(health_check(
                    "nextcloud-config",
                    "NEXTCLOUD_CONFIG_ERROR",
                    true,
                    false,
                    format!("Nextcloud config error: {err}"),
                    Some("Fix NEXTCLOUD_* environment variables or ~/.config/task/nextcloud.toml."),
                ));
                return system_health(deep, checks);
            }
        };

        let Some(config) = nextcloud else {
            checks.push(health_check(
                "nextcloud-config",
                "NEXTCLOUD_NOT_CONFIGURED",
                false,
                false,
                "Nextcloud is not configured".into(),
                Some(
                    "Set NEXTCLOUD_URL and NEXTCLOUD_PASSWORD, or configure TASK_NEXTCLOUD_CONFIG.",
                ),
            ));
            return system_health(deep, checks);
        };

        checks.push(health_check(
            "nextcloud-config",
            "NEXTCLOUD_CONFIGURED",
            true,
            true,
            format!(
                "Nextcloud configured for {} at {}",
                config.username, config.url
            ),
            None,
        ));

        if !deep {
            checks.push(health_check(
                "provider-live-checks",
                "DEEP_CHECKS_SKIPPED",
                false,
                true,
                "live WebDAV/CalDAV/Mail/Deck checks skipped".into(),
                Some("Run task doctor --deep to verify provider reachability."),
            ));
            return system_health(deep, checks);
        }

        match nextcloud_webdav_provider(&config).stat("").await {
            Ok(Some(_)) => checks.push(health_check(
                "webdav",
                "WEBDAV_OK",
                true,
                true,
                format!("projects path '{}' is reachable", config.projects_path),
                None,
            )),
            Ok(None) => checks.push(health_check(
                "webdav",
                "WEBDAV_PROJECTS_PATH_MISSING",
                true,
                false,
                format!("projects path '{}' was not found", config.projects_path),
                Some("Create the configured Nextcloud projects path or update NEXTCLOUD_PROJECTS_PATH."),
            )),
            Err(err) => checks.push(health_check(
                "webdav",
                "WEBDAV_UNREACHABLE",
                true,
                false,
                format!("WebDAV check failed: {err}"),
                Some("Verify Nextcloud URL, credentials, network access, and file permissions."),
            )),
        }

        match VaultServiceImpl::discover_caldav(self).await {
            Ok(discovery) => checks.push(health_check(
                "caldav",
                "CALDAV_OK",
                true,
                true,
                format!("discovered {} calendar(s)", discovery.calendars.len()),
                None,
            )),
            Err(err) => checks.push(health_check(
                "caldav",
                "CALDAV_DISCOVERY_FAILED",
                true,
                false,
                format!("CalDAV check failed: {err}"),
                Some("Verify calendar names and CalDAV permissions for the configured Nextcloud user."),
            )),
        }

        match nextcloud_mail_client() {
            Ok(client) => match client.list_accounts().await {
                Ok(accounts) => checks.push(health_check(
                    "mail",
                    "MAIL_OK",
                    true,
                    true,
                    format!("found {} mail account(s)", accounts.len()),
                    None,
                )),
                Err(err) => checks.push(health_check(
                    "mail",
                    "MAIL_ACCOUNTS_FAILED",
                    true,
                    false,
                    format!("Mail check failed: {err}"),
                    Some(
                        "Verify the Nextcloud Mail app is enabled and the user has mail accounts.",
                    ),
                )),
            },
            Err(err) => checks.push(health_check(
                "mail",
                "MAIL_CONFIG_ERROR",
                true,
                false,
                format!("Mail check failed: {err}"),
                Some("Verify Nextcloud Mail shares the same configured Nextcloud credentials."),
            )),
        }

        if config.deck_enabled {
            match VaultServiceImpl::list_remote_deck_boards(self).await {
                Ok(boards) => checks.push(health_check(
                    "deck",
                    "DECK_OK",
                    true,
                    true,
                    format!("found {} Deck board(s)", boards.len()),
                    None,
                )),
                Err(err) => checks.push(health_check(
                    "deck",
                    "DECK_UNREACHABLE",
                    true,
                    false,
                    format!("Deck check failed: {err}"),
                    Some("Verify the Deck app is enabled and accessible to the configured user."),
                )),
            }
        } else {
            checks.push(health_check(
                "deck",
                "DECK_DISABLED",
                false,
                true,
                "Deck checks disabled by configuration".into(),
                Some("Set NEXTCLOUD_DECK_ENABLED=true to enable Deck checks."),
            ));
        }

        system_health(deep, checks)
    }
}

impl crate::service::CalendarService for VaultServiceImpl {
    async fn tasks_due_by(&self, date: String) -> Vec<Task> {
        self.tasks_due_by(date).await
    }

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

    async fn trigger_sync(&self) -> Result<SyncStats, VaultError> {
        self.trigger_sync().await
    }
    async fn sync_status(&self) -> Option<SyncStats> {
        self.sync_status().await
    }
    async fn sync_plan(&self) -> SyncPlan {
        self.sync_plan().await
    }
    async fn discover_caldav(&self) -> Result<CalDavDiscovery, VaultError> {
        VaultServiceImpl::discover_caldav(self).await
    }
    async fn discover_carddav(&self) -> Result<CardDavDiscovery, VaultError> {
        VaultServiceImpl::discover_carddav(self).await
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
    async fn addressbook_multiget(
        &self,
        request: CardDavMultigetRequest,
    ) -> Result<Vec<CardDavObject>, VaultError> {
        VaultServiceImpl::addressbook_multiget(self, request).await
    }
    async fn addressbook_sync_collection(
        &self,
        request: CardDavSyncCollectionRequest,
    ) -> Result<CardDavSyncCollectionResponse, VaultError> {
        VaultServiceImpl::addressbook_sync_collection(self, request).await
    }
    async fn put_calendar_object(&self, request: CalDavPutObjectRequest) -> Result<(), VaultError> {
        VaultServiceImpl::put_calendar_object(self, request).await
    }
    async fn delete_calendar_object(
        &self,
        request: CalDavDeleteObjectRequest,
    ) -> Result<(), VaultError> {
        VaultServiceImpl::delete_calendar_object(self, request).await
    }
    async fn put_addressbook_object(
        &self,
        request: CardDavPutObjectRequest,
    ) -> Result<(), VaultError> {
        VaultServiceImpl::put_addressbook_object(self, request).await
    }
    async fn delete_addressbook_object(
        &self,
        request: CardDavDeleteObjectRequest,
    ) -> Result<(), VaultError> {
        VaultServiceImpl::delete_addressbook_object(self, request).await
    }
    async fn send_calendar_schedule(
        &self,
        request: CalDavScheduleRequest,
    ) -> Result<CalDavScheduleResponse, VaultError> {
        VaultServiceImpl::send_calendar_schedule(self, request).await
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
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud file provider is not configured".into())
        })?;
        let provider = nextcloud_webdav_provider(&config);
        provider
            .list(&path, if depth.is_empty() { "1" } else { &depth })
            .await
            .map(|entries| entries.into_iter().map(file_entry_from_webdav).collect())
    }

    async fn stat_file(&self, path: String) -> Result<Option<FileEntry>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud file provider is not configured".into())
        })?;
        let provider = nextcloud_webdav_provider(&config);
        provider
            .stat(&path)
            .await
            .map(|entry| entry.map(file_entry_from_webdav))
    }

    async fn read_file(&self, path: String) -> Result<Option<FileReadResponse>, VaultError> {
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud file provider is not configured".into())
        })?;
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
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud file provider is not configured".into())
        })?;
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
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud file provider is not configured".into())
        })?;
        nextcloud_webdav_provider(&config).create_dir(&path).await
    }

    async fn delete_file(&self, path: String) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud file provider is not configured".into())
        })?;
        nextcloud_webdav_provider(&config).remove(&path).await
    }

    async fn copy_file(&self, request: FileCopyMoveRequest) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud file provider is not configured".into())
        })?;
        nextcloud_webdav_provider(&config)
            .copy(
                &request.from,
                &request.to,
                request.overwrite,
                Some("infinity"),
            )
            .await
    }

    async fn move_file(&self, request: FileCopyMoveRequest) -> Result<(), VaultError> {
        let config = NextcloudRuntimeConfig::load()?.ok_or_else(|| {
            VaultError::IoError("Nextcloud file provider is not configured".into())
        })?;
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

fn project_file_summary_from_webdav(entry: crate::provider::WebDavEntry) -> ProjectFileSummary {
    let role = classify_project_file(&entry.path, &entry.name, &entry.content_type);
    ProjectFileSummary {
        path: entry.path,
        name: entry.name,
        kind: match entry.kind {
            crate::provider::WebDavResourceKind::File => "file".to_string(),
            crate::provider::WebDavResourceKind::Collection => "directory".to_string(),
        },
        role,
        content_type: entry.content_type,
        content_length: entry.content_length,
        last_modified: entry.last_modified,
    }
}

fn project_storage_path(project: &Project) -> String {
    project
        .dev_path
        .as_deref()
        .filter(|path| !path.trim().is_empty() && !path.starts_with('~') && !path.starts_with('/'))
        .map(|path| path.trim_matches('/').to_string())
        .unwrap_or_else(|| project.title.trim_matches('/').to_string())
}

fn classify_project_file(path: &str, name: &str, content_type: &Option<String>) -> String {
    let haystack = format!(
        "{} {}",
        path.to_ascii_lowercase(),
        name.to_ascii_lowercase()
    );
    if haystack.contains("decision") || haystack.contains("adr") {
        return "decision".to_string();
    }
    if haystack.contains("deliverable")
        || haystack.contains("exports/")
        || haystack.contains("final")
        || haystack.contains("release")
    {
        return "deliverable".to_string();
    }
    if haystack.contains("reference")
        || haystack.contains("refs/")
        || haystack.contains("asset")
        || haystack.contains("brief")
    {
        return "reference".to_string();
    }
    if name.ends_with(".md")
        || name.ends_with(".txt")
        || content_type
            .as_deref()
            .map(|kind| kind.contains("text/") || kind.contains("markdown"))
            .unwrap_or(false)
    {
        return "note".to_string();
    }
    "file".to_string()
}

fn person_provider_conflicts(local: &Person, remote: &Person) -> Option<ProviderConflict> {
    let (local_ref, remote_ref) = shared_provider_ref(&local.provider_refs, &remote.provider_refs)?;
    if !provider_refs_changed(local_ref, remote_ref) {
        return None;
    }

    let mut fields = Vec::new();
    push_provider_conflict(
        &mut fields,
        "display_name",
        Some(local.display_name.clone()),
        Some(remote.display_name.clone()),
    );
    push_provider_conflict(
        &mut fields,
        "given_name",
        local.given_name.clone(),
        remote.given_name.clone(),
    );
    push_provider_conflict(
        &mut fields,
        "family_name",
        local.family_name.clone(),
        remote.family_name.clone(),
    );
    push_provider_conflict(
        &mut fields,
        "organization",
        local.organization.clone(),
        remote.organization.clone(),
    );
    push_provider_conflict(
        &mut fields,
        "title",
        local.title.clone(),
        remote.title.clone(),
    );
    push_provider_conflict(
        &mut fields,
        "contact_methods",
        Some(format_contact_methods(&local.contact_methods)),
        Some(format_contact_methods(&remote.contact_methods)),
    );
    push_provider_conflict(
        &mut fields,
        "notes",
        local.notes.clone(),
        remote.notes.clone(),
    );
    push_provider_conflict(
        &mut fields,
        "follow_up_on",
        fmt_date(local.follow_up_on),
        fmt_date(remote.follow_up_on),
    );
    push_provider_conflict(
        &mut fields,
        "last_contacted_at",
        local.last_contacted_at.map(|value| value.to_rfc3339()),
        remote.last_contacted_at.map(|value| value.to_rfc3339()),
    );

    provider_conflict_report(
        "person",
        local
            .id
            .as_deref()
            .or(remote.id.as_deref())
            .unwrap_or(&local.display_name),
        local_ref,
        remote_ref,
        fields,
    )
}

fn organization_provider_conflicts(
    local: &OrganizationRecord,
    remote: &OrganizationRecord,
) -> Option<ProviderConflict> {
    let (local_ref, remote_ref) = shared_provider_ref(&local.provider_refs, &remote.provider_refs)?;
    if !provider_refs_changed(local_ref, remote_ref) {
        return None;
    }

    let mut fields = Vec::new();
    push_provider_conflict(
        &mut fields,
        "name",
        Some(local.name.clone()),
        Some(remote.name.clone()),
    );
    push_provider_conflict(
        &mut fields,
        "people",
        Some(format_string_list(&local.people)),
        Some(format_string_list(&remote.people)),
    );
    push_provider_conflict(
        &mut fields,
        "contact_methods",
        Some(format_contact_methods(&local.contact_methods)),
        Some(format_contact_methods(&remote.contact_methods)),
    );
    push_provider_conflict(
        &mut fields,
        "notes",
        local.notes.clone(),
        remote.notes.clone(),
    );
    push_provider_conflict(
        &mut fields,
        "follow_up_on",
        fmt_date(local.follow_up_on),
        fmt_date(remote.follow_up_on),
    );

    provider_conflict_report(
        "organization",
        local
            .id
            .as_deref()
            .or(remote.id.as_deref())
            .unwrap_or(&local.name),
        local_ref,
        remote_ref,
        fields,
    )
}

fn shared_provider_ref<'a>(
    local_refs: &'a [ProviderRef],
    remote_refs: &'a [ProviderRef],
) -> Option<(&'a ProviderRef, &'a ProviderRef)> {
    local_refs.iter().find_map(|local| {
        remote_refs
            .iter()
            .find(|remote| {
                local.provider == remote.provider
                    && local.account == remote.account
                    && local.collection == remote.collection
                    && ((local.href.is_some() && local.href == remote.href)
                        || (local.uid.is_some() && local.uid == remote.uid))
            })
            .map(|remote| (local, remote))
    })
}

fn provider_refs_changed(local: &ProviderRef, remote: &ProviderRef) -> bool {
    match (&local.etag, &remote.etag) {
        (Some(local_etag), Some(remote_etag)) => local_etag != remote_etag,
        _ => true,
    }
}

fn provider_conflict_report(
    entity_type: &str,
    entity_id: &str,
    local_ref: &ProviderRef,
    remote_ref: &ProviderRef,
    fields: Vec<ProviderConflictField>,
) -> Option<ProviderConflict> {
    if fields.is_empty() {
        return None;
    }

    Some(ProviderConflict {
        entity_type: entity_type.to_string(),
        entity_id: entity_id.to_string(),
        provider: local_ref.provider.clone(),
        account: local_ref
            .account
            .clone()
            .or_else(|| remote_ref.account.clone()),
        collection: local_ref
            .collection
            .clone()
            .or_else(|| remote_ref.collection.clone()),
        href: local_ref.href.clone().or_else(|| remote_ref.href.clone()),
        uid: local_ref.uid.clone().or_else(|| remote_ref.uid.clone()),
        local_etag: local_ref.etag.clone(),
        remote_etag: remote_ref.etag.clone(),
        fields,
    })
}

fn push_provider_conflict(
    fields: &mut Vec<ProviderConflictField>,
    field: &str,
    local_value: Option<String>,
    remote_value: Option<String>,
) {
    if local_value != remote_value {
        fields.push(ProviderConflictField {
            field: field.to_string(),
            local_value,
            remote_value,
        });
    }
}

fn format_contact_methods(methods: &[ContactMethod]) -> String {
    let mut values = methods
        .iter()
        .map(|method| {
            format!(
                "{}:{}:{}:{}",
                method.kind,
                method.value,
                method.label.as_deref().unwrap_or_default(),
                method.primary
            )
        })
        .collect::<Vec<_>>();
    values.sort();
    values.join(",")
}

fn format_string_list(values: &[String]) -> String {
    let mut values = values.to_vec();
    values.sort();
    values.join(",")
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct TaskSyncConflict {
    field: &'static str,
    local_value: Option<String>,
    remote_value: Option<String>,
}

fn task_sync_key(task: &Task) -> String {
    format!("id:{}", task.id)
}

fn find_matching_local_task<'a>(remote: &Task, local_tasks: &'a [Task]) -> Option<&'a Task> {
    local_tasks
        .iter()
        .find(|local| local.id == remote.id || local.title == remote.title)
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
    push_conflict(
        &mut conflicts,
        "due",
        fmt_date(local.due),
        fmt_date(remote.due),
    );
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
        Some(
            local
                .projects
                .iter()
                .map(|p| p.0.as_str())
                .collect::<Vec<_>>()
                .join(","),
        ),
        Some(
            remote
                .projects
                .iter()
                .map(|p| p.0.as_str())
                .collect::<Vec<_>>()
                .join(","),
        ),
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
    let id = new.id_ref();

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
        rows.push(("tags", Some(old.tags.join(",")), Some(new.tags.join(","))));
    }
    if old.projects != new.projects {
        rows.push((
            "projects",
            Some(
                old.projects
                    .iter()
                    .map(|p| p.0.as_str())
                    .collect::<Vec<_>>()
                    .join(","),
            ),
            Some(
                new.projects
                    .iter()
                    .map(|p| p.0.as_str())
                    .collect::<Vec<_>>()
                    .join(","),
            ),
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
            &id,
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
    use std::time::{SystemTime, UNIX_EPOCH};

    fn modified_at(ts: &str) -> chrono::DateTime<Utc> {
        chrono::DateTime::parse_from_rfc3339(ts).unwrap().to_utc()
    }

    fn temp_vault() -> std::path::PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let path = std::env::temp_dir().join(format!("task-core-inbox-test-{nanos}"));
        std::fs::create_dir_all(&path).unwrap();
        path
    }

    #[tokio::test]
    async fn health_treats_unavailable_index_as_degraded_not_failed() {
        let vault = temp_vault();
        let svc = VaultServiceImpl::new(&vault);
        svc.index.lock().unwrap().take();

        let health = crate::service::SystemService::health(&svc, false).await;
        let index = health
            .checks
            .iter()
            .find(|check| check.code == "INDEX_UNAVAILABLE")
            .expect("health should report the unavailable SQLite index");

        assert!(
            health.ok,
            "file-scan mode remains usable when the SQLite index is unavailable"
        );
        assert!(
            health.degraded,
            "health JSON should expose the usable-but-degraded state"
        );
        assert_eq!(index.severity, "warning");
        assert!(
            index
                .hint
                .as_deref()
                .unwrap_or_default()
                .contains("task index rebuild"),
            "operator hint should explain how to rebuild or enable the index"
        );

        let _ = std::fs::remove_dir_all(vault);
    }

    #[tokio::test]
    async fn inbox_capture_and_promote_round_trip() {
        let vault = temp_vault();
        let svc = VaultServiceImpl::new(&vault);

        let captured = svc
            .capture_inbox(InboxCaptureRequest {
                text: "Call accountant tomorrow !high #finance @phone".to_string(),
                actor: Some("agent".to_string()),
                source: Some("test".to_string()),
                kind: None,
            })
            .await
            .unwrap();

        assert_eq!(captured.title, "Call accountant");
        assert_eq!(captured.kind, "inbox");
        assert_eq!(captured.priority, "high");
        assert_eq!(captured.source.as_deref(), Some("test"));
        assert!(captured.tags.iter().any(|tag| tag == "inbox"));

        let inbox = svc.list_inbox_items().await;
        assert_eq!(inbox.len(), 1);

        let mut deleted_capture = svc
            .vault
            .read()
            .await
            .load_tasks()
            .into_iter()
            .find(|task| {
                captured
                    .id
                    .as_deref()
                    .is_some_and(|id| task.matches_reference(id))
            })
            .unwrap();
        deleted_capture.deleted_at = Some(Utc::now());
        svc.update_task(deleted_capture.clone()).await.unwrap();
        assert!(
            svc.list_inbox_items().await.is_empty(),
            "soft-deleted inbox captures should be hidden by default"
        );

        deleted_capture.deleted_at = None;
        svc.update_task(deleted_capture).await.unwrap();

        let promoted = svc
            .promote_inbox(InboxPromoteRequest {
                reference: captured.id.clone().unwrap(),
                kind: Some("commitment".to_string()),
                project: Some("Operations".to_string()),
                status: Some("planned".to_string()),
                assignee: Some("agent".to_string()),
                due: None,
                scheduled: Some("2026-05-01".to_string()),
                add_tags: vec!["review".to_string()],
                actor: Some("agent".to_string()),
            })
            .await
            .unwrap();

        assert_eq!(promoted.kind, "commitment");
        assert_eq!(promoted.status, "planned");
        assert_eq!(promoted.scheduled.as_deref(), Some("2026-05-01"));
        assert!(
            promoted
                .projects
                .iter()
                .any(|project| project == "Operations")
        );
        assert!(!promoted.tags.iter().any(|tag| tag == "inbox"));
        assert!(svc.list_inbox_items().await.is_empty());

        let _ = std::fs::remove_dir_all(vault);
    }

    #[tokio::test]
    async fn open_asset_repair_creates_task_and_links_asset() {
        let vault = temp_vault();
        let svc = VaultServiceImpl::new(&vault);

        let asset = svc
            .create_asset(AssetCreateRequest {
                name: "Main vocal mic".to_string(),
                status: Some("available".to_string()),
                category: Some("audio".to_string()),
                ..AssetCreateRequest::default()
            })
            .await
            .unwrap();

        let response = svc
            .open_asset_repair(
                &asset.id,
                AssetRepairRequest {
                    title: "Replace loose XLR connector".to_string(),
                    notes: Some("Fails when cable moves".to_string()),
                    vendor: Some("Bench tech".to_string()),
                    actor: Some("agent".to_string()),
                    ..AssetRepairRequest::default()
                },
            )
            .await
            .unwrap();

        assert_eq!(response.asset.status, AssetStatus::NeedsRepair);
        assert_eq!(response.asset.maintenance.len(), 1);
        assert_eq!(
            response.asset.maintenance[0]
                .task
                .as_ref()
                .map(|task| task.0.as_str()),
            Some(response.task.title.as_str())
        );
        assert!(
            response
                .asset
                .linked_tasks
                .iter()
                .any(|task| task.0 == response.task.title)
        );

        let tasks = svc.list_tasks().await;
        assert_eq!(tasks.len(), 1);
        assert_eq!(tasks[0].title, "Replace loose XLR connector");
        assert!(tasks[0].tags.iter().any(|tag| tag == "repair"));

        let _ = std::fs::remove_dir_all(vault);
    }

    #[tokio::test]
    async fn asset_reservations_flag_conflicts_and_can_be_released() {
        let vault = temp_vault();
        let svc = VaultServiceImpl::new(&vault);
        let start = DateTime::parse_from_rfc3339("2026-05-04T20:00:00Z")
            .unwrap()
            .to_utc();
        let end = DateTime::parse_from_rfc3339("2026-05-04T22:00:00Z")
            .unwrap()
            .to_utc();

        let asset = svc
            .create_asset(AssetCreateRequest {
                name: "Playback rig".to_string(),
                ..AssetCreateRequest::default()
            })
            .await
            .unwrap();
        let first = svc
            .reserve_asset(
                &asset.id,
                AssetReserveRequest {
                    reference: "Show A".to_string(),
                    starts_at: Some(start),
                    ends_at: Some(end),
                    ..AssetReserveRequest::default()
                },
            )
            .await
            .unwrap();
        assert!(first.conflicts.is_empty());
        assert_eq!(first.asset.status, AssetStatus::Reserved);

        let overlap = svc
            .reserve_asset(
                &asset.id,
                AssetReserveRequest {
                    reference: "Show B".to_string(),
                    starts_at: Some(start + chrono::Duration::hours(1)),
                    ends_at: Some(end + chrono::Duration::hours(1)),
                    force: true,
                    ..AssetReserveRequest::default()
                },
            )
            .await
            .unwrap();
        assert_eq!(overlap.conflicts.len(), 1);

        let conflicts = svc.asset_conflicts(AssetFilter::default()).await;
        assert!(!conflicts.is_empty());

        let released = svc
            .release_asset_reservation(&asset.id, &first.reservation.id)
            .await
            .unwrap();
        assert_eq!(released.reservations.len(), 1);

        let _ = std::fs::remove_dir_all(vault);
    }

    #[tokio::test]
    async fn list_tasks_deduplicates_records_with_same_stable_id() {
        let vault = temp_vault();
        let svc = VaultServiceImpl::new(&vault);
        let duplicate_id = Uuid::parse_str("00000000-0000-4000-8000-000000000501").unwrap();

        for title in ["Duplicate A", "Duplicate B"] {
            svc.create_task(Task {
                id: duplicate_id,
                title: title.to_string(),
                status: Status::Open,
                ..Default::default()
            })
            .await
            .unwrap();
        }

        let listed = svc.list_tasks().await;
        assert_eq!(
            listed.iter().filter(|task| task.id == duplicate_id).count(),
            1,
            "task list should not emit duplicate logical tasks with the same stable id"
        );

        let queried = svc.execute_query(Query::default()).await;
        assert_eq!(
            queried
                .iter()
                .filter(|task| task.id == duplicate_id)
                .count(),
            1,
            "query output should not emit duplicate logical tasks with the same stable id"
        );

        let _ = std::fs::remove_dir_all(vault);
    }

    #[tokio::test]
    async fn inbox_review_buckets_life_and_business_work() {
        let vault = temp_vault();
        let svc = VaultServiceImpl::new(&vault);
        let today = chrono::Local::now().date_naive();
        let old = today - chrono::Duration::days(10);

        for task in [
            Task {
                title: "Loose capture".to_string(),
                tags: vec!["inbox".to_string()].into(),
                issue_type: Some("inbox".to_string()),
                ..Default::default()
            },
            Task {
                title: "Client commitment".to_string(),
                issue_type: Some("commitment".to_string()),
                due: Some(today),
                ..Default::default()
            },
            Task {
                title: "Waiting on vendor".to_string(),
                issue_type: Some("waiting".to_string()),
                status: Status::OnHold,
                ..Default::default()
            },
            Task {
                title: "Someday cabin idea".to_string(),
                issue_type: Some("idea".to_string()),
                tags: vec!["someday".to_string()].into(),
                ..Default::default()
            },
            Task {
                title: "Overdue tax thing".to_string(),
                due: Some(today - chrono::Duration::days(1)),
                ..Default::default()
            },
        ] {
            svc.create_task(task).await.unwrap();
        }
        svc.vault
            .read()
            .await
            .save_task(&Task {
                id: Uuid::parse_str("00000000-0000-4000-8000-000000000502").unwrap(),
                title: "Unscheduled stale thing".to_string(),
                date_modified: Some(modified_at(&format!("{old}T00:00:00Z"))),
                ..Default::default()
            })
            .unwrap();

        let report = svc.daily_review_report().await;
        assert!(
            report
                .inbox
                .iter()
                .any(|item| item.title == "Loose capture")
        );
        assert!(
            report
                .commitments
                .iter()
                .any(|task| task.title == "Client commitment")
        );
        assert!(
            report
                .due_today
                .iter()
                .any(|task| task.title == "Client commitment")
        );
        assert!(
            report
                .waiting
                .iter()
                .any(|task| task.title == "Waiting on vendor")
        );
        assert!(
            report
                .someday
                .iter()
                .any(|task| task.title == "Someday cabin idea")
        );
        assert!(
            report
                .overdue
                .iter()
                .any(|task| task.title == "Overdue tax thing")
        );
        assert!(
            report
                .unscheduled
                .iter()
                .any(|task| task.title == "Unscheduled stale thing")
        );
        assert!(
            report
                .stale
                .iter()
                .any(|task| task.title == "Unscheduled stale thing")
        );

        let _ = std::fs::remove_dir_all(vault);
    }

    #[test]
    fn carddav_contacts_become_people_and_organizations() {
        let person = person_from_carddav_object(CardDavObject {
            href: "/remote.php/dav/addressbooks/users/agent/contacts/ada.vcf".to_string(),
            etag: Some("\"abc\"".to_string()),
            contact: Some(crate::service::CardDavContact {
                uid: Some("person-1".to_string()),
                full_name: Some("Ada Lovelace".to_string()),
                given_name: Some("Ada".to_string()),
                family_name: Some("Lovelace".to_string()),
                organization: Some("Analytical Engines".to_string()),
                title: Some("Founder".to_string()),
                emails: vec!["ada@example.com".to_string()],
                phones: vec!["+15550100".to_string()],
                ..Default::default()
            }),
            ..Default::default()
        })
        .expect("person should map from contact");

        assert_eq!(person.id.as_deref(), Some("person-1"));
        assert_eq!(person.display_name, "Ada Lovelace");
        assert_eq!(person.organization.as_deref(), Some("Analytical Engines"));
        assert!(
            person
                .contact_methods
                .iter()
                .any(|method| method.kind == "email" && method.value == "ada@example.com")
        );

        let orgs = organizations_from_people(&[person.clone()]);
        assert_eq!(orgs.len(), 1);
        assert_eq!(orgs[0].name, "Analytical Engines");
        assert_eq!(orgs[0].people, vec!["Ada Lovelace"]);

        let tokens = person_context_tokens(&person);
        let task = Task {
            title: "Follow up with Ada".to_string(),
            body: "Email ada@example.com about the prototype.".to_string(),
            ..Default::default()
        };
        assert!(task_matches_tokens(&task, &tokens));
    }

    #[test]
    fn carddav_people_and_org_conflicts_report_changed_fields() {
        let base_ref = ProviderRef {
            provider: "carddav".to_string(),
            account: Some("agent".to_string()),
            collection: Some("/remote.php/dav/addressbooks/users/agent/contacts/".to_string()),
            href: Some("/remote.php/dav/addressbooks/users/agent/contacts/ada.vcf".to_string()),
            uid: Some("person-1".to_string()),
            etag: Some("\"local\"".to_string()),
        };
        let remote_ref = ProviderRef {
            etag: Some("\"remote\"".to_string()),
            ..base_ref.clone()
        };
        let local = Person {
            id: Some("person-1".to_string()),
            display_name: "Ada Lovelace".to_string(),
            title: Some("Founder".to_string()),
            contact_methods: vec![ContactMethod {
                kind: "email".to_string(),
                value: "ada@example.com".to_string(),
                primary: true,
                ..Default::default()
            }],
            provider_refs: vec![base_ref],
            ..Default::default()
        };
        let remote = Person {
            title: Some("CEO".to_string()),
            contact_methods: vec![ContactMethod {
                kind: "email".to_string(),
                value: "ada@analytical.example".to_string(),
                primary: true,
                ..Default::default()
            }],
            provider_refs: vec![remote_ref],
            ..local.clone()
        };

        let conflict =
            person_provider_conflicts(&local, &remote).expect("changed etags and fields");
        assert_eq!(conflict.entity_type, "person");
        assert_eq!(conflict.entity_id, "person-1");
        assert_eq!(conflict.provider, "carddav");
        assert_eq!(conflict.local_etag.as_deref(), Some("\"local\""));
        assert_eq!(conflict.remote_etag.as_deref(), Some("\"remote\""));
        assert!(conflict.fields.iter().any(|field| field.field == "title"));
        assert!(
            conflict
                .fields
                .iter()
                .any(|field| field.field == "contact_methods")
        );

        let org_local = OrganizationRecord {
            id: Some("org-1".to_string()),
            name: "Analytical Engines".to_string(),
            people: vec!["Ada Lovelace".to_string()],
            provider_refs: vec![ProviderRef {
                provider: "carddav".to_string(),
                collection: Some("/remote.php/dav/addressbooks/users/agent/contacts/".to_string()),
                href: Some("/remote.php/dav/addressbooks/users/agent/contacts/org.vcf".to_string()),
                etag: Some("\"org-local\"".to_string()),
                ..Default::default()
            }],
            ..Default::default()
        };
        let org_remote = OrganizationRecord {
            people: vec!["Ada Lovelace".to_string(), "Charles Babbage".to_string()],
            provider_refs: vec![ProviderRef {
                etag: Some("\"org-remote\"".to_string()),
                ..org_local.provider_refs[0].clone()
            }],
            ..org_local.clone()
        };

        let org_conflict =
            organization_provider_conflicts(&org_local, &org_remote).expect("org conflict");
        assert_eq!(org_conflict.entity_type, "organization");
        assert!(
            org_conflict
                .fields
                .iter()
                .any(|field| field.field == "people")
        );
    }

    #[test]
    fn operating_model_groups_goals_routines_habits_and_area_pressure() {
        let today = chrono::Local::now().date_naive();
        let old = today - chrono::Duration::days(21);
        let tasks = vec![
            Task {
                title: "Grow consulting revenue".to_string(),
                issue_type: Some("goal".to_string()),
                areas: vec![WikiLink("Business".to_string())].into(),
                projects: vec![WikiLink("Consulting".to_string())].into(),
                due: Some(today + chrono::Duration::days(30)),
                ..Default::default()
            },
            Task {
                title: "Send proposal".to_string(),
                areas: vec![WikiLink("Business".to_string())].into(),
                projects: vec![WikiLink("Consulting".to_string())].into(),
                due: Some(today),
                ..Default::default()
            },
            Task {
                title: "Weekly planning".to_string(),
                issue_type: Some("routine".to_string()),
                areas: vec![WikiLink("Personal".to_string())].into(),
                recurrence: Some("FREQ=WEEKLY".to_string()),
                ..Default::default()
            },
            Task {
                title: "Exercise".to_string(),
                issue_type: Some("habit".to_string()),
                areas: vec![WikiLink("Health".to_string())].into(),
                recurrence: Some("FREQ=DAILY".to_string()),
                ..Default::default()
            },
            Task {
                title: "Old admin".to_string(),
                areas: vec![WikiLink("Business".to_string())].into(),
                date_modified: Some(modified_at(&format!("{old}T00:00:00Z"))),
                ..Default::default()
            },
        ];
        let projects = vec![Project {
            title: "Consulting".to_string(),
            area: Some("Business".to_string()),
            ..Default::default()
        }];

        let report = build_operating_model_report(tasks, projects, Vec::new());
        let business = report
            .areas
            .iter()
            .find(|area| area.name == "Business")
            .expect("business area");
        assert_eq!(business.active_projects, 1);
        assert_eq!(business.due_today_tasks, 1);
        assert_eq!(business.goal_tasks, 1);
        assert!(business.next_action.is_some());
        assert_eq!(report.goals.len(), 1);
        assert_eq!(
            report.goals[0]
                .next_action
                .as_ref()
                .map(|task| task.title.as_str()),
            Some("Send proposal")
        );
        assert_eq!(report.routines.len(), 1);
        assert_eq!(report.habits.len(), 1);
        assert_eq!(report.stale_tasks, 1);
    }

    #[test]
    fn project_file_classifier_identifies_knowledge_roles() {
        assert_eq!(
            classify_project_file("Project/docs/decision-auth.md", "decision-auth.md", &None),
            "decision"
        );
        assert_eq!(
            classify_project_file("Project/Exports/final.wav", "final.wav", &None),
            "deliverable"
        );
        assert_eq!(
            classify_project_file("Project/References/brief.pdf", "brief.pdf", &None),
            "reference"
        );
        assert_eq!(
            classify_project_file(
                "Project/notes.md",
                "notes.md",
                &Some("text/markdown".to_string())
            ),
            "note"
        );
        let project = Project {
            title: "Client Launch".to_string(),
            ..Default::default()
        };
        assert_eq!(project_storage_path(&project), "Client Launch");
    }

    #[test]
    fn finance_report_rolls_up_unbilled_time_and_invoice_aging() {
        let today = chrono::NaiveDate::from_ymd_opt(2026, 4, 30).unwrap();
        let start = modified_at("2026-04-29T10:00:00Z");
        let entries = vec![TimeEntryContext {
            task_title: "Client implementation".to_string(),
            client_name: Some("Acme".to_string()),
            client_rate: Some(12_000),
            entry: crate::TimeEntry {
                id: "entry-1".to_string(),
                start_time: start,
                end_time: Some(start + chrono::Duration::minutes(90)),
                billable: true,
                ..Default::default()
            },
            ..Default::default()
        }];
        let invoice = crate::invoice::Invoice {
            id: "INV-2026-0001".to_string(),
            client: WikiLink("Acme".to_string()),
            issue_date: today - chrono::Duration::days(45),
            due_date: today - chrono::Duration::days(15),
            line_items: vec![crate::invoice::InvoiceLine {
                hours: 2.0,
                rate_cents: 10_000,
                ..Default::default()
            }]
            .into(),
            ..Default::default()
        };

        let report = build_finance_report(entries, vec![invoice], today);
        assert_eq!(report.unbilled_minutes, 90);
        assert_eq!(report.unbilled_cents, 18_000);
        assert_eq!(report.open_invoice_cents, 20_000);
        assert_eq!(report.overdue_invoice_cents, 20_000);
        assert_eq!(report.clients[0].client_name, "Acme");
        assert_eq!(report.clients[0].unbilled_cents, 18_000);
        assert_eq!(report.aging[1].balance_cents, 20_000);
    }

    #[test]
    fn communication_token_matching_covers_message_text() {
        let tokens = normalize_context_tokens(vec![
            "Ada Lovelace".to_string(),
            "ada@example.com".to_string(),
        ]);
        assert!(text_matches_tokens(
            "Following up with Ada Lovelace about the contract",
            &tokens
        ));
        assert!(text_matches_tokens(
            "Ping ada@example.com tomorrow",
            &tokens
        ));
        assert!(!text_matches_tokens("Unrelated message", &tokens));
    }

    #[test]
    fn sync_plan_is_safe_dry_run_metadata() {
        let plan = build_sync_plan(Some(NextcloudRuntimeConfig {
            url: "https://cloud.example.test".to_string(),
            username: "agent".to_string(),
            password: "secret".to_string(),
            projects_path: "Projects/".to_string(),
            calendar: "tasks".to_string(),
            event_calendar: Some("events".to_string()),
            deck_enabled: true,
        }));
        assert!(plan.safe_to_run);
        assert!(plan.items.iter().any(|item| item.provider == "caldav"));
        assert!(
            plan.items
                .iter()
                .any(|item| item.provider == "nextcloud-talk")
        );
        assert!(plan.items.iter().all(|item| !item.destructive));

        let missing = build_sync_plan(None);
        assert!(!missing.safe_to_run);
        assert!(!missing.warnings.is_empty());
    }

    #[test]
    fn caldav_conflict_detection_blocks_same_field_overwrite() {
        let local = Task {
            id: Uuid::parse_str("00000000-0000-4000-8000-000000000503").unwrap(),
            title: "Shared task".to_string(),
            status: Status::InProgress,
            priority: Priority::Normal,
            date_modified: Some(modified_at("2026-04-29T10:00:00Z")),
            projects: vec![WikiLink("Personal".to_string())].into(),
            ..Default::default()
        };
        let remote = Task {
            id: Uuid::parse_str("00000000-0000-4000-8000-000000000503").unwrap(),
            title: "Shared task".to_string(),
            status: Status::Done,
            priority: Priority::Normal,
            date_modified: Some(modified_at("2026-04-29T10:05:00Z")),
            projects: vec![WikiLink("Personal".to_string())].into(),
            ..Default::default()
        };

        let conflicts = task_sync_conflicts(&local, &remote);
        assert_eq!(conflicts.len(), 1);
        assert_eq!(conflicts[0].field, "status");
        assert_eq!(task_sync_key(&local), format!("id:{}", local.id));
    }

    #[test]
    fn caldav_remote_newer_without_field_delta_is_not_a_conflict() {
        let local = Task {
            id: Uuid::parse_str("00000000-0000-4000-8000-000000000504").unwrap(),
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
