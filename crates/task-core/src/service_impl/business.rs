use std::sync::Arc;

use tokio::sync::broadcast;
use vox::Tx;

use super::helpers::{convert_model, convert_ref, provider_not_configured};
use crate::calendar_event::{CalendarEvent, CalendarEventApiList, CalendarEventRepo};
#[cfg(feature = "realtime")]
use crate::crdt::{CrdtDocument, SharedCrdtSnapshotStore};
use crate::expense::{
    Expense, ExpenseApiList, ExpenseFilter, ExpenseRepo, ExpenseReport, build_expense_report,
    matches_expense_filter,
};
use crate::project::{
    Project, ProjectApiList, ProjectDashboardEntry, ProjectRepo, ProjectStats,
    next_task as find_next_task, project_dashboard as build_project_dashboard,
};
use crate::provider::NextcloudSync;
use crate::query::Query;
use crate::service::{
    CalDavDeleteObjectRequest, CalDavDiscovery, CalDavFreeBusyInterval, CalDavFreeBusyRequest,
    CalDavMultigetRequest, CalDavObject, CalDavPutObjectRequest, CalDavScheduleRequest,
    CalDavScheduleResponse, CalDavSyncCollectionRequest, CalDavSyncCollectionResponse,
    CalendarService, CardDavDeleteObjectRequest, CardDavDiscovery, CardDavMultigetRequest,
    CardDavObject, CardDavPutObjectRequest, CardDavSyncCollectionRequest,
    CardDavSyncCollectionResponse, ExpenseService, ProjectKnowledgeContext, ProjectService,
    RemoteDeckBoard, RemoteDeckStack, SyncPlan, SyncStats, TaskOp, TaskService,
    TaskSubscriptionFilter, VaultError,
};
use crate::task::{Status, Task, TaskApi, TaskApiList, TaskApiUpdate, TaskRepo};

/// Default broadcast channel capacity for in-process task op fan-out.
///
/// 256 buffers a moderate burst per subscriber without unbounded memory; if a
/// slow subscriber falls behind the broadcast layer reports `RecvError::Lagged`
/// and the subscriber re-syncs on its own.
const TASK_OP_CHANNEL_CAPACITY: usize = 256;

/// Typed requirements for [`TaskServiceImpl`].
pub struct TaskServiceDeps<R> {
    pub task_repo: R,
    /// Optional broadcast sender for live `TaskOp` fan-out. When `None`, the
    /// service builds a fresh broadcast channel — useful for one-process
    /// callers that don't want to share a tx across other constructors.
    pub op_tx: Option<broadcast::Sender<TaskOp>>,
    /// Optional persistence layer for the per-task Loro snapshot column.
    /// `None` keeps `apply_op` working without realtime persistence (the test
    /// double in task-db wires this to `None`); when `Some`, `apply_op`
    /// loads/decodes/applies/encodes the doc and writes the snapshot back.
    #[cfg(feature = "realtime")]
    pub snapshot_store: Option<SharedCrdtSnapshotStore>,
}

/// Typed requirements for [`ProjectServiceImpl`].
pub struct ProjectServiceDeps<P, T> {
    pub project_repo: P,
    pub task_repo: T,
}

/// Typed requirements for [`ExpenseServiceImpl`].
pub struct ExpenseServiceDeps<R> {
    pub expense_repo: R,
}

/// Typed requirements for [`CalendarServiceImpl`].
///
/// `provider = None` makes the CalDAV/CardDAV/Deck protocol ops return
/// `provider_not_configured`; the local repo-driven ops
/// (`tasks_due_by`, `scheduled_between`, `events_between`) work either
/// way.
pub struct CalendarServiceDeps<T, E> {
    pub task_repo: T,
    pub event_repo: E,
    pub provider: Option<Arc<NextcloudSync>>,
}

#[derive(Clone)]
pub struct TaskServiceImpl<R> {
    task_repo: R,
    op_tx: broadcast::Sender<TaskOp>,
    #[cfg(feature = "realtime")]
    snapshot_store: Option<SharedCrdtSnapshotStore>,
}

#[derive(Clone)]
pub struct ProjectServiceImpl<P, T> {
    project_repo: P,
    task_repo: T,
}

#[derive(Clone)]
pub struct ExpenseServiceImpl<R> {
    expense_repo: R,
}

#[derive(Clone)]
pub struct CalendarServiceImpl<T, E> {
    task_repo: T,
    event_repo: E,
    provider: Option<Arc<NextcloudSync>>,
}

impl<R> TaskServiceImpl<R> {
    pub fn new(deps: TaskServiceDeps<R>) -> Self {
        let op_tx = deps
            .op_tx
            .unwrap_or_else(|| broadcast::channel(TASK_OP_CHANNEL_CAPACITY).0);
        Self {
            task_repo: deps.task_repo,
            op_tx,
            #[cfg(feature = "realtime")]
            snapshot_store: deps.snapshot_store,
        }
    }

    /// Borrow the broadcast sender so callers (e.g. tests, peer servers) can
    /// fan in additional ops without going through `apply_op`.
    pub fn op_sender(&self) -> &broadcast::Sender<TaskOp> {
        &self.op_tx
    }
}

impl<P, T> ProjectServiceImpl<P, T> {
    pub fn new(deps: ProjectServiceDeps<P, T>) -> Self {
        Self {
            project_repo: deps.project_repo,
            task_repo: deps.task_repo,
        }
    }
}

impl<R> ExpenseServiceImpl<R> {
    pub fn new(deps: ExpenseServiceDeps<R>) -> Self {
        Self {
            expense_repo: deps.expense_repo,
        }
    }
}

impl<T, E> CalendarServiceImpl<T, E> {
    pub fn new(deps: CalendarServiceDeps<T, E>) -> Self {
        Self {
            task_repo: deps.task_repo,
            event_repo: deps.event_repo,
            provider: deps.provider,
        }
    }

    fn provider(&self, op: &str) -> Result<&NextcloudSync, VaultError> {
        self.provider
            .as_ref()
            .map(Arc::as_ref)
            .ok_or_else(|| provider_not_configured(op))
    }
}

impl<R> TaskServiceImpl<R>
where
    R: TaskRepo,
{
    async fn list_task_models(&self) -> Result<Vec<Task>, VaultError> {
        self.task_repo
            .list_tasks(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<TaskApiList, Task>)
            .collect()
    }

    async fn update_task_model(&self, task: &Task) -> Result<Task, VaultError> {
        let update = convert_ref::<Task, TaskApiUpdate>(task)?;
        self.task_repo
            .update_task(task.id.to_string(), update)
            .await
            .map_err(VaultError::ParseError)
            .and_then(convert_model::<TaskApi, Task>)
    }
}

impl<P, T> ProjectServiceImpl<P, T>
where
    P: ProjectRepo,
    T: TaskRepo,
{
    async fn list_project_models(&self) -> Result<Vec<Project>, VaultError> {
        self.project_repo
            .list_projects(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<ProjectApiList, Project>)
            .collect()
    }

    async fn list_task_models(&self) -> Result<Vec<Task>, VaultError> {
        self.task_repo
            .list_tasks(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<TaskApiList, Task>)
            .collect()
    }
}

impl<R> ExpenseServiceImpl<R>
where
    R: ExpenseRepo,
{
    async fn list_expense_models(&self) -> Result<Vec<Expense>, VaultError> {
        self.expense_repo
            .list_expenses(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<ExpenseApiList, Expense>)
            .collect()
    }
}

impl<T, E> CalendarServiceImpl<T, E>
where
    T: TaskRepo,
    E: CalendarEventRepo,
{
    async fn list_task_models(&self) -> Result<Vec<Task>, VaultError> {
        self.task_repo
            .list_tasks(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<TaskApiList, Task>)
            .collect()
    }

    async fn list_event_models(&self) -> Result<Vec<CalendarEvent>, VaultError> {
        self.event_repo
            .list_calendar_events(None, None, None, Some(10_000))
            .await
            .map_err(VaultError::ParseError)?
            .into_iter()
            .map(convert_model::<CalendarEventApiList, CalendarEvent>)
            .collect()
    }
}

impl<R> TaskService for TaskServiceImpl<R>
where
    R: TaskRepo,
{
    async fn execute_query(&self, query: Query) -> Vec<Task> {
        self.list_task_models()
            .await
            .map(|tasks| query.execute(&tasks).into_iter().cloned().collect())
            .unwrap_or_default()
    }

    async fn urgency_score(&self, task: Task) -> i32 {
        task.urgency_score()
    }

    async fn complete_task(&self, title: String) -> Result<Task, VaultError> {
        let mut task = self
            .list_task_models()
            .await?
            .into_iter()
            .find(|task| task.matches_reference(&title))
            .ok_or_else(|| VaultError::NotFound(title.clone()))?;
        task.status = Status::Done;
        task.completed_date = Some(chrono::Utc::now().date_naive());
        task.date_modified = Some(chrono::Utc::now());
        let updated = self.update_task_model(&task).await?;

        // Mirror the scalar mutation into the Loro doc so subscribers and the
        // snapshot column stay authoritative.
        #[cfg(feature = "realtime")]
        self.apply_field_to_doc(updated.id, "status", Some("Done"))
            .await;

        // Broadcast the user-visible mutation. `send` only fails when there
        // are zero subscribers — that's fine.
        let _ = self.op_tx.send(TaskOp::FieldChanged {
            task_id: updated.id,
            field: "status".to_string(),
            value: Some("Done".to_string()),
            peer: None,
        });

        Ok(updated)
    }

    async fn search_tasks(&self, query: String) -> Vec<Task> {
        let needle = query.to_lowercase();
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.title.to_lowercase().contains(&needle)
                    || task.body.to_lowercase().contains(&needle)
            })
            .collect()
    }

    async fn tasks_for_user(&self, username: String) -> Vec<Task> {
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.assignee.as_deref() == Some(username.as_str())
                    || task.assignees.iter().any(|assignee| assignee == &username)
            })
            .collect()
    }

    async fn subscribe(&self, filter: TaskSubscriptionFilter, output: Tx<TaskOp>) {
        let mut rx = self.op_tx.subscribe();
        loop {
            match rx.recv().await {
                Ok(op) => {
                    if !matches_filter(&op, &filter, &self.task_repo).await {
                        continue;
                    }
                    if output.send(op).await.is_err() {
                        break;
                    }
                }
                Err(broadcast::error::RecvError::Lagged(_)) => continue,
                Err(broadcast::error::RecvError::Closed) => break,
            }
        }
        let _ = output.close(Default::default()).await;
    }

    async fn apply_op(&self, op: TaskOp) -> Result<(), VaultError> {
        match &op {
            TaskOp::FieldChanged {
                task_id,
                field,
                value,
                ..
            } => {
                let mut task = self.load_task(*task_id).await?;
                apply_field_to_task(&mut task, field, value.as_deref())?;
                task.date_modified = Some(chrono::Utc::now());
                self.update_task_model(&task).await?;
                #[cfg(feature = "realtime")]
                self.apply_field_to_doc(*task_id, field, value.as_deref())
                    .await;
            }
            TaskOp::BodyUpdate {
                task_id,
                update_bytes,
            } => {
                // Re-validate that the task exists before importing into the
                // doc; the import + persistence loop only runs when the
                // realtime feature is on (Loro is not compiled in otherwise).
                let _ = self.load_task(*task_id).await?;
                #[cfg(feature = "realtime")]
                self.apply_body_update_to_doc(*task_id, update_bytes).await;
                #[cfg(not(feature = "realtime"))]
                {
                    let _ = update_bytes;
                }
            }
            TaskOp::Created { task_id, snapshot } => {
                // Upsert: if the task is unknown, decode the snapshot into a
                // Task and create it through the repo; otherwise import the
                // bytes into the existing doc.
                let exists = self.load_task(*task_id).await.is_ok();
                #[cfg(feature = "realtime")]
                self.apply_created_op(*task_id, snapshot, exists).await?;
                #[cfg(not(feature = "realtime"))]
                {
                    let _ = (snapshot, exists);
                }
            }
            TaskOp::Deleted { task_id } => {
                self.task_repo
                    .delete_task(task_id.to_string())
                    .await
                    .map_err(VaultError::ParseError)?;
                #[cfg(feature = "realtime")]
                if let Some(store) = self.snapshot_store.as_ref() {
                    if let Err(err) = store.save_snapshot(*task_id, None).await {
                        tracing::warn!(
                            target: "task_core::crdt",
                            ?err,
                            task_id = %task_id,
                            "failed to clear crdt snapshot on delete",
                        );
                    }
                }
            }
        }
        // Fan out. `send` returns the live receiver count or a no-subscribers
        // error; either is fine.
        let _ = self.op_tx.send(op);
        Ok(())
    }
}

impl<R> TaskServiceImpl<R>
where
    R: TaskRepo,
{
    async fn load_task(&self, id: uuid::Uuid) -> Result<Task, VaultError> {
        let api = self
            .task_repo
            .get_task(id.to_string())
            .await
            .map_err(VaultError::NotFound)?;
        convert_model::<TaskApi, Task>(api)
    }

    /// Decode the persisted Loro snapshot for `task_id`, falling back to a
    /// fresh doc seeded from the current scalar columns when no snapshot has
    /// been written yet. Returns `Ok((task, doc))` so callers can mutate
    /// either side without re-reading.
    #[cfg(feature = "realtime")]
    async fn load_doc(&self, task_id: uuid::Uuid) -> Result<(Task, CrdtDocument), VaultError> {
        let task = self.load_task(task_id).await?;
        let doc = match self.snapshot_store.as_ref() {
            Some(store) => match store.load_snapshot(task_id).await? {
                Some(bytes) => CrdtDocument::from_snapshot(&bytes, task_id.to_string())?,
                None => CrdtDocument::from_task(&task, task_id.to_string()),
            },
            None => CrdtDocument::from_task(&task, task_id.to_string()),
        };
        Ok((task, doc))
    }

    /// Encode `doc` and persist the bytes through the snapshot store. No-op
    /// when no store is wired (tests / sqlite-only callers).
    #[cfg(feature = "realtime")]
    async fn save_doc(&self, task_id: uuid::Uuid, doc: &CrdtDocument) {
        let Some(store) = self.snapshot_store.as_ref() else {
            return;
        };
        match doc.export_snapshot() {
            Ok(bytes) => {
                if let Err(err) = store.save_snapshot(task_id, Some(bytes)).await {
                    tracing::warn!(
                        target: "task_core::crdt",
                        ?err,
                        task_id = %task_id,
                        "failed to persist crdt snapshot",
                    );
                }
            }
            Err(err) => {
                tracing::warn!(
                    target: "task_core::crdt",
                    ?err,
                    task_id = %task_id,
                    "failed to export crdt snapshot bytes",
                );
            }
        }
    }

    /// Mirror a scalar field mutation into the Loro doc, detect concurrent
    /// writes, and persist the new snapshot.
    #[cfg(feature = "realtime")]
    async fn apply_field_to_doc(&self, task_id: uuid::Uuid, field: &str, value: Option<&str>) {
        let (_, doc) = match self.load_doc(task_id).await {
            Ok(pair) => pair,
            Err(err) => {
                tracing::warn!(
                    target: "task_core::crdt",
                    ?err,
                    task_id = %task_id,
                    "failed to load crdt doc for field op",
                );
                return;
            }
        };
        doc.set_field(field, value.unwrap_or(""));
        doc.commit();
        self.save_doc(task_id, &doc).await;
    }

    /// Import remote Loro update bytes into the local doc and persist the
    /// merged snapshot. Conflicts are surfaced via tracing — the dedicated
    /// conflict log lives in `index::sqlite::TaskIndex` and is wired in a
    /// follow-up.
    #[cfg(feature = "realtime")]
    async fn apply_body_update_to_doc(&self, task_id: uuid::Uuid, update_bytes: &[u8]) {
        let (_, doc) = match self.load_doc(task_id).await {
            Ok(pair) => pair,
            Err(err) => {
                tracing::warn!(
                    target: "task_core::crdt",
                    ?err,
                    task_id = %task_id,
                    "failed to load crdt doc for body update",
                );
                return;
            }
        };
        let before = doc.snapshot_fields();
        if let Err(err) = doc.import(update_bytes) {
            tracing::warn!(
                target: "task_core::crdt",
                ?err,
                task_id = %task_id,
                "failed to import body update bytes",
            );
            return;
        }
        let conflicts = crate::crdt::detect_field_conflicts(&doc, &before, doc.peer_id());
        if !conflicts.is_empty() {
            tracing::warn!(
                target: "task_core::crdt",
                task_id = %task_id,
                ?conflicts,
                "concurrent field writes detected during body update import",
            );
        }
        // Mirror any scalar changes brought in by the import into the
        // entity columns so list / repo queries stay consistent with the
        // CRDT view.
        if let Err(err) = self.write_task_from_doc(&doc).await {
            tracing::warn!(
                target: "task_core::crdt",
                ?err,
                task_id = %task_id,
                "failed to persist task scalars after body update",
            );
        }
        self.save_doc(task_id, &doc).await;
    }

    /// Handle a `Created` op by either creating the task from the snapshot's
    /// embedded `Task` decode or importing into the existing doc.
    #[cfg(feature = "realtime")]
    async fn apply_created_op(
        &self,
        task_id: uuid::Uuid,
        snapshot: &[u8],
        exists: bool,
    ) -> Result<(), VaultError> {
        if exists {
            // Fast path: import the snapshot into the existing doc.
            let (_, doc) = self.load_doc(task_id).await?;
            if let Err(err) = doc.import(snapshot) {
                tracing::warn!(
                    target: "task_core::crdt",
                    ?err,
                    task_id = %task_id,
                    "failed to import Created snapshot into existing doc",
                );
            }
            if let Err(err) = self.write_task_from_doc(&doc).await {
                tracing::warn!(
                    target: "task_core::crdt",
                    ?err,
                    task_id = %task_id,
                    "failed to mirror Created snapshot into entity columns",
                );
            }
            self.save_doc(task_id, &doc).await;
            Ok(())
        } else {
            // Upsert: decode the snapshot, create the task, persist snapshot.
            let doc = CrdtDocument::from_snapshot(snapshot, task_id.to_string())?;
            let mut task = doc.to_task();
            // The snapshot may carry a different id — force the op's id to
            // win so peers stay aligned.
            task.id = task_id;
            let create = convert_ref::<Task, crate::task::TaskApiCreate>(&task)?;
            self.task_repo
                .create_task(create)
                .await
                .map_err(VaultError::ParseError)?;
            self.save_doc(task_id, &doc).await;
            Ok(())
        }
    }

    /// Decode the doc into a `Task` and write it through the repo. Used after
    /// remote imports so scalar columns mirror the CRDT view.
    #[cfg(feature = "realtime")]
    async fn write_task_from_doc(&self, doc: &CrdtDocument) -> Result<(), VaultError> {
        let mut task = doc.to_task();
        task.date_modified = Some(chrono::Utc::now());
        self.update_task_model(&task).await.map(|_| ())
    }
}

async fn matches_filter<R>(op: &TaskOp, filter: &TaskSubscriptionFilter, repo: &R) -> bool
where
    R: TaskRepo,
{
    if let Some(task_id) = filter.task_id {
        if op.task_id() != task_id {
            return false;
        }
    }
    let Some(project) = filter.project.as_deref() else {
        return true;
    };
    let Ok(api) = repo.get_task(op.task_id().to_string()).await else {
        return false;
    };
    let Ok(task) = convert_model::<TaskApi, Task>(api) else {
        return false;
    };
    task.projects.iter().any(|p| p.0 == project)
}

fn parse_status_label(s: &str) -> Option<Status> {
    Some(match s {
        "Open" | "open" => Status::Open,
        "InProgress" | "in_progress" => Status::InProgress,
        "OnHold" | "on_hold" => Status::OnHold,
        "Planned" | "planned" => Status::Planned,
        "Done" | "done" => Status::Done,
        "Cancelled" | "cancelled" => Status::Cancelled,
        "Archived" | "archived" => Status::Archived,
        "None" | "none" => Status::None,
        _ => return None,
    })
}

fn parse_priority_label(s: &str) -> Option<crate::task::Priority> {
    use crate::task::Priority;
    Some(match s {
        "None" | "none" => Priority::None,
        "Low" | "low" => Priority::Low,
        "Normal" | "normal" => Priority::Normal,
        "High" | "high" => Priority::High,
        "Urgent" | "urgent" => Priority::Urgent,
        _ => return None,
    })
}

fn apply_field_to_task(
    task: &mut Task,
    field: &str,
    value: Option<&str>,
) -> Result<(), VaultError> {
    let parse_err =
        |what: &str, raw: &str| VaultError::ParseError(format!("invalid {what} value '{raw}'"));
    match field {
        "title" => {
            task.title = value.unwrap_or_default().to_string();
        }
        "status" => {
            let raw = value.unwrap_or("Open");
            task.status = parse_status_label(raw).ok_or_else(|| parse_err("status", raw))?;
        }
        "priority" => {
            let raw = value.unwrap_or("None");
            task.priority = parse_priority_label(raw).ok_or_else(|| parse_err("priority", raw))?;
        }
        "assignee" => task.assignee = value.map(str::to_string),
        "due" => {
            task.due = match value {
                Some(raw) => Some(raw.parse().map_err(|_| parse_err("due", raw))?),
                None => None,
            };
        }
        "scheduled" => {
            task.scheduled = match value {
                Some(raw) => Some(raw.parse().map_err(|_| parse_err("scheduled", raw))?),
                None => None,
            };
        }
        "recurrence" => task.recurrence = value.map(str::to_string),
        "time_estimate" => {
            task.time_estimate = match value {
                Some(raw) => Some(raw.parse().map_err(|_| parse_err("time_estimate", raw))?),
                None => None,
            };
        }
        "external_id" => task.external_id = value.map(str::to_string),
        "external_source" => task.external_source = value.map(str::to_string),
        "created_by" => task.created_by = value.map(str::to_string),
        other => {
            return Err(VaultError::ParseError(format!(
                "unknown task field '{other}'"
            )));
        }
    }
    Ok(())
}

impl<P, T> ProjectService for ProjectServiceImpl<P, T>
where
    P: ProjectRepo,
    T: TaskRepo,
{
    async fn project_stats(&self, project_title: String) -> ProjectStats {
        let tasks = self.list_task_models().await.unwrap_or_default();
        let refs: Vec<&Task> = tasks
            .iter()
            .filter(|task| {
                task.projects
                    .iter()
                    .any(|project| project.0 == project_title)
            })
            .collect();
        ProjectStats::from_tasks(&refs)
    }

    async fn project_dashboard(&self) -> Vec<ProjectDashboardEntry> {
        let projects = self.list_project_models().await.unwrap_or_default();
        let tasks = self.list_task_models().await.unwrap_or_default();
        build_project_dashboard(&projects, &tasks)
    }

    async fn next_task(&self, project_title: String) -> Option<Task> {
        let tasks = self.list_task_models().await.unwrap_or_default();
        find_next_task(&project_title, &tasks).cloned()
    }

    async fn tasks_for_project(&self, project_title: String) -> Vec<Task> {
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.projects
                    .iter()
                    .any(|project| project.0 == project_title)
            })
            .collect()
    }

    async fn project_context(
        &self,
        project_title: String,
        _include_files: bool,
        _depth: String,
    ) -> Result<Option<ProjectKnowledgeContext>, VaultError> {
        let project = self
            .list_project_models()
            .await?
            .into_iter()
            .find(|project| project.title.eq_ignore_ascii_case(&project_title));
        let Some(project) = project else {
            return Ok(None);
        };
        let tasks = self.tasks_for_project(project.title.clone()).await;
        let next_action = find_next_task(&project.title, &tasks).cloned();
        let project_path = project
            .dev_path
            .as_deref()
            .filter(|path| {
                !path.trim().is_empty() && !path.starts_with('~') && !path.starts_with('/')
            })
            .map(|path| path.trim_matches('/').to_string())
            .unwrap_or_else(|| project.title.trim_matches('/').to_string());

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
            files: Vec::new(),
            notes: Vec::new(),
            decisions: Vec::new(),
            deliverables: Vec::new(),
        }))
    }
}

impl<R> ExpenseService for ExpenseServiceImpl<R>
where
    R: ExpenseRepo,
{
    async fn expense_report(&self, filter: ExpenseFilter) -> ExpenseReport {
        let expenses: Vec<Expense> = self
            .list_expense_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|expense| matches_expense_filter(expense, &filter))
            .collect();
        build_expense_report(&expenses, chrono::Utc::now().date_naive())
    }
}

impl<T, E> CalendarService for CalendarServiceImpl<T, E>
where
    T: TaskRepo,
    E: CalendarEventRepo,
{
    async fn tasks_due_by(&self, date: String) -> Vec<Task> {
        let Ok(due_date) = date.parse::<chrono::NaiveDate>() else {
            return Vec::new();
        };
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.due.map(|due| due <= due_date).unwrap_or(false) && !task.is_complete()
            })
            .collect()
    }

    async fn scheduled_between(&self, from: String, to: String) -> Result<Vec<Task>, VaultError> {
        let from = from.parse::<chrono::NaiveDate>().map_err(|err| {
            VaultError::ParseError(format!("invalid scheduled start date '{from}': {err}"))
        })?;
        let to = to.parse::<chrono::NaiveDate>().map_err(|err| {
            VaultError::ParseError(format!("invalid scheduled end date '{to}': {err}"))
        })?;
        Ok(self
            .list_task_models()
            .await?
            .into_iter()
            .filter(|task| {
                task.scheduled
                    .map(|scheduled| scheduled >= from && scheduled <= to)
                    .unwrap_or(false)
            })
            .collect())
    }

    async fn events_between(
        &self,
        from: String,
        to: String,
    ) -> Result<Vec<CalendarEvent>, VaultError> {
        let from = chrono::DateTime::parse_from_rfc3339(&from)
            .map_err(|err| VaultError::ParseError(format!("invalid event start '{from}': {err}")))?
            .with_timezone(&chrono::Utc);
        let to = chrono::DateTime::parse_from_rfc3339(&to)
            .map_err(|err| VaultError::ParseError(format!("invalid event end '{to}': {err}")))?
            .with_timezone(&chrono::Utc);
        Ok(self
            .list_event_models()
            .await?
            .into_iter()
            .filter(|event| {
                let event_end = event.end.unwrap_or(event.start);
                event_end >= from && event.start <= to
            })
            .collect())
    }

    async fn trigger_sync(&self) -> Result<SyncStats, VaultError> {
        Err(provider_not_configured("calendar sync"))
    }

    async fn sync_status(&self) -> Option<SyncStats> {
        None
    }

    async fn sync_plan(&self) -> SyncPlan {
        SyncPlan {
            generated_at: chrono::Utc::now().to_rfc3339(),
            safe_to_run: false,
            warnings: vec![
                "provider adapters are not configured for sqlite-only CalendarService".into(),
            ],
            ..SyncPlan::default()
        }
    }

    async fn discover_caldav(&self) -> Result<CalDavDiscovery, VaultError> {
        self.provider("CalDAV discovery")?
            .discover_calendars()
            .await
    }

    async fn discover_carddav(&self) -> Result<CardDavDiscovery, VaultError> {
        self.provider("CardDAV discovery")?
            .discover_addressbooks()
            .await
    }

    async fn calendar_multiget(
        &self,
        request: CalDavMultigetRequest,
    ) -> Result<Vec<CalDavObject>, VaultError> {
        self.provider("CalDAV multiget")?
            .calendar_multiget(&request.calendar, &request.hrefs)
            .await
    }

    async fn calendar_sync_collection(
        &self,
        request: CalDavSyncCollectionRequest,
    ) -> Result<CalDavSyncCollectionResponse, VaultError> {
        self.provider("CalDAV sync collection")?
            .sync_calendar_collection(&request.calendar, request.sync_token.as_deref())
            .await
    }

    async fn addressbook_multiget(
        &self,
        request: CardDavMultigetRequest,
    ) -> Result<Vec<CardDavObject>, VaultError> {
        self.provider("CardDAV multiget")?
            .addressbook_multiget(&request.addressbook, &request.hrefs)
            .await
    }

    async fn addressbook_sync_collection(
        &self,
        request: CardDavSyncCollectionRequest,
    ) -> Result<CardDavSyncCollectionResponse, VaultError> {
        self.provider("CardDAV sync collection")?
            .sync_addressbook_collection(&request.addressbook, request.sync_token.as_deref())
            .await
    }

    async fn put_calendar_object(&self, request: CalDavPutObjectRequest) -> Result<(), VaultError> {
        self.provider("CalDAV put object")?
            .put_calendar_object(
                &request.calendar,
                &request.href,
                &request.calendar_data,
                request.if_match.as_deref(),
                request.if_none_match.as_deref(),
            )
            .await
    }

    async fn delete_calendar_object(
        &self,
        request: CalDavDeleteObjectRequest,
    ) -> Result<(), VaultError> {
        self.provider("CalDAV delete object")?
            .delete_calendar_object(
                &request.calendar,
                &request.href,
                request.if_match.as_deref(),
            )
            .await
    }

    async fn put_addressbook_object(
        &self,
        request: CardDavPutObjectRequest,
    ) -> Result<(), VaultError> {
        self.provider("CardDAV put object")?
            .put_addressbook_object(
                &request.addressbook,
                &request.href,
                &request.address_data,
                request.if_match.as_deref(),
                request.if_none_match.as_deref(),
            )
            .await
    }

    async fn delete_addressbook_object(
        &self,
        request: CardDavDeleteObjectRequest,
    ) -> Result<(), VaultError> {
        self.provider("CardDAV delete object")?
            .delete_addressbook_object(
                &request.addressbook,
                &request.href,
                request.if_match.as_deref(),
            )
            .await
    }

    async fn send_calendar_schedule(
        &self,
        request: CalDavScheduleRequest,
    ) -> Result<CalDavScheduleResponse, VaultError> {
        let provider = self.provider("CalDAV schedule")?;
        let outbox = match request.outbox_url.as_deref() {
            Some(url) if !url.is_empty() => url.to_string(),
            _ => provider
                .discover_calendars()
                .await?
                .schedule_outbox_url
                .ok_or_else(|| {
                    VaultError::IoError(
                        "CalDAV schedule outbox URL not configured on the principal".into(),
                    )
                })?,
        };
        provider
            .send_calendar_schedule(&outbox, &request.calendar_data)
            .await
    }

    async fn calendar_free_busy(
        &self,
        request: CalDavFreeBusyRequest,
    ) -> Result<Vec<CalDavFreeBusyInterval>, VaultError> {
        self.provider("CalDAV free busy")?
            .calendar_free_busy(&request.calendar, request.start, request.end)
            .await
    }

    async fn list_deck_boards(&self) -> Result<Vec<RemoteDeckBoard>, VaultError> {
        let boards = self.provider("Deck boards")?.list_boards().await?;
        Ok(boards
            .into_iter()
            .map(|board| RemoteDeckBoard {
                id: board.id,
                title: board.title,
                archived: board.archived,
            })
            .collect())
    }

    async fn list_deck_stacks(&self, board_id: u64) -> Result<Vec<RemoteDeckStack>, VaultError> {
        let stacks = self.provider("Deck stacks")?.list_stacks(board_id).await?;
        Ok(stacks
            .into_iter()
            .map(|stack| RemoteDeckStack {
                card_count: stack.cards.len() as u32,
                id: stack.id,
                title: stack.title,
            })
            .collect())
    }
}
