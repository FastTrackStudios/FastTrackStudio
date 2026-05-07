//! SeaORM-backed `TimeService` implementation.
//!
//! `TimeEntry` is stored as JSON inside the owning `Task`. Each operation here
//! reads tasks via `TaskRepo`, mutates the `time_entries` vector, then writes
//! the task back through `TaskRepo::update_task`.
//!
//! Project / client rate enrichment for `list_time_entries` is intentionally
//! left unset on this repo-only impl — the cascade in
//! [`crate::service::TimeEntryContext::effective_rate`] still falls back through
//! entry-level rate and the caller's fallback, which is sufficient for the
//! sqlite-only build. Wiring `ProjectRepo` / `ClientRepo` enrichment is a
//! follow-up.

use chrono::Utc;
use tokio::sync::broadcast;
use uuid::Uuid;

use crate::service::{
    TaskOp, TimeEntryContext, TimeEntryFilter, TimeEntryPatch, TimeLogRequest, TimeService,
    TimeStartRequest, TimedTaskEntry, VaultError,
};
use crate::task::{Task, TaskApi, TaskApiList, TaskApiUpdate, TaskRepo, TimeEntry};

use super::helpers::{convert_model, convert_ref};

/// Capacity used when the caller does not provide a shared op channel. Mirrors
/// the value in `business.rs` so a stand-alone `TimeServiceImpl` still
/// supports broadcast subscribers.
const TASK_OP_CHANNEL_CAPACITY: usize = 256;

/// Typed requirements for [`TimeServiceImpl`].
pub struct TimeServiceDeps<R> {
    pub task_repo: R,
    /// Optional shared broadcast sender so time-entry mutations show up on
    /// the same `TaskService::subscribe` stream that scalar field changes do.
    /// `None` builds a dead-end channel — useful for tests / sqlite-only
    /// callers that don't watch ops.
    pub op_tx: Option<broadcast::Sender<TaskOp>>,
}

#[derive(Clone)]
pub struct TimeServiceImpl<R> {
    task_repo: R,
    op_tx: broadcast::Sender<TaskOp>,
}

impl<R> TimeServiceImpl<R> {
    pub fn new(deps: TimeServiceDeps<R>) -> Self {
        let op_tx = deps
            .op_tx
            .unwrap_or_else(|| broadcast::channel(TASK_OP_CHANNEL_CAPACITY).0);
        Self {
            task_repo: deps.task_repo,
            op_tx,
        }
    }

    /// Borrow the broadcast sender so callers (e.g. tests) can subscribe
    /// directly without going through `TaskService::subscribe`.
    pub fn op_sender(&self) -> &broadcast::Sender<TaskOp> {
        &self.op_tx
    }

    fn broadcast_time_entries_change(&self, task_id: Uuid) {
        // Time-entry mutations don't fit any of the scalar `FieldChanged`
        // entries; we still want subscribers to know the task changed so they
        // can refresh. Carrying the field name `time_entries` with a `None`
        // value is the agreed signal — clients reload on receipt.
        let _ = self.op_tx.send(TaskOp::FieldChanged {
            task_id,
            field: "time_entries".to_string(),
            value: None,
            peer: None,
        });
    }
}

impl<R> TimeServiceImpl<R>
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

impl<R> TimeService for TimeServiceImpl<R>
where
    R: TaskRepo,
{
    async fn start_timer(&self, request: TimeStartRequest) -> Result<TimeEntry, VaultError> {
        let tasks = self.list_task_models().await?;

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
            .find(|t| t.matches_reference(&request.task_ref))
            .ok_or_else(|| VaultError::NotFound(request.task_ref.clone()))?;

        let entry = TimeEntry {
            id: Uuid::new_v4().to_string(),
            user: request.user.clone(),
            start_time: Utc::now(),
            end_time: None,
            description: request.description,
            billable: request.billable,
            billable_rate: request.billable_rate,
            ..Default::default()
        };
        task.time_entries.push(entry.clone());
        task.date_modified = Some(Utc::now());
        let updated = self.update_task_model(&task).await?;
        self.broadcast_time_entries_change(updated.id);
        Ok(entry)
    }

    async fn stop_timer(&self, task_ref: Option<String>) -> Result<TimedTaskEntry, VaultError> {
        let tasks = self.list_task_models().await?;

        let mut task = tasks
            .into_iter()
            .find(|t| match task_ref.as_deref() {
                Some(r) => t.matches_reference(r) && t.running_timer().is_some(),
                None => t.running_timer().is_some(),
            })
            .ok_or_else(|| {
                VaultError::NotFound(match task_ref.as_deref() {
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
        let updated = self.update_task_model(&task).await?;
        self.broadcast_time_entries_change(updated.id);
        Ok(TimedTaskEntry {
            task_title: title,
            entry: stopped,
        })
    }

    async fn log_time(&self, request: TimeLogRequest) -> Result<TimeEntry, VaultError> {
        if request.end <= request.start {
            return Err(VaultError::ParseError("end must be after start".into()));
        }
        let mut task = self
            .list_task_models()
            .await?
            .into_iter()
            .find(|t| t.matches_reference(&request.task_ref))
            .ok_or_else(|| VaultError::NotFound(request.task_ref.clone()))?;

        let entry = TimeEntry {
            id: Uuid::new_v4().to_string(),
            user: request.user.clone(),
            start_time: request.start,
            end_time: Some(request.end),
            description: request.description,
            billable: request.billable,
            billable_rate: request.billable_rate,
            ..Default::default()
        };
        task.time_entries.push(entry.clone());
        task.date_modified = Some(Utc::now());
        let updated = self.update_task_model(&task).await?;
        self.broadcast_time_entries_change(updated.id);
        Ok(entry)
    }

    async fn active_timer(&self) -> Option<TimedTaskEntry> {
        let tasks = self.list_task_models().await.ok()?;
        for t in tasks {
            if let Some(e) = t.running_timer().cloned() {
                return Some(TimedTaskEntry {
                    task_title: t.title,
                    entry: e,
                });
            }
        }
        None
    }

    async fn list_time_entries(&self, filter: TimeEntryFilter) -> Vec<TimeEntryContext> {
        let tasks = self.list_task_models().await.unwrap_or_default();
        let mut out = Vec::new();
        for t in &tasks {
            if let Some(ref r) = filter.task_ref {
                if !t.matches_reference(r) {
                    continue;
                }
            }
            if let Some(ref p) = filter.project {
                if !t.projects.iter().any(|w| w.0.eq_ignore_ascii_case(p)) {
                    continue;
                }
            }
            // Client filter cannot be honored on a repo-only impl; if a caller
            // requests one, skip rather than return ungated entries.
            if filter.client.is_some() {
                continue;
            }
            let task_projects: Vec<String> = t.projects.iter().map(|w| w.0.clone()).collect();
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
                    if !e.tags.iter().any(|tt| tt.eq_ignore_ascii_case(tag)) {
                        continue;
                    }
                }
                out.push(TimeEntryContext {
                    task_title: t.title.clone(),
                    task_projects: task_projects.clone(),
                    client_name: None,
                    project_rate: None,
                    client_rate: None,
                    entry: e.clone(),
                });
            }
        }
        out
    }

    async fn edit_time_entry(
        &self,
        entry_id: String,
        patch: TimeEntryPatch,
        _actor: Option<String>,
    ) -> Result<TimedTaskEntry, VaultError> {
        let tasks = self.list_task_models().await?;
        for mut t in tasks {
            let Some(idx) = t.time_entries.iter().position(|e| e.id == entry_id) else {
                continue;
            };
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

            if let (Some(end), start) = (entry.end_time, entry.start_time) {
                if end <= start {
                    return Err(VaultError::ParseError("end must be after start".into()));
                }
            }

            let after = entry.clone();
            t.date_modified = Some(Utc::now());
            let title = t.title.clone();
            let updated = self.update_task_model(&t).await?;
            self.broadcast_time_entries_change(updated.id);
            return Ok(TimedTaskEntry {
                task_title: title,
                entry: after,
            });
        }
        Err(VaultError::NotFound(format!("time entry {entry_id}")))
    }

    async fn delete_time_entry(
        &self,
        entry_id: String,
        _actor: Option<String>,
    ) -> Result<(), VaultError> {
        let tasks = self.list_task_models().await?;
        for mut t in tasks {
            if t.time_entries.iter().any(|e| e.id == entry_id) {
                t.time_entries.retain(|e| e.id != entry_id);
                t.date_modified = Some(Utc::now());
                let updated = self.update_task_model(&t).await?;
                self.broadcast_time_entries_change(updated.id);
                return Ok(());
            }
        }
        Err(VaultError::NotFound(format!("time entry {entry_id}")))
    }
}
