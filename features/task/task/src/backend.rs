//! Server-side [`TaskService`] backend — walks the configured
//! vault on each call.
//!
//! Mirror of `project::ProjectBackend` / `goal::GoalBackend`.
//! Cheap to `Clone`.

use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use chrono::Utc;
use uuid::Uuid;
use vault::Vault;

use crate::model::TaskInfo;
use crate::parse::{looks_like_task, parse_page};
use crate::service::{ClaimResult, TaskError, TaskEvent, TaskService};
use crate::write::{default_task_path, write_task};

#[derive(Clone, architect::HasDispatcher)]
pub struct TaskBackend {
    vault_root: PathBuf,
    /// Serializes `try_claim` read-check-write so two concurrent
    /// claims on the same task can't both win. One server process
    /// per org, so a process-local mutex is the whole story.
    claim_lock: Arc<Mutex<()>>,
    /// Fan-out hub behind the `#[subscribe] fn events` stream —
    /// every successful mutation publishes the post-write state
    /// here ([`TaskEvent::Upserted`] / [`TaskEvent::Deleted`]).
    /// Sliding mailbox: a slow subscriber loses its *oldest*
    /// queued events, which is correct for state-shaped payloads.
    /// Clones share the hub (it's `Arc` inside), so the service
    /// mount and the stream mount can each hold a backend clone.
    #[cfg(feature = "vox")]
    events: architect::PubSub<TaskEvent>,
}

// Manual impl: `PubSub` carries no `Debug`.
impl std::fmt::Debug for TaskBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TaskBackend")
            .field("vault_root", &self.vault_root)
            .finish_non_exhaustive()
    }
}

impl TaskBackend {
    #[must_use]
    pub fn new(vault_root: impl Into<PathBuf>) -> Self {
        Self {
            vault_root: vault_root.into(),
            claim_lock: Arc::new(Mutex::new(())),
            #[cfg(feature = "vox")]
            events: architect::PubSub::sliding(256),
        }
    }

    /// Publish a task change to every `events` subscriber. Call only
    /// after the write succeeded — subscribers fold these into state
    /// fetched via `list()`, so a phantom event would desync them.
    /// No-op without the `vox` feature (no wire, no subscribers).
    fn publish(&self, event: TaskEvent) {
        #[cfg(feature = "vox")]
        self.events.publish(event);
        #[cfg(not(feature = "vox"))]
        let _ = event;
    }

    #[must_use]
    pub fn vault_root(&self) -> &Path {
        &self.vault_root
    }

    fn list_inner(&self) -> Result<Vec<TaskInfo>, TaskError> {
        let vault = Vault::open(&self.vault_root)
            .map_err(|e| TaskError::Io(format!("open vault {}: {e}", self.vault_root.display())))?;
        let mut out = Vec::new();
        for page in &vault.pages {
            // The parser consumes the wasm-clean
            // `vault_proto::VaultPage`; convert the live page first.
            let proto = page.to_proto();
            if !looks_like_task(&proto) {
                continue;
            }
            match parse_page(&proto) {
                Ok(t) => out.push(t),
                Err(e) => tracing::warn!(path = %proto.rel_path, ?e, "task parse failed"),
            }
        }
        Ok(out)
    }
}

impl TaskService for TaskBackend {
    fn list(&self) -> Result<Vec<TaskInfo>, TaskError> {
        self.list_inner()
    }

    fn get(&self, id: Uuid) -> Result<TaskInfo, TaskError> {
        self.list_inner()?
            .into_iter()
            .find(|t| t.id == id)
            .ok_or_else(|| TaskError::NotFound(id.to_string()))
    }

    fn get_by_path(&self, path: &str) -> Result<TaskInfo, TaskError> {
        self.list_inner()?
            .into_iter()
            .find(|t| t.path == path)
            .ok_or_else(|| TaskError::NotFound(path.to_owned()))
    }

    fn create(&self, mut task: TaskInfo) -> Result<TaskInfo, TaskError> {
        if task.title.trim().is_empty() {
            return Err(TaskError::BadRequest("title is required".into()));
        }
        if task.id.is_nil() {
            task.id = Uuid::new_v4();
        }
        if task.path.is_empty() {
            task.path = default_task_path(&task.title, None);
        }
        let now = Utc::now();
        if task.date_created.is_none() {
            task.date_created = Some(now);
        }
        task.date_modified = Some(now);

        let abs = self.vault_root.join(&task.path);
        if abs.exists() {
            return Err(TaskError::AlreadyExists(task.path.clone()));
        }
        write_task(&self.vault_root, &mut task, false)
            .map_err(|e| TaskError::Io(format!("write: {e}")))?;
        self.publish(TaskEvent::Upserted(task.clone()));
        Ok(task)
    }

    fn update(&self, task: TaskInfo) -> Result<TaskInfo, TaskError> {
        let existing = self
            .list_inner()?
            .into_iter()
            .find(|t| t.id == task.id)
            .ok_or_else(|| TaskError::NotFound(task.id.to_string()))?;
        let mut next = task;
        next.path = existing.path;
        next.date_created = existing.date_created.or(next.date_created);
        next.date_modified = Some(Utc::now());
        write_task(&self.vault_root, &mut next, true)
            .map_err(|e| TaskError::Io(format!("write: {e}")))?;
        self.publish(TaskEvent::Upserted(next.clone()));
        Ok(next)
    }

    fn try_claim(&self, id: Uuid, agent: String, force: bool) -> Result<ClaimResult, TaskError> {
        let agent: workflows_proto::AgentRef = serde_json::from_str(&agent)
            .map_err(|e| TaskError::BadRequest(format!("agent ref json: {e}")))?;
        // Hold the lock across read → check → write so no other
        // claim can interleave. Truly atomic within the process.
        let _guard = self
            .claim_lock
            .lock()
            .map_err(|_| TaskError::Io("claim lock poisoned".into()))?;
        let mut t = self
            .list_inner()?
            .into_iter()
            .find(|t| t.id == id)
            .ok_or_else(|| TaskError::NotFound(id.to_string()))?;
        let w = t
            .workflow
            .get_or_insert_with(crate::model::WorkflowAttrs::default);
        if let Some(holder) = w.assignees.0.first() {
            if holder == &agent {
                return Ok(ClaimResult::AlreadyMine);
            }
            if !force {
                return Ok(ClaimResult::Lost {
                    holder: holder.short_label(),
                });
            }
        }
        w.assignees = crate::model::AgentRefList(vec![agent]);
        // Inline the write (we already hold the claim lock; calling
        // self.update would re-list but that's fine — keep it simple
        // and write directly).
        t.date_modified = Some(Utc::now());
        write_task(&self.vault_root, &mut t, true)
            .map_err(|e| TaskError::Io(format!("write: {e}")))?;
        // A won claim changed the assignees — that's an upsert. The
        // `AlreadyMine` / `Lost` early returns above write nothing,
        // so they (correctly) publish nothing.
        self.publish(TaskEvent::Upserted(t));
        Ok(ClaimResult::Won)
    }

    fn rename(&self, id: Uuid, new_path: &str) -> Result<TaskInfo, TaskError> {
        if new_path.is_empty() || new_path.contains("..") || new_path.starts_with('/') {
            return Err(TaskError::BadRequest(format!("bad path: {new_path}")));
        }
        let mut t = self
            .list_inner()?
            .into_iter()
            .find(|t| t.id == id)
            .ok_or_else(|| TaskError::NotFound(id.to_string()))?;
        let from = self.vault_root.join(&t.path);
        let to = self.vault_root.join(new_path);
        if to.exists() {
            return Err(TaskError::AlreadyExists(new_path.to_owned()));
        }
        if let Some(parent) = to.parent() {
            std::fs::create_dir_all(parent).map_err(|e| TaskError::Io(format!("mkdir: {e}")))?;
        }
        std::fs::rename(&from, &to).map_err(|e| TaskError::Io(format!("rename: {e}")))?;
        t.path = new_path.to_owned();
        t.date_modified = Some(Utc::now());
        write_task(&self.vault_root, &mut t, true)
            .map_err(|e| TaskError::Io(format!("write: {e}")))?;
        self.publish(TaskEvent::Upserted(t.clone()));
        Ok(t)
    }

    fn delete(&self, id: Uuid) -> Result<(), TaskError> {
        let t = self
            .list_inner()?
            .into_iter()
            .find(|t| t.id == id)
            .ok_or_else(|| TaskError::NotFound(id.to_string()))?;
        let abs = self.vault_root.join(&t.path);
        std::fs::remove_file(&abs)
            .map_err(|e| TaskError::Io(format!("remove {}: {e}", abs.display())))?;
        self.publish(TaskEvent::Deleted(id));
        Ok(())
    }
}

/// The `#[subscribe]` backend contract: hand the emitted stream host
/// the hub it attaches subscriber sinks to. Publishing happens in the
/// `TaskService` impl above, on every successful mutation.
#[cfg(feature = "vox")]
impl crate::service::TaskServiceStreamSource for TaskBackend {
    fn events_hub(&self) -> &architect::PubSub<TaskEvent> {
        &self.events
    }
}
