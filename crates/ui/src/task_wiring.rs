//! Shared live-task wiring used by the `/tasks` workspace and the
//! project-overview Tasks tab.
//!
//! Holds the authoritative `Vec<task::TaskInfo>` (full persistence
//! shape), converts forward to the dumb `task_ui::TaskInfo` for
//! rendering, and maps UI mutations back onto the authoritative
//! records with optimistic write-through to the org's `TaskService`.
//! Native has no client yet (fetch returns an offline notice).

use dioxus::prelude::*;
use task::TaskInfo as DbTask;
use task_ui::{TaskInfo as UiTask, TaskMutation, TimeEntry as UiTimeEntry};

/// Fetch the active org's tasks via the cached `TaskServiceClient`.
pub(crate) async fn fetch_tasks() -> Result<Vec<DbTask>, String> {
    #[cfg(target_arch = "wasm32")]
    {
        let client = crate::vox_clients::task_client().await?;
        client.list().await.map_err(|e| format!("list: {e:?}"))
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Err("native client not wired yet".to_owned())
    }
}

/// Apply a UI mutation: optimistically update the authoritative list,
/// then write the affected record through to the service.
pub(crate) fn handle(tasks: &mut Signal<Vec<DbTask>>, mu: TaskMutation) {
    let mut list = tasks.write();
    let affected: Option<Affected> = match mu {
        TaskMutation::Create { task } => {
            let new = task::capture(&task.title);
            let snap = new.clone();
            list.push(new);
            Some(Affected::Create(snap))
        }
        TaskMutation::Update { task } => list.iter_mut().find(|t| t.id == task.id).map(|t| {
            apply_ui_edits(t, &task);
            Affected::Update(t.clone())
        }),
        TaskMutation::SetStatus { id, status } => list.iter_mut().find(|t| t.id == id).map(|t| {
            t.status = status;
            Affected::Update(t.clone())
        }),
        TaskMutation::SetPriority { id, priority } => {
            list.iter_mut().find(|t| t.id == id).map(|t| {
                t.priority = priority;
                Affected::Update(t.clone())
            })
        }
        TaskMutation::Delete { id } => {
            list.retain(|t| t.id != id);
            Some(Affected::Delete(id))
        }
    };
    drop(list);
    if let Some(a) = affected {
        persist(a);
    }
}

// Fields are read only on the wasm write-through path; native is a no-op.
#[cfg_attr(not(target_arch = "wasm32"), allow(dead_code))]
enum Affected {
    Create(DbTask),
    Update(DbTask),
    Delete(uuid::Uuid),
}

/// Forward convert the persistence model into the dumb UI model.
pub(crate) fn to_ui(t: &DbTask) -> UiTask {
    UiTask {
        id: t.id,
        title: t.title.clone(),
        status: t.status.clone(),
        priority: t.priority.clone(),
        due: t.due.clone(),
        scheduled: t.scheduled.clone(),
        tags: t.tags.0.clone(),
        contexts: t.contexts.0.clone(),
        projects: t.projects.0.clone(),
        time_estimate: t.time_estimate,
        time_entries: t
            .time_entries
            .0
            .iter()
            .map(|e| UiTimeEntry {
                start_time: e.start_time,
                end_time: e.end_time,
            })
            .collect(),
        recurrence: t.recurrence.clone(),
        completed_date: t.completed_date,
        date_created: t.date_created,
        date_modified: t.date_modified,
        details: t.details.clone(),
    }
}

/// Map the UI-editable fields of a detail-sheet save back onto the
/// authoritative record (preserving server-only fields like `path`,
/// `project_id`, billing, agent attribution).
fn apply_ui_edits(t: &mut DbTask, ui: &UiTask) {
    t.title = ui.title.clone();
    t.status = ui.status.clone();
    t.priority = ui.priority.clone();
    t.due = ui.due.clone();
    t.scheduled = ui.scheduled.clone();
    t.tags = ui.tags.clone().into();
    t.contexts = ui.contexts.clone().into();
    t.projects = ui.projects.clone().into();
    t.details = ui.details.clone();
}

/// Best-effort write-through. Optimistic UI already updated; on
/// failure the next load reconciles.
fn persist(affected: Affected) {
    #[cfg(target_arch = "wasm32")]
    {
        wasm_bindgen_futures::spawn_local(async move {
            if let Err(e) = persist_inner(affected).await {
                tracing::warn!("task write-through failed: {e}");
            }
        });
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = affected;
    }
}

#[cfg(target_arch = "wasm32")]
async fn persist_inner(affected: Affected) -> Result<(), String> {
    let client = crate::vox_clients::task_client().await?;
    match affected {
        Affected::Create(task) => {
            client
                .create(task)
                .await
                .map_err(|e| format!("create: {e:?}"))?;
        }
        Affected::Update(task) => {
            client
                .update(task)
                .await
                .map_err(|e| format!("update: {e:?}"))?;
        }
        Affected::Delete(id) => {
            client
                .delete(id)
                .await
                .map_err(|e| format!("delete: {e:?}"))?;
        }
    }
    Ok(())
}
