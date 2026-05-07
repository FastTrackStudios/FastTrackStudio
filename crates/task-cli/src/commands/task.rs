#![allow(clippy::needless_lifetimes, clippy::manual_strip)]
#![allow(unused_imports)]

use crate::*;

/// Typed inputs for [`build_new_task`].
pub(crate) struct NewTaskInput {
    pub(crate) title: String,
    pub(crate) priority: Option<String>,
    pub(crate) status: Option<String>,
    pub(crate) due: Option<String>,
    pub(crate) scheduled: Option<String>,
    pub(crate) project: Option<String>,
    pub(crate) context: Option<String>,
    pub(crate) tag: Option<String>,
    pub(crate) recurrence: Option<String>,
    pub(crate) assignee: Option<String>,
    pub(crate) actor: Option<String>,
}

pub(crate) fn build_new_task(input: NewTaskInput) -> eyre::Result<Task> {
    let NewTaskInput {
        title,
        priority,
        status,
        due,
        scheduled,
        project,
        context,
        tag,
        recurrence,
        assignee,
        actor,
    } = input;
    Ok(Task {
        title,
        priority: priority
            .as_deref()
            .map(parse_priority)
            .transpose()
            .map_err(|e| eyre::eyre!("{e}"))?
            .unwrap_or(Priority::None),
        status: status
            .as_deref()
            .map(|s| parse_status(s).ok_or_else(|| format!("Unknown status: {s}")))
            .transpose()
            .map_err(|e| eyre::eyre!("{e}"))?
            .unwrap_or(Status::Open),
        due: due
            .as_deref()
            .map(|d| {
                d.parse::<chrono::NaiveDate>()
                    .map_err(|e| eyre::eyre!("{e}"))
            })
            .transpose()?,
        scheduled: scheduled
            .as_deref()
            .map(|d| {
                d.parse::<chrono::NaiveDate>()
                    .map_err(|e| eyre::eyre!("{e}"))
            })
            .transpose()?,
        projects: project
            .map(|p| vec![WikiLink(p)])
            .unwrap_or_default()
            .into(),
        contexts: context.map(|c| vec![c]).unwrap_or_default().into(),
        tags: tag.map(|t| vec![t]).unwrap_or_default().into(),
        recurrence,
        assignee,
        created_by: actor,
        ..Task::default()
    })
}

/// Typed inputs for [`apply_task_update`].
pub(crate) struct TaskUpdateInput {
    pub(crate) title: Option<String>,
    pub(crate) status: Option<String>,
    pub(crate) priority: Option<String>,
    pub(crate) due: Option<String>,
    pub(crate) scheduled: Option<String>,
    pub(crate) assignee: Option<String>,
    pub(crate) add_tag: Vec<String>,
    pub(crate) remove_tag: Vec<String>,
    pub(crate) add_project: Vec<String>,
    pub(crate) remove_project: Vec<String>,
    pub(crate) add_context: Vec<String>,
    pub(crate) remove_context: Vec<String>,
    pub(crate) recurrence: Option<String>,
    pub(crate) body: Option<String>,
}

pub(crate) fn apply_task_update(task: &mut Task, input: TaskUpdateInput) -> eyre::Result<()> {
    let TaskUpdateInput {
        title,
        status,
        priority,
        due,
        scheduled,
        assignee,
        add_tag,
        remove_tag,
        add_project,
        remove_project,
        add_context,
        remove_context,
        recurrence,
        body,
    } = input;
    if let Some(t) = title {
        task.title = t;
    }
    if let Some(s) = status {
        task.status = parse_status(&s).ok_or_else(|| eyre::eyre!("Unknown status: {s}"))?;
    }
    if let Some(p) = priority {
        task.priority = parse_priority(&p).map_err(|e| eyre::eyre!("{e}"))?;
    }
    if let Some(d) = due {
        task.due = parse_optional_date(&d)?;
    }
    if let Some(d) = scheduled {
        task.scheduled = parse_optional_date(&d)?;
    }
    if let Some(a) = assignee {
        task.assignee = if a == "clear" || a.is_empty() {
            None
        } else {
            Some(a)
        };
    }
    for t in &remove_tag {
        task.tags.retain(|x| x != t);
    }
    for t in add_tag {
        if !task.tags.contains(&t) {
            task.tags.push(t);
        }
    }
    for p in &remove_project {
        task.projects.retain(|x| &x.0 != p);
    }
    for p in add_project {
        if !task.projects.iter().any(|x| x.0 == p) {
            task.projects.push(WikiLink(p));
        }
    }
    for c in &remove_context {
        task.contexts.retain(|x| x != c);
    }
    for c in add_context {
        if !task.contexts.contains(&c) {
            task.contexts.push(c);
        }
    }
    if let Some(r) = recurrence {
        task.recurrence = if r == "clear" || r.is_empty() {
            None
        } else {
            Some(r)
        };
    }
    if let Some(b) = body {
        task.body = b;
    }
    Ok(())
}

/// `task watch` — subscribe to live task ops via Vox and print them as they
/// arrive. Streams until the connection is closed (Ctrl-C).
pub(crate) async fn run_watch(
    remote: &crate::shared::RemoteVoxConfig,
    task_id: Option<String>,
    project: Option<String>,
    json: bool,
) -> eyre::Result<()> {
    use task_core::service::{TaskOp, TaskSubscriptionFilter};

    let task_id = match task_id {
        Some(s) => Some(
            uuid::Uuid::parse_str(s.trim()).map_err(|e| eyre::eyre!("invalid --task-id: {e}"))?,
        ),
        None => None,
    };
    let filter = TaskSubscriptionFilter {
        task_id,
        project: project.filter(|s| !s.is_empty()),
    };

    let client = remote.task().await?;
    let (tx, mut rx) = vox::channel::<TaskOp>();
    let subscribe = tokio::spawn(async move { client.subscribe(filter, tx).await });

    eprintln!("Watching task ops on {}…", remote.display_url);
    while let Ok(Some(op)) = rx.recv().await {
        let op = op.get();
        if json {
            println!("{}", task_op_to_json_line(op));
        } else {
            println!("{}", format_task_op(op));
        }
    }
    let _ = subscribe.await;
    Ok(())
}

fn format_task_op(op: &task_core::service::TaskOp) -> String {
    use task_core::service::TaskOp;
    match op {
        TaskOp::FieldChanged {
            task_id,
            field,
            value,
            peer,
        } => {
            let val = value.as_deref().unwrap_or("<cleared>");
            match peer {
                Some(p) => format!("{task_id}  field {field} = {val}  (peer={p})"),
                None => format!("{task_id}  field {field} = {val}"),
            }
        }
        TaskOp::BodyUpdate {
            task_id,
            update_bytes,
        } => format!("{task_id}  body update ({} bytes)", update_bytes.len()),
        TaskOp::Created { task_id, snapshot } => {
            format!("{task_id}  created (snapshot {} bytes)", snapshot.len())
        }
        TaskOp::Deleted { task_id } => format!("{task_id}  deleted"),
    }
}

fn task_op_to_json_line(op: &task_core::service::TaskOp) -> String {
    use task_core::service::TaskOp;
    match op {
        TaskOp::FieldChanged {
            task_id,
            field,
            value,
            peer,
        } => serde_json::json!({
            "kind": "field_changed",
            "task_id": task_id,
            "field": field,
            "value": value,
            "peer": peer,
        })
        .to_string(),
        TaskOp::BodyUpdate {
            task_id,
            update_bytes,
        } => serde_json::json!({
            "kind": "body_update",
            "task_id": task_id,
            "update_bytes_len": update_bytes.len(),
        })
        .to_string(),
        TaskOp::Created { task_id, snapshot } => serde_json::json!({
            "kind": "created",
            "task_id": task_id,
            "snapshot_len": snapshot.len(),
        })
        .to_string(),
        TaskOp::Deleted { task_id } => serde_json::json!({
            "kind": "deleted",
            "task_id": task_id,
        })
        .to_string(),
    }
}
