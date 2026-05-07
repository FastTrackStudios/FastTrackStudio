//! SeaORM-backed `InboxService` implementation.
//!
//! Captures, lists, and promotes inbox items, plus assembles daily / weekly /
//! monthly / project review reports. Backed entirely by `TaskRepo`.

use chrono::Utc;
use uuid::Uuid;

use crate::service::{
    InboxCaptureRequest, InboxItem, InboxPromoteRequest, InboxService, ReviewReport, VaultError,
};
use crate::task::{
    Priority, Status, Task, TaskApi, TaskApiCreate, TaskApiList, TaskApiUpdate, TaskRepo, WikiLink,
};

use super::helpers::{
    build_review_report, convert_model, convert_ref, inbox_item_from_task, is_inbox_task,
    normalize_inbox_kind, parse_optional_naive_date, parse_task_status, push_unique,
    task_matches_reference,
};

/// Typed requirements for [`InboxServiceImpl`].
pub struct InboxServiceDeps<R> {
    pub task_repo: R,
}

#[derive(Clone)]
pub struct InboxServiceImpl<R> {
    task_repo: R,
}

impl<R> InboxServiceImpl<R> {
    pub fn new(deps: InboxServiceDeps<R>) -> Self {
        Self {
            task_repo: deps.task_repo,
        }
    }
}

impl<R> InboxServiceImpl<R>
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

    async fn create_task_model(&self, task: &Task) -> Result<Task, VaultError> {
        let create = convert_ref::<Task, TaskApiCreate>(task)?;
        self.task_repo
            .create_task(create)
            .await
            .map_err(VaultError::ParseError)
            .and_then(convert_model::<TaskApi, Task>)
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

impl<R> InboxService for InboxServiceImpl<R>
where
    R: TaskRepo,
{
    async fn capture(&self, request: InboxCaptureRequest) -> Result<InboxItem, VaultError> {
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
        let mut tags: Vec<String> = parsed.tags;
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

        let saved = self.create_task_model(&task).await?;
        Ok(inbox_item_from_task(&saved))
    }

    async fn list_inbox(&self) -> Vec<InboxItem> {
        self.list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(is_inbox_task)
            .map(|task| inbox_item_from_task(&task))
            .collect()
    }

    async fn promote(&self, request: InboxPromoteRequest) -> Result<InboxItem, VaultError> {
        let mut task = self
            .list_task_models()
            .await?
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
        task.date_modified = Some(Utc::now());

        let saved = self.update_task_model(&task).await?;
        Ok(inbox_item_from_task(&saved))
    }

    async fn daily_review(&self) -> ReviewReport {
        let tasks = self.list_task_models().await.unwrap_or_default();
        build_review_report(tasks, 7, 7)
    }

    async fn weekly_review(&self) -> ReviewReport {
        let tasks = self.list_task_models().await.unwrap_or_default();
        build_review_report(tasks, 30, 30)
    }

    async fn monthly_review(&self) -> ReviewReport {
        let tasks = self.list_task_models().await.unwrap_or_default();
        build_review_report(tasks, 90, 45)
    }

    async fn project_review(&self, project_title: String) -> ReviewReport {
        let tasks = self
            .list_task_models()
            .await
            .unwrap_or_default()
            .into_iter()
            .filter(|task| {
                task.projects
                    .iter()
                    .any(|project| project.0 == project_title)
            })
            .collect::<Vec<_>>();
        build_review_report(tasks, 30, 21)
    }
}
