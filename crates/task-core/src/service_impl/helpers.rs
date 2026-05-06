//! Shared helpers used by the SeaORM-backed `*ServiceImpl` types.
//!
//! These were extracted from the legacy `VaultServiceImpl` so the new
//! repo-backed Inbox/Operating/Time services can reuse the same review-bucket
//! and operating-model logic.

use chrono::Utc;
use serde::{Serialize, de::DeserializeOwned};

use crate::calendar_event::CalendarEvent;
use crate::project::Project;
use crate::service::{
    InboxItem, OperatingAreaStatus, OperatingGoal, OperatingModelReport, OperatingRoutine,
    ReviewReport, VaultError,
};
use crate::task::{Priority, Status, Task};

// ── Conversion helpers (mirror `business.rs`) ─────────────────────────────

pub(crate) fn convert_ref<T, U>(value: &T) -> Result<U, VaultError>
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(
        serde_json::to_value(value).map_err(|err| {
            VaultError::ParseError(format!("failed to serialize repo model: {err}"))
        })?,
    )
    .map_err(|err| VaultError::ParseError(format!("failed to deserialize repo model: {err}")))
}

pub(crate) fn convert_model<T, U>(value: T) -> Result<U, VaultError>
where
    T: Serialize,
    U: DeserializeOwned,
{
    serde_json::from_value(
        serde_json::to_value(value).map_err(|err| {
            VaultError::ParseError(format!("failed to serialize repo model: {err}"))
        })?,
    )
    .map_err(|err| VaultError::ParseError(format!("failed to deserialize repo model: {err}")))
}

pub(crate) fn provider_not_configured(operation: &str) -> VaultError {
    VaultError::IoError(format!(
        "{operation} requires a provider adapter; sqlite repositories only provide local read/query behavior"
    ))
}

// ── Status / priority labels ──────────────────────────────────────────────

pub(crate) fn status_label(status: &Status) -> &'static str {
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

pub(crate) fn priority_label(priority: &Priority) -> &'static str {
    match priority {
        Priority::None => "none",
        Priority::Low => "low",
        Priority::Normal => "normal",
        Priority::High => "high",
        Priority::Urgent => "urgent",
    }
}

pub(crate) fn parse_task_status(status: &str) -> Option<Status> {
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

pub(crate) fn parse_optional_naive_date(
    input: &str,
    field: &str,
) -> Result<Option<chrono::NaiveDate>, VaultError> {
    if input.is_empty() || input == "clear" {
        Ok(None)
    } else {
        input
            .parse::<chrono::NaiveDate>()
            .map(Some)
            .map_err(|err| VaultError::ParseError(format!("invalid {field}: {err}")))
    }
}

pub(crate) fn push_unique<T: PartialEq>(items: &mut Vec<T>, item: T) {
    if !items.contains(&item) {
        items.push(item);
    }
}

pub(crate) fn task_matches_reference(task: &Task, reference: &str) -> bool {
    task.matches_reference(reference)
}

pub(crate) fn normalize_inbox_kind(kind: Option<&str>) -> String {
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

// ── Inbox / review predicates ─────────────────────────────────────────────

pub(crate) fn is_inbox_task(task: &Task) -> bool {
    !task.is_deleted()
        && (task.tags.iter().any(|tag| tag == "inbox")
            || task.issue_type.as_deref() == Some("inbox"))
}

pub(crate) fn inbox_item_from_task(task: &Task) -> InboxItem {
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

pub(crate) fn is_review_actionable(task: &Task) -> bool {
    !task.is_complete()
        && !matches!(task.status, Status::Cancelled | Status::Archived)
        && task.deleted_at.is_none()
}

pub(crate) fn is_waiting_task(task: &Task) -> bool {
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

pub(crate) fn is_idea_task(task: &Task) -> bool {
    task.issue_type.as_deref() == Some("idea") || task.tags.iter().any(|tag| tag == "idea")
}

pub(crate) fn is_someday_task(task: &Task) -> bool {
    task.issue_type
        .as_deref()
        .map(|kind| matches!(kind, "someday" | "maybe"))
        .unwrap_or(false)
        || task
            .tags
            .iter()
            .any(|tag| matches!(tag.as_str(), "someday" | "maybe"))
}

pub(crate) fn is_goal_task(task: &Task) -> bool {
    task.issue_type
        .as_deref()
        .map(|kind| matches!(kind, "goal" | "objective" | "outcome"))
        .unwrap_or(false)
        || task
            .tags
            .iter()
            .any(|tag| matches!(tag.as_str(), "goal" | "objective" | "outcome"))
}

pub(crate) fn is_routine_task(task: &Task) -> bool {
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

pub(crate) fn is_habit_task(task: &Task) -> bool {
    task.issue_type.as_deref() == Some("habit") || task.tags.iter().any(|tag| tag == "habit")
}

pub(crate) fn first_task_area(task: &Task) -> Option<String> {
    task.areas.first().map(|area| area.0.clone())
}

pub(crate) fn task_in_area(task: &Task, area: &str) -> bool {
    if area == "Unassigned" {
        return task.areas.is_empty();
    }
    task.areas.iter().any(|candidate| candidate.0 == area)
}

pub(crate) fn task_to_operating_routine(task: &Task) -> OperatingRoutine {
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

pub(crate) fn related_next_action(goal: &Task, tasks: &[Task]) -> Option<Task> {
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

pub(crate) fn review_tasks(tasks: &[Task], predicate: impl Fn(&Task) -> bool) -> Vec<Task> {
    tasks
        .iter()
        .filter(|task| is_review_actionable(task) && predicate(task))
        .cloned()
        .collect()
}

pub(crate) fn sort_review_tasks(tasks: &mut [Task]) {
    tasks.sort_by(|a, b| {
        a.due
            .or(a.scheduled)
            .cmp(&b.due.or(b.scheduled))
            .then_with(|| b.priority.weight().cmp(&a.priority.weight()))
            .then_with(|| b.urgency_score().cmp(&a.urgency_score()))
            .then_with(|| a.title.cmp(&b.title))
    });
}

pub(crate) fn build_review_report(
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

pub(crate) fn build_operating_model_report(
    mut tasks: Vec<Task>,
    projects: Vec<Project>,
    events: Vec<CalendarEvent>,
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
                    .find(|task| is_review_actionable(task) && !is_waiting_task(task))
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
