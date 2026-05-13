//! Project overview grid — compact card per project with the next-up
//! task highlighted. Inspired by Linear's "Projects" page and Plane's
//! project grid: dense cards you can scan at a glance.

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ArrowRight, CircleCheck, Clock, Flag};
use fts_ui::prelude::*;
use project_proto::{Project, Task};
use uuid::Uuid;

use crate::pm::common::{color_swatch_class, status_to_badge_variant};
use crate::pm::priority::{Priority, PriorityBadge};

/// Responsive grid of [`ProjectOverviewCard`]s — 1 col on mobile, 2 at
/// md, 3 at xl. `tasks` is the global flat list across all projects;
/// the card filters by `task.project_id` so callers don't need to
/// pre-group.
#[component]
pub fn ProjectOverviewGrid(
    projects: Vec<Project>,
    tasks: Vec<Task>,
    on_open: EventHandler<Uuid>,
) -> Element {
    if projects.is_empty() {
        return rsx! {
            EmptyState {
                message: "No projects yet. Create one to get started.",
                icon: rsx! { Flag { size: 32 } },
            }
        };
    }

    rsx! {
        div { class: "grid grid-cols-1 gap-4 md:grid-cols-2 xl:grid-cols-3",
            for project in projects.iter().cloned() {
                ProjectOverviewCard {
                    key: "{project.id}",
                    project: project.clone(),
                    tasks: tasks.iter().filter(|t| t.project_id == project.id).cloned().collect(),
                    on_open: move |id| on_open.call(id),
                }
            }
        }
    }
}

#[component]
pub fn ProjectOverviewCard(
    project: Project,
    tasks: Vec<Task>,
    on_open: EventHandler<Uuid>,
) -> Element {
    let swatch = color_swatch_class(project.color.as_deref());
    let status_variant = status_to_badge_variant(&project.status);

    let total = tasks.len();
    let done = tasks
        .iter()
        .filter(|t| t.completed_at.is_some() || t.status == "done")
        .count();
    let blocked = tasks.iter().filter(|t| t.status == "blocked").count();
    let open = total.saturating_sub(done);
    let progress = if total == 0 {
        0
    } else {
        (done * 100 / total) as i32
    };

    let next = next_up_task(&tasks);

    let desc = project
        .description
        .clone()
        .unwrap_or_else(|| "No description.".into());

    rsx! {
        Card { class: "flex h-full flex-col transition-shadow hover:shadow-md",
            CardHeader {
                div { class: "flex items-start gap-3",
                    div { class: format!("mt-1 h-3 w-3 shrink-0 rounded-full {swatch}") }
                    div { class: "flex min-w-0 flex-1 flex-col gap-1",
                        div { class: "flex items-center gap-2",
                            CardTitle { class: "truncate text-base".to_string(), "{project.name}" }
                            StatusBadge { variant: status_variant, label: project.status.clone() }
                        }
                        CardDescription { class: "line-clamp-2".to_string(), "{desc}" }
                    }
                }
            }

            CardContent { class: "flex flex-1 flex-col gap-4".to_string(),
                // Progress bar + counts
                div { class: "flex flex-col gap-2",
                    div { class: "flex items-center justify-between text-xs text-muted-foreground",
                        span { "{done}/{total} tasks" }
                        span { "{progress}%" }
                    }
                    Progress { value: progress as f64 }
                    div { class: "flex flex-wrap gap-2",
                        Badge { variant: BadgeVariant::Outline, "{open} open" }
                        if blocked > 0 {
                            Badge { variant: BadgeVariant::Outline, class: "border-amber-500/40 text-amber-400".to_string(), "{blocked} blocked" }
                        }
                        if let Some(owner) = project.owner.as_deref() {
                            Badge { variant: BadgeVariant::Secondary, "{owner}" }
                        }
                    }
                }

                // Next-up task
                div { class: "mt-auto flex flex-col gap-2",
                    Text {
                        variant: TextVariant::Small,
                        class: "uppercase tracking-wider text-muted-foreground".to_string(),
                        "Next up"
                    }
                    match next {
                        Some(task) => rsx! { NextUpRow { task: task.clone(), on_open: move |id| on_open.call(id) } },
                        None => rsx! { CaughtUp {} },
                    }
                }
            }
        }
    }
}

#[component]
fn NextUpRow(task: Task, on_open: EventHandler<Uuid>) -> Element {
    let id = task.id;
    let due = task.due_date.map(format_due);
    let _ = Priority::try_from(task.priority.as_str()).unwrap_or(Priority::None);

    rsx! {
        button {
            class: "group flex w-full items-center gap-3 rounded-md border border-border bg-muted/30 px-3 py-2 text-left transition-colors hover:bg-muted/60",
            onclick: move |_| on_open.call(id),
            PriorityBadge {
                priority: task.priority.clone(),
                show_label: false,
                editable: false,
                on_change: move |_| {},
            }
            div { class: "flex min-w-0 flex-1 flex-col",
                span { class: "truncate text-sm font-medium", "{task.title}" }
                if let Some(d) = due.clone() {
                    span { class: "flex items-center gap-1 text-xs text-muted-foreground",
                        Clock { size: 12 }
                        "{d}"
                    }
                }
            }
            ArrowRight { size: 14, class: "shrink-0 opacity-0 transition-opacity group-hover:opacity-70".to_string() }
        }
    }
}

#[component]
fn CaughtUp() -> Element {
    rsx! {
        div { class: "flex items-center gap-2 rounded-md border border-dashed border-border bg-muted/20 px-3 py-2 text-sm text-muted-foreground",
            CircleCheck { size: 14 }
            "Caught up — no open tasks."
        }
    }
}

fn next_up_task(tasks: &[Task]) -> Option<&Task> {
    let now = Utc::now();
    // Open tasks only.
    let open: Vec<&Task> = tasks
        .iter()
        .filter(|t| t.completed_at.is_none() && t.status != "done" && t.status != "cancelled")
        .collect();

    // Highest priority first (Urgent > High > Medium > Low > None);
    // then earliest due date (None last); then earliest sort_index.
    open.into_iter().min_by(|a, b| {
        let pa = priority_rank(&a.priority);
        let pb = priority_rank(&b.priority);
        pa.cmp(&pb)
            .then_with(|| {
                due_rank(a.due_date.as_ref(), now).cmp(&due_rank(b.due_date.as_ref(), now))
            })
            .then_with(|| a.sort_index.cmp(&b.sort_index))
    })
}

fn priority_rank(p: &str) -> u8 {
    match Priority::try_from(p).unwrap_or(Priority::None) {
        Priority::Urgent => 0,
        Priority::High => 1,
        Priority::Medium => 2,
        Priority::Low => 3,
        Priority::None | Priority::Unknown(_) => 4,
    }
}

fn due_rank(due: Option<&chrono::DateTime<Utc>>, now: chrono::DateTime<Utc>) -> i64 {
    match due {
        Some(d) => (*d - now).num_seconds(),
        None => i64::MAX,
    }
}

fn format_due(d: chrono::DateTime<Utc>) -> String {
    let now = Utc::now();
    let secs = (d - now).num_seconds();
    if secs < 0 {
        let days = (-secs) / 86400;
        if days == 0 {
            "Overdue today".into()
        } else {
            format!("Overdue {days}d")
        }
    } else {
        let days = secs / 86400;
        match days {
            0 => "Due today".into(),
            1 => "Due tomorrow".into(),
            n if n < 7 => format!("Due in {n}d"),
            _ => d.format("Due %b %-d").to_string(),
        }
    }
}
