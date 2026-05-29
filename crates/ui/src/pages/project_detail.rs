//! `/projects/:id` — project overview.
//!
//! Fetches one [`ProjectInfo`] via `ProjectServiceClient.get` and the
//! org's tasks via the shared [`crate::task_wiring`], filters tasks to
//! this project (by `project_id` or `[[wikilink]]`), and renders a
//! header with a live progress bar plus the project's tasks through
//! [`task_ui::TasksApp`] (fully editable, write-through). Native has
//! no client yet (offline notice).

use dioxus::prelude::*;
use fts_ui::prelude::*;
use project::ProjectInfo;
use task::TaskInfo as DbTask;
use task_ui::{TaskInfo as UiTask, TaskMutation, TasksApp};
use uuid::Uuid;

use crate::routes::Route;
use crate::task_wiring::{fetch_tasks, handle, to_ui};

#[component]
pub fn ProjectDetailView(id: String) -> Element {
    let route_id = id.clone();
    let project = use_resource(move || {
        let id = route_id.clone();
        async move { fetch_project(id).await }
    });
    let tasks_loader = use_resource(|| async move { fetch_tasks().await });
    let mut tasks = use_signal(Vec::<DbTask>::new);
    use_effect(move || {
        if let Some(Ok(rows)) = &*tasks_loader.read_unchecked() {
            tasks.set(rows.clone());
        }
    });

    let proj = project.read_unchecked();
    let body = match &*proj {
        Some(Ok(p)) => {
            let p = p.clone();
            let all = tasks.read().clone();
            let mine: Vec<UiTask> = all.iter().filter(|t| belongs(t, &p)).map(to_ui).collect();
            let total = mine.len();
            let done = mine.iter().filter(|t| t.status == "done").count();
            let pct: f32 = if p.progress_percent >= 0 {
                f32::from(p.progress_percent)
            } else if total > 0 {
                (done as f32 / total as f32) * 100.0
            } else {
                0.0
            };

            rsx! {
                div { class: "flex flex-col gap-2",
                    Link { to: Route::ProjectsRoute {}, class: "text-xs text-muted-foreground hover:text-foreground",
                        "‹ Projects"
                    }
                    div { class: "flex items-center justify-between gap-3",
                        Heading { level: HeadingLevel::H1, "{p.title}" }
                        StatusBadge { variant: status_variant(&p.status), label: p.status.clone() }
                    }
                    div { class: "flex flex-wrap items-center gap-2 text-sm text-muted-foreground",
                        span { "{p.priority}" }
                        if !p.lead.is_empty() {
                            span { "· lead: {p.lead}" }
                        }
                        if let Some(d) = p.target_date {
                            span { "· due {d}" }
                        }
                    }
                    div { class: "mt-1 flex items-center gap-2",
                        div { class: "h-2 flex-1 overflow-hidden rounded-full bg-muted",
                            div { class: "h-full rounded-full bg-primary", style: "width: {pct}%" }
                        }
                        span { class: "text-xs text-muted-foreground", "{done}/{total} · {pct:.0}%" }
                    }
                }
                if !p.details.trim().is_empty() {
                    Text { variant: TextVariant::Muted, class: "text-sm", "{first_line(&p.details)}" }
                }
                div { class: "flex flex-col gap-2",
                    Heading { level: HeadingLevel::H2, "Tasks" }
                    if total == 0 {
                        Text { variant: TextVariant::Muted, "No tasks linked to this project yet." }
                    } else {
                        TasksApp {
                            tasks: mine,
                            on_event: move |mu: TaskMutation| handle(&mut tasks, mu),
                        }
                    }
                }
            }
        }
        Some(Err(e)) => rsx! {
            div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                "Couldn't load project: {e}"
            }
        },
        None => rsx! {
            Text { variant: TextVariant::Muted, "Loading project…" }
        },
    };

    rsx! {
        div { class: "mx-auto w-full max-w-6xl flex flex-col gap-6 p-6 lg:p-10", {body} }
    }
}

/// A task belongs to a project if its `project_id` matches, or its
/// `projects:` wikilink array references the project by `[[title]]`
/// (or bare title).
fn belongs(t: &DbTask, p: &ProjectInfo) -> bool {
    if t.project_id == Some(p.id) {
        return true;
    }
    let link = format!("[[{}]]", p.title);
    t.projects.0.iter().any(|x| x == &link || x == &p.title)
}

fn first_line(body: &str) -> String {
    body.lines()
        .map(str::trim)
        .find(|l| !l.is_empty() && !l.starts_with("---"))
        .unwrap_or("")
        .to_string()
}

fn status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "done" | "completed" | "active" => StatusBadgeVariant::Success,
        "on_hold" | "on-hold" | "paused" => StatusBadgeVariant::Warning,
        "cancelled" | "canceled" | "archived" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}

/// Fetch a single project via `ProjectServiceClient.get`.
async fn fetch_project(id: String) -> Result<ProjectInfo, String> {
    let uuid = Uuid::parse_str(&id).map_err(|_| "invalid project id".to_owned())?;
    let client = crate::vox_clients::project_client().await?;
    #[cfg(target_arch = "wasm32")]
    {
        client.get(uuid).await.map_err(|e| format!("get: {e:?}"))
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        let _ = (uuid, client);
        Err("native client not wired yet".to_owned())
    }
}
