//! Project route — opens a `ProjectRepoClient` + `TaskRepoClient`
//! over vox, lists everything, groups tasks by `project_id`, and
//! renders the result. This is the browser-side proof that the same
//! architect-emitted Repo traits the CLI calls also work end-to-end
//! from wasm.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use project_proto::architect::Page;
use project_proto::{Project, ProjectRepoClient, Task, TaskRepoClient};
use uuid::Uuid;

#[component]
pub fn ProjectView() -> Element {
    let mut snapshot = use_resource(load_snapshot);

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-4 p-6 lg:p-10",
            HStack { class: "items-center gap-3",
                Heading { level: HeadingLevel::H1, "Projects" }
                Button {
                    on_click: move |_| snapshot.restart(),
                    "Refresh"
                }
            }
            match &*snapshot.read_unchecked() {
                None => rsx! {
                    Text { variant: TextVariant::Muted, "Loading from /vox…" }
                },
                Some(Err(err)) => rsx! {
                    Text { variant: TextVariant::Muted, "Failed: {err}" }
                },
                Some(Ok(snap)) => render_snapshot(snap),
            }
        }
    }
}

#[derive(Clone, PartialEq)]
struct Snapshot {
    project_names: HashMap<Uuid, String>,
    tasks_by_project: HashMap<Uuid, Vec<Task>>,
    /// Sorted list of `(project_id, name)` for stable rendering.
    ordered_projects: Vec<(Uuid, String)>,
}

async fn load_snapshot() -> Result<Snapshot, String> {
    let projects: ProjectRepoClient = crate::vox_session::connect_client().await?;
    let tasks: TaskRepoClient = crate::vox_session::connect_client().await?;
    let big_page = Page {
        index: 0,
        size: 1000,
    };
    let project_page = projects
        .list(big_page.clone(), None, None)
        .await
        .map_err(|e| format!("project list: {e:?}"))?;
    let task_page = tasks
        .list(big_page, None, None)
        .await
        .map_err(|e| format!("task list: {e:?}"))?;

    let mut project_names: HashMap<Uuid, String> = HashMap::new();
    for p in project_page.items.iter() {
        project_names.insert(p.id, p.name.clone());
    }
    let mut tasks_by_project: HashMap<Uuid, Vec<Task>> = HashMap::new();
    for t in task_page.items.into_iter() {
        tasks_by_project.entry(t.project_id).or_default().push(t);
    }
    // Show projects that have tasks first, sorted by name.
    let mut ordered_projects: Vec<(Uuid, String)> = project_page
        .items
        .into_iter()
        .filter(|p: &Project| tasks_by_project.contains_key(&p.id))
        .map(|p| (p.id, p.name))
        .collect();
    ordered_projects.sort_by(|a, b| a.1.cmp(&b.1));

    Ok(Snapshot {
        project_names,
        tasks_by_project,
        ordered_projects,
    })
}

fn render_snapshot(snap: &Snapshot) -> Element {
    if snap.ordered_projects.is_empty() {
        return rsx! {
            Text { variant: TextVariant::Muted, "No projects with tasks." }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-6",
            for (project_id, name) in snap.ordered_projects.iter() {
                {render_project_block(*project_id, name.clone(), snap.tasks_by_project.get(project_id).cloned().unwrap_or_default())}
            }
        }
    }
}

fn render_project_block(project_id: Uuid, name: String, tasks: Vec<Task>) -> Element {
    rsx! {
        section {
            key: "{project_id}",
            class: "rounded-md border border-border bg-card p-4",
            HStack { class: "items-baseline justify-between mb-3",
                Heading { level: HeadingLevel::H3, "{name}" }
                Text { variant: TextVariant::Muted, "{tasks.len()} task(s)" }
            }
            ul { class: "flex flex-col gap-1.5",
                for task in tasks.iter() {
                    li { key: "{task.id}",
                        class: "flex items-center gap-3 text-sm",
                        StatusBadge {
                            variant: status_variant(&task.status),
                            label: task.status.clone(),
                        }
                        span { class: "text-foreground", "{task.title}" }
                    }
                }
            }
        }
    }
}

fn status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "done" => StatusBadgeVariant::Success,
        "in-progress" | "in-review" => StatusBadgeVariant::Warning,
        "blocked" | "cancelled" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}
