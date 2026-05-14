//! Live wrappers — components that own a `use_resource` against the
//! `project-proto` vox client surface. Compose the dumb render
//! components from this crate's other modules with a real WebSocket
//! transport.
//!
//! Route code mounts these with a `vox_url` prop and gets a working
//! data pipe with no client construction in the route file.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use project_proto::architect::Page;
use project_proto::{Project, ProjectRepoClient, Task, TaskRepoClient};
use uuid::Uuid;

/// Open + render the "tasks grouped by project" view from a vox
/// endpoint. Refresh button drops + restarts the underlying
/// `use_resource`.
#[component]
pub fn TasksByProjectLive(vox_url: String) -> Element {
    let url_for_resource = vox_url.clone();
    let mut snapshot = use_resource(move || {
        let url = url_for_resource.clone();
        async move { load_snapshot(url).await }
    });

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-4 p-6 lg:p-10",
            HStack { class: "items-center gap-3",
                Heading { level: HeadingLevel::H1, "Projects" }
                Button { on_click: move |_| snapshot.restart(), "Refresh" }
            }
            match &*snapshot.read_unchecked() {
                None => rsx! {
                    Text { variant: TextVariant::Muted, "Loading from /vox…" }
                },
                Some(Err(err)) => rsx! {
                    Text { variant: TextVariant::Muted, "Failed: {err}" }
                },
                Some(Ok(snap)) => rsx! { TasksByProjectView { snapshot: snap.clone() } },
            }
        }
    }
}

/// Dumb render half — feed it a `Snapshot` and it draws the cards.
#[component]
pub fn TasksByProjectView(snapshot: Snapshot) -> Element {
    if snapshot.ordered_projects.is_empty() {
        return rsx! {
            Text { variant: TextVariant::Muted, "No projects with tasks." }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-6",
            for (project_id, name) in snapshot.ordered_projects.iter() {
                {render_project_block(*project_id, name.clone(), snapshot.tasks_by_project.get(project_id).cloned().unwrap_or_default())}
            }
        }
    }
}

#[derive(Clone, PartialEq)]
pub struct Snapshot {
    pub project_names: HashMap<Uuid, String>,
    pub tasks_by_project: HashMap<Uuid, Vec<Task>>,
    /// `(project_id, name)` ordered by name for stable rendering.
    pub ordered_projects: Vec<(Uuid, String)>,
}

async fn load_snapshot(vox_url: String) -> Result<Snapshot, String> {
    let projects: ProjectRepoClient = connect_client(&vox_url).await?;
    let tasks: TaskRepoClient = connect_client(&vox_url).await?;
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

/// Open a typed vox client over a fresh WebSocket. Each call opens
/// its own session — for the simplest possible client wiring we
/// don't multiplex services over one WS. If a route needs N
/// clients, it opens N sockets.
#[cfg(target_arch = "wasm32")]
async fn connect_client<C>(url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession,
{
    use vox_core::{TransportMode, initiator_on};
    let link = vox_websocket::WsLink::connect(url)
        .await
        .map_err(|e| format!("ws connect: {e:?}"))?;
    initiator_on(link, TransportMode::Bare)
        .establish::<C>()
        .await
        .map_err(|e| format!("vox establish: {e:?}"))
}

#[cfg(not(target_arch = "wasm32"))]
async fn connect_client<C>(_url: &str) -> Result<C, String>
where
    C: vox_core::FromVoxSession,
{
    Err("connect_client only implemented for wasm32".into())
}
