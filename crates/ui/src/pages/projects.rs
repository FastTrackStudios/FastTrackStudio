//! `/projects` — card-grid project browser.
//!
//! Fetches `Vec<ProjectInfo>` from the active org's
//! `/org/<slug>/vox` endpoint via the architect-generated
//! `ProjectServiceClient`. Subprojects nest inside their
//! parent card as a compact list.
//!
//! Loading + error states are rendered inline. When no
//! server is reachable, falls back to an empty grid with
//! a hint so dev/offline mode still looks like the route.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use project::ProjectInfo;
#[cfg(target_arch = "wasm32")]
use project::ProjectServiceClient;

use crate::routes::Route;

use crate::vox_session::vox_url;

#[component]
pub fn ProjectsView() -> Element {
    let projects = use_resource(|| async move { fetch_projects().await });

    let view = match &*projects.read_unchecked() {
        Some(Ok(rows)) => render_loaded(rows),
        Some(Err(e)) => render_error(e),
        None => render_loading(),
    };

    rsx! {
        div { class: "mx-auto w-full max-w-6xl flex flex-col gap-6 p-6 lg:p-10",
            div { class: "flex items-start justify-between gap-4",
                div { class: "flex flex-col gap-1",
                    Heading { level: HeadingLevel::H1, "Projects" }
                    Text {
                        variant: TextVariant::Muted,
                        "Live from `/org/<slug>/vox` via `ProjectServiceClient`."
                    }
                }
            }
            {view}
        }
    }
}

fn render_loading() -> Element {
    rsx! {
        Text { variant: TextVariant::Muted, "Loading projects…" }
    }
}

fn render_error(err: &str) -> Element {
    rsx! {
        div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
            "Couldn't reach the project service: {err}"
        }
    }
}

fn render_loaded(rows: &[ProjectInfo]) -> Element {
    let top: Vec<&ProjectInfo> = rows.iter().filter(|p| p.parent_id.is_none()).collect();
    let total = rows.len();
    let top_count = top.len();

    if rows.is_empty() {
        return rsx! {
            Text {
                variant: TextVariant::Muted,
                "No projects found. Seed a `type: project` page under `vault/Projects/`."
            }
        };
    }

    rsx! {
        div { class: "flex flex-col gap-4",
            Text {
                variant: TextVariant::Muted,
                class: "text-xs",
                "{top_count} top-level · {total} total"
            }
            div { class: "grid grid-cols-1 md:grid-cols-2 xl:grid-cols-3 gap-4",
                for parent in top.iter() {
                    {
                        let parent: ProjectInfo = (*parent).clone();
                        let kids: Vec<ProjectInfo> = rows
                            .iter()
                            .filter(|p| p.parent_id == Some(parent.id))
                            .cloned()
                            .collect();
                        rsx! {
                            ProjectCardView {
                                key: "{parent.id}",
                                p: parent,
                                subprojects: kids,
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct ProjectCardProps {
    p: ProjectInfo,
    subprojects: Vec<ProjectInfo>,
}

#[component]
fn ProjectCardView(props: ProjectCardProps) -> Element {
    let p = &props.p;
    let kids = props.subprojects.clone();
    let title = p.title.clone();
    let pid = p.id.to_string();
    let status = p.status.clone();
    let priority = p.priority.clone();
    let tags: Vec<String> = p.tags.0.clone();
    let summary = first_line(&p.details);

    rsx! {
        Card {
            CardHeader {
                div { class: "flex items-start justify-between gap-2",
                    Link { to: Route::ProjectDetailRoute { id: pid.clone() }, class: "hover:underline",
                        CardTitle { "{title}" }
                    }
                    StatusBadge {
                        variant: status_variant(&status),
                        label: status.clone(),
                    }
                }
                if let Some(s) = summary {
                    CardDescription { "{s}" }
                }
            }
            CardContent {
                div { class: "flex flex-wrap items-center gap-1.5 text-xs",
                    PriorityChip { priority: priority.clone() }
                    for tag in tags.iter() {
                        Badge {
                            variant: BadgeVariant::Secondary,
                            "{tag}"
                        }
                    }
                }
                if !kids.is_empty() {
                    div { class: "mt-4 flex flex-col gap-2 border-t border-border/40 pt-3",
                        Text {
                            variant: TextVariant::Muted,
                            class: "text-xs uppercase tracking-wider",
                            "Subprojects",
                        }
                        for kid in kids.iter() {
                            {
                                let kid_title = kid.title.clone();
                                let kid_status = kid.status.clone();
                                rsx! {
                                    div {
                                        key: "{kid.id}",
                                        class: "flex items-center justify-between gap-2 rounded-md bg-muted/30 px-3 py-2 text-sm",
                                        span { class: "font-medium", "{kid_title}" }
                                        StatusBadge {
                                            variant: status_variant(&kid_status),
                                            label: kid_status,
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct PriorityChipProps {
    priority: String,
}

#[component]
fn PriorityChip(props: PriorityChipProps) -> Element {
    let cls = match props.priority.as_str() {
        "high" | "urgent" | "p0" | "p1" => {
            "rounded-full px-2 py-0.5 bg-destructive/15 text-destructive font-medium"
        }
        "normal" | "p2" => "rounded-full px-2 py-0.5 bg-muted/60 text-muted-foreground",
        _ => "rounded-full px-2 py-0.5 bg-muted/40 text-muted-foreground",
    };
    let label = props.priority;
    rsx! {
        span { class: "{cls}", "{label}" }
    }
}

fn status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "active" => StatusBadgeVariant::Success,
        "on-hold" | "paused" => StatusBadgeVariant::Warning,
        "cancelled" | "blocked" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}

/// First non-empty line of the details body, trimmed. The
/// project parser preserves the whole markdown body in
/// `details`; we just want the first paragraph for a card
/// summary.
fn first_line(body: &str) -> Option<String> {
    body.lines()
        .map(str::trim)
        .find(|l| !l.is_empty() && !l.starts_with("---"))
        .map(std::string::ToString::to_string)
}

/// Fetch the active org's project list via the architect-
/// generated `ProjectServiceClient`. Returns `Err` strings
/// for the page to render; no `tracing` here so the wasm
/// console stays clean unless something genuinely surprises.
async fn fetch_projects() -> Result<Vec<ProjectInfo>, String> {
    let url = build_org_vox_url();
    if url.is_empty() {
        return Err("no vox URL configured (set TASK_VOX_URL_WEB)".to_owned());
    }
    #[cfg(target_arch = "wasm32")]
    {
        use vox_core::acceptor_on;
        let link = vox_websocket::WsLink::connect(&url)
            .await
            .map_err(|e| format!("ws connect: {e:?}"))?;
        let client: ProjectServiceClient = acceptor_on(link)
            .on_connection(())
            .establish::<ProjectServiceClient>()
            .await
            .map_err(|e| format!("establish: {e:?}"))?;
        client.list().await.map_err(|e| format!("list: {e:?}"))
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        Err("native client not wired yet".to_owned())
    }
}

/// Build the per-org vox URL from the base. The base may
/// already include `/vox` (legacy single-org default); strip
/// the suffix and retarget at `/org/codywright/vox` for now.
/// Multi-org switcher lands when the org-context signal does.
fn build_org_vox_url() -> String {
    let base = vox_url();
    if base.is_empty() {
        return String::new();
    }
    let trimmed = base
        .trim_end_matches("/vox")
        .trim_end_matches('/')
        .to_string();
    format!("{trimmed}/org/codywright/vox")
}
