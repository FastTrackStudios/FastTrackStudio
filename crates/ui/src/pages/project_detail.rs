//! `/projects/:id` — project overview.
//!
//! Fetches one [`ProjectInfo`] via `ProjectServiceClient.get` and the
//! org's tasks via the shared [`crate::task_wiring`], filters tasks to
//! this project (by `project_id` or `[[wikilink]]`), and renders a
//! header with a live progress bar plus the project's tasks through
//! [`task_ui::TasksApp`] (fully editable, write-through). Native has
//! no client yet (offline notice).

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use project::ProjectInfo;
use task::TaskInfo as DbTask;
use task_ui::{TaskInfo as UiTask, TaskMutation, TasksApp};
use uuid::Uuid;

use crate::orgs::{OrgMeta, OrgSelection};
use crate::routes::Route;
use crate::task_wiring::{handle, to_ui};
use threads::ui::ConversationsPanel;

/// The overview payload: project + its tasks + threads + connected repos,
/// loaded in one resource so they fetch concurrently.
type Overview = (
    ProjectInfo,
    String,
    Vec<DbTask>,
    Vec<threads::Thread>,
    Vec<git_proto::RepoId>,
);

#[component]
pub fn ProjectDetailView(id: String) -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let route_id = id.clone();
    // Bumped after a write (e.g. changing the project type) to re-fetch.
    let mut page_refresh = use_signal(|| 0u32);
    // Bumped after a thread is created or a message posted.
    let mut threads_refresh = use_signal(|| 0u32);
    let mut selected_thread = use_signal(|| Option::<Uuid>::None);

    // One resource loads the whole overview. After locating the project
    // (which carries its id + owning org), tasks + threads + connected
    // repos are fetched **concurrently** — no `use_effect → signal →
    // resource` waterfall gating a second round-trip wave behind a render
    // cycle. Works in "All" mode (searches the selected orgs by id).
    let data = use_resource(move || {
        let id = route_id.clone();
        let _ = page_refresh.read();
        let _ = threads_refresh.read();
        async move {
            let slugs = crate::orgs::selected_slugs(&selection.read(), &org_list.read());
            let (project, slug) = crate::feeds::find_project(&id, &slugs).await?;
            let pid = project.id;
            let (tasks_r, threads_r, repos_r) = futures_util::future::join3(
                crate::feeds::fetch_tasks_tagged(std::slice::from_ref(&slug)),
                crate::feeds::fetch_threads(&slug, "project", pid),
                crate::feeds::fetch_repos_for_project(&slug, pid),
            )
            .await;
            let tasks = tasks_r?.into_iter().map(|(_, t)| t).collect::<Vec<_>>();
            // Threads / repos are non-fatal: a forge or threads hiccup
            // shouldn't blank the whole overview.
            let threads = threads_r.unwrap_or_default();
            let repos = repos_r.unwrap_or_default();
            Ok::<Overview, String>((project, slug, tasks, threads, repos))
        }
    });

    let mut tasks = use_signal(Vec::<DbTask>::new);
    let mut org_of = use_signal(HashMap::<Uuid, String>::new);
    let mut project_slug = use_signal(String::new);
    let mut project_uuid = use_signal(|| Option::<Uuid>::None);
    use_effect(move || {
        if let Some(Ok((p, slug, rows, _, _))) = &*data.read_unchecked() {
            tasks.set(rows.clone());
            org_of.set(rows.iter().map(|t| (t.id, slug.clone())).collect());
            project_slug.set(slug.clone());
            project_uuid.set(Some(p.id));
        }
    });

    // Messages of the selected thread — separate so selecting a thread
    // doesn't refetch the whole overview.
    let messages_res = use_resource(move || {
        let _ = threads_refresh.read();
        let sel = *selected_thread.read();
        let slug = project_slug.read().clone();
        async move {
            match sel {
                Some(tid) if !slug.is_empty() => {
                    crate::feeds::fetch_thread_messages(&slug, tid).await
                }
                _ => Ok::<Vec<threads::Message>, String>(Vec::new()),
            }
        }
    });

    let proj = data.read();
    let body = match &*proj {
        Some(Ok((p, _slug, _tasks, threads_list, connected_repos))) => {
            let p = p.clone();
            let threads_list = threads_list.clone();
            let connected_repos = connected_repos.clone();
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

            let msgs = messages_res
                .read()
                .as_ref()
                .and_then(|r| r.as_ref().ok())
                .cloned()
                .unwrap_or_default();
            let sel = *selected_thread.read();
            let forge_slug = project_slug.read().clone();
            let kind = ProjectKind::from_str(&p.project_type);

            rsx! {
                Link { to: Route::ProjectsRoute {}, class: "text-xs text-muted-foreground hover:text-foreground",
                    "‹ Projects"
                }

                // ── Header ──────────────────────────────────────────────
                div { class: "flex flex-col gap-3 rounded-2xl border border-border bg-card/50 p-5",
                    div { class: "flex flex-wrap items-start justify-between gap-3",
                        div { class: "flex flex-col gap-1",
                            div { class: "flex items-center gap-2",
                                if !p.color.is_empty() {
                                    span { class: "h-3 w-3 shrink-0 rounded-full", style: "background:{p.color}" }
                                }
                                Heading { level: HeadingLevel::H1, "{p.title}" }
                            }
                            div { class: "flex flex-wrap items-center gap-x-2 gap-y-1 text-sm text-muted-foreground",
                                span { class: "capitalize", "{p.priority} priority" }
                                if !p.lead.is_empty() {
                                    span { "· lead {p.lead}" }
                                }
                                if let Some(d) = p.target_date {
                                    span { "· due {d}" }
                                }
                                if let Some(m) = p.date_modified {
                                    span { "· updated {m.date_naive()}" }
                                }
                            }
                        }
                        div { class: "flex items-center gap-2",
                            span { class: "rounded-md border border-border bg-muted/60 px-2 py-0.5 text-[11px] font-medium uppercase tracking-wide text-muted-foreground",
                                "{kind.label()}"
                            }
                            StatusBadge { variant: status_variant(&p.status), label: p.status.clone() }
                        }
                    }
                    div { class: "flex items-center gap-3",
                        div { class: "h-2 flex-1 overflow-hidden rounded-full bg-muted",
                            div { class: "h-full rounded-full bg-primary", style: "width: {pct}%" }
                        }
                        span { class: "shrink-0 text-xs font-medium text-muted-foreground", "{done}/{total} tasks · {pct:.0}%" }
                    }
                    if !p.tags.0.is_empty() {
                        div { class: "flex flex-wrap gap-1.5",
                            for tag in p.tags.0.iter() {
                                span { class: "rounded-full border border-border bg-muted/60 px-2 py-0.5 text-[11px] text-muted-foreground",
                                    "{tag}"
                                }
                            }
                        }
                    }
                }

                // ── Stat tiles ──────────────────────────────────────────
                div { class: "grid grid-cols-2 gap-3 sm:grid-cols-4",
                    StatTile { label: "Tasks", value: "{total}" }
                    StatTile { label: "Done", value: "{done}" }
                    StatTile { label: "Progress", value: "{pct:.0}%" }
                    StatTile { label: "Due", value: due_label(p.target_date) }
                }

                // ── Body grid: main + sidebar ───────────────────────────
                div { class: "grid grid-cols-1 gap-6 lg:grid-cols-3",
                    div { class: "flex flex-col gap-6 lg:col-span-2",
                        if !p.details.trim().is_empty() {
                            div { class: "rounded-xl border border-border bg-card/40 p-4",
                                Text { class: "whitespace-pre-line text-sm leading-relaxed", "{p.details}" }
                            }
                        }
                        // Code projects lead with issues & PRs.
                        if kind == ProjectKind::Code {
                            div { class: "flex flex-col gap-2",
                                Heading { level: HeadingLevel::H2, "Issues & Pull requests" }
                                if connected_repos.is_empty() {
                                    div { class: "rounded-xl border border-dashed border-border px-4 py-6 text-center",
                                        Text { variant: TextVariant::Muted, class: "text-sm",
                                            "Connect a repo (bind it to this project) to track issues & PRs here."
                                        }
                                    }
                                } else {
                                    for rid in connected_repos.iter() {
                                        crate::forge_views::ForgePanel {
                                            key: "{rid.owner}/{rid.repo}",
                                            slug: forge_slug.clone(),
                                            repo_id: rid.clone(),
                                        }
                                    }
                                }
                            }
                        }
                        div { class: "flex flex-col gap-2",
                            Heading { level: HeadingLevel::H2, "Tasks" }
                    if total == 0 {
                        Text { variant: TextVariant::Muted, "No tasks linked to this project yet." }
                    } else {
                        TasksApp {
                            tasks: mine,
                            on_event: move |mu: TaskMutation| {
                                let create_slug = project_slug.read().clone();
                                handle(&mut tasks, &mut org_of, &create_slug, mu);
                            },
                        }
                    }
                }
                ConversationsPanel {
                    threads: threads_list,
                    messages: msgs,
                    selected: sel,
                    on_select: move |id: Uuid| selected_thread.set(Some(id)),
                    on_new_thread: move |title: String| {
                        let slug = project_slug.read().clone();
                        let pid = *project_uuid.read();
                        let org_id = org_list
                            .read()
                            .iter()
                            .find(|o| o.slug == slug)
                            .and_then(|o| o.id)
                            .unwrap_or_else(Uuid::nil);
                        spawn(async move {
                            if let Some(pid) = pid {
                                let req = threads::CreateThreadRequest {
                                    org_id,
                                    entity_type: "project".into(),
                                    entity_id: pid,
                                    title,
                                    kind: String::new(),
                                    created_by: crate::chrome::owner_id(org_id),
                                    source_kind: "native".into(),
                                    source_ref: None,
                                    source_url: None,
                                };
                                if let Err(e) = crate::feeds::create_thread(&slug, req).await {
                                    tracing::warn!("create thread: {e}");
                                }
                                threads_refresh.with_mut(|r| *r += 1);
                            }
                        });
                    },
                    on_post: move |body: String| {
                        let slug = project_slug.read().clone();
                        let sel = *selected_thread.read();
                        let org_id = org_list
                            .read()
                            .iter()
                            .find(|o| o.slug == slug)
                            .and_then(|o| o.id)
                            .unwrap_or_else(Uuid::nil);
                        spawn(async move {
                            if let Some(tid) = sel {
                                let req = threads::PostMessageRequest {
                                    thread_id: tid,
                                    org_id,
                                    author_id: Some(crate::chrome::owner_id(org_id)),
                                    author_label: "me".into(),
                                    body,
                                    reply_to: None,
                                    source_kind: "native".into(),
                                    external_id: None,
                                    original_text: None,
                                    source_url: None,
                                    posted_at: None,
                                };
                                if let Err(e) = crate::feeds::post_thread_message(&slug, req).await {
                                    tracing::warn!("post message: {e}");
                                }
                                threads_refresh.with_mut(|r| *r += 1);
                            }
                        });
                    },
                }
                // General projects show the repo (when connected) after
                // their tasks/conversations. Code shows it up top; personal
                // hides it entirely.
                if kind == ProjectKind::General && !connected_repos.is_empty() {
                    div { class: "flex flex-col gap-2",
                        Heading { level: HeadingLevel::H2, "Repository" }
                        for rid in connected_repos.iter() {
                            crate::forge_views::ForgePanel {
                                key: "{rid.owner}/{rid.repo}",
                                slug: forge_slug.clone(),
                                repo_id: rid.clone(),
                            }
                        }
                    }
                }
                    }
                    // ── Sidebar: details ────────────────────────────────
                    div { class: "flex flex-col gap-4",
                        div { class: "flex flex-col gap-3 rounded-xl border border-border bg-card/40 p-4",
                            Heading { level: HeadingLevel::H3, "Details" }
                            // Editable project type / template.
                            div { class: "flex items-center justify-between gap-3 text-sm",
                                span { class: "shrink-0 text-muted-foreground", "Type" }
                                div { class: "flex gap-1",
                                    for k in ProjectKind::ALL {
                                        {
                                            let np_base = p.clone();
                                            let type_slug = forge_slug.clone();
                                            let is_current = k == kind;
                                            rsx! {
                                                Button {
                                                    key: "{k.slug()}",
                                                    variant: if is_current { ButtonVariant::Secondary } else { ButtonVariant::Ghost },
                                                    size: ButtonSize::Small,
                                                    on_click: move |_| {
                                                        let mut np = np_base.clone();
                                                        np.project_type = k.slug().to_string();
                                                        let slug = type_slug.clone();
                                                        spawn(async move {
                                                            if let Err(e) = crate::feeds::update_project(&slug, np).await {
                                                                tracing::warn!("update project type: {e}");
                                                            }
                                                            page_refresh.with_mut(|x| *x += 1);
                                                        });
                                                    },
                                                    "{k.label()}"
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                            DetailRow { label: "Status".to_string(), value: p.status.clone() }
                            DetailRow { label: "Priority".to_string(), value: p.priority.clone() }
                            if !p.lead.is_empty() {
                                DetailRow { label: "Lead".to_string(), value: p.lead.clone() }
                            }
                            DetailRow { label: "Due".to_string(), value: due_label(p.target_date) }
                            if p.estimated_seconds > 0 {
                                DetailRow { label: "Estimate".to_string(), value: hours_label(p.estimated_seconds) }
                            }
                            if let Some(c) = p.date_created {
                                DetailRow { label: "Created".to_string(), value: c.date_naive().to_string() }
                            }
                            if let Some(m) = p.date_modified {
                                DetailRow { label: "Updated".to_string(), value: m.date_naive().to_string() }
                            }
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
        div { class: "mx-auto w-full max-w-6xl flex flex-col gap-6 p-4 sm:p-6 lg:p-10", {body} }
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

/// A big-number overview tile (Tasks / Done / Progress / Due).
#[component]
fn StatTile(label: String, value: String) -> Element {
    rsx! {
        div { class: "flex flex-col gap-0.5 rounded-xl border border-border bg-card/40 p-3",
            span { class: "text-2xl font-semibold tabular-nums", "{value}" }
            span { class: "text-[11px] uppercase tracking-wide text-muted-foreground", "{label}" }
        }
    }
}

/// A label/value row in the sidebar Details card.
#[component]
fn DetailRow(label: String, value: String) -> Element {
    rsx! {
        div { class: "flex items-baseline justify-between gap-3 text-sm",
            span { class: "shrink-0 text-muted-foreground", "{label}" }
            span { class: "text-right font-medium", "{value}" }
        }
    }
}

fn due_label(d: Option<chrono::NaiveDate>) -> String {
    d.map_or_else(|| "—".to_string(), |d| d.to_string())
}

fn hours_label(secs: i64) -> String {
    format!("{:.1}h", secs as f64 / 3600.0)
}

/// Project type → overview layout. Free-form string under the hood;
/// unknown / empty ⇒ General.
#[derive(Clone, Copy, PartialEq, Eq)]
enum ProjectKind {
    Code,
    General,
    Personal,
}

impl ProjectKind {
    fn from_str(s: &str) -> Self {
        match s {
            "code" => Self::Code,
            "personal" => Self::Personal,
            _ => Self::General,
        }
    }

    fn label(self) -> &'static str {
        match self {
            Self::Code => "Code",
            Self::General => "General",
            Self::Personal => "Personal",
        }
    }

    const ALL: [Self; 3] = [Self::Code, Self::General, Self::Personal];

    fn slug(self) -> &'static str {
        match self {
            Self::Code => "code",
            Self::General => "general",
            Self::Personal => "personal",
        }
    }
}

fn status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "done" | "completed" | "active" => StatusBadgeVariant::Success,
        "on_hold" | "on-hold" | "paused" => StatusBadgeVariant::Warning,
        "cancelled" | "canceled" | "archived" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}
