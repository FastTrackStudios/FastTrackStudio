//! `/` — the home dashboard.
//!
//! Currently-active projects, each with the single next task to do
//! (soonest-due open task, priority as tiebreak). Honors the org
//! switcher: `All` aggregates every hosted org, or scope to one.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{CalendarDays, CircleCheck};
use fts_ui::prelude::*;
use project::ProjectInfo;
use task::TaskInfo as DbTask;

use crate::orgs::{OrgMeta, OrgSelection, selected_slugs};
use crate::routes::Route;

#[component]
pub fn HomeView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    let data = use_resource(move || async move {
        let slugs = selected_slugs(&selection.read(), &org_list.read());
        let (pr, tr) = futures_util::future::join(
            crate::feeds::fetch_projects(&slugs),
            crate::feeds::fetch_tasks(&slugs),
        )
        .await;
        Ok::<(Vec<ProjectInfo>, Vec<DbTask>), String>((pr?, tr?))
    });

    // Until the org list is discovered, the fetch resolves to an empty
    // set — show the loading state rather than a misleading "nothing
    // active".
    let discovering = org_list.read().is_empty();
    let view = if discovering {
        render_loading()
    } else {
        match &*data.read() {
            Some(Ok((projects, tasks))) => render_loaded(projects, tasks),
            Some(Err(e)) => rsx! {
                div { class: "rounded-xl border border-destructive/40 bg-destructive/10 px-4 py-3 text-sm",
                    "Couldn't load your workspace: {e}"
                }
            },
            None => render_loading(),
        }
    };

    rsx! {
        div { class: "mx-auto flex w-full max-w-3xl flex-col gap-6 p-4 sm:p-6 lg:p-10",
            header { class: "flex flex-col gap-1",
                span { class: "text-[0.7rem] font-semibold uppercase tracking-[0.18em] text-muted-foreground",
                    "Workspace"
                }
                Heading { level: HeadingLevel::H1, class: "tracking-tight", "Active work" }
                Text {
                    variant: TextVariant::Muted,
                    "Your active projects and the next thing to do in each."
                }
            }
            {view}
        }
    }
}

fn render_loading() -> Element {
    rsx! {
        div { class: "flex flex-col gap-3",
            for _ in 0..4 {
                div { class: "flex flex-col gap-3 rounded-xl border border-border/70 bg-card p-4",
                    div { class: "h-5 w-40 animate-pulse rounded-md bg-muted" }
                    div { class: "h-4 w-full animate-pulse rounded-md bg-muted" }
                }
            }
        }
    }
}

fn render_loaded(projects: &[ProjectInfo], tasks: &[DbTask]) -> Element {
    // Pair each active project with its single next task; drop projects
    // that have nothing to do — the home page is only "what's next".
    let mut active: Vec<(&ProjectInfo, DbTask)> = projects
        .iter()
        .filter(|p| !p.archived && is_active(&p.status))
        .filter_map(|p| next_task(p, tasks).map(|t| (p, t.clone())))
        .collect();
    // Soonest due first (undated last), then project title.
    active.sort_by(|(pa, ta), (pb, tb)| {
        match (ta.due.clone(), tb.due.clone()) {
            (Some(x), Some(y)) => x.cmp(&y),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => std::cmp::Ordering::Equal,
        }
        .then_with(|| pa.title.to_lowercase().cmp(&pb.title.to_lowercase()))
    });

    if active.is_empty() {
        return rsx! {
            div { class: "flex flex-col items-center gap-3 rounded-2xl border border-dashed border-border/70 bg-card/40 px-6 py-16 text-center",
                div { class: "flex size-12 items-center justify-center rounded-2xl bg-muted text-muted-foreground",
                    CircleCheck { size: 24 }
                }
                Heading { level: HeadingLevel::H3, "You're all caught up" }
                Text { variant: TextVariant::Muted, "No active projects have open tasks right now." }
            }
        };
    }

    rsx! {
        div { class: "flex flex-col gap-3",
            for (p, t) in active.into_iter() {
                {
                    let pid = p.id.to_string();
                    let title = p.title.clone();
                    rsx! {
                        div {
                            key: "{p.id}",
                            class: "group flex flex-col gap-3 rounded-xl border border-border/70 bg-card/70 p-4 transition-colors hover:border-border",
                            div { class: "flex items-center justify-between gap-2",
                                Link {
                                    to: Route::ProjectDetailRoute { id: pid },
                                    class: "min-w-0 text-sm font-semibold text-foreground hover:underline",
                                    span { class: "truncate", "{title}" }
                                }
                                StatusBadge {
                                    variant: status_variant(&p.status),
                                    label: p.status.clone(),
                                }
                            }
                            NextTask { task: t }
                        }
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct NextTaskProps {
    task: DbTask,
}

#[component]
fn NextTask(props: NextTaskProps) -> Element {
    let t = &props.task;
    let due = t.due.as_deref().and_then(parse_due);
    rsx! {
        div { class: "flex items-center gap-2.5 rounded-lg bg-muted/30 px-3 py-2.5",
            span { class: "size-4 shrink-0 rounded-md border-2 border-border" }
            span { class: "min-w-0 flex-1 truncate text-sm text-foreground", "{t.title}" }
            if let Some((label, cls)) = due {
                span { class: "inline-flex shrink-0 items-center gap-1 rounded-full px-2 py-0.5 text-[11px] {cls}",
                    CalendarDays { size: 11 }
                    "{label}"
                }
            }
        }
    }
}

// ── helpers ─────────────────────────────────────────────────────────

fn is_active(status: &str) -> bool {
    matches!(status, "active" | "open" | "in_progress")
}

fn is_open_task(t: &DbTask) -> bool {
    !matches!(
        t.status.as_str(),
        "done" | "complete" | "completed" | "shipped" | "cancelled" | "canceled" | "abandoned"
    )
}

/// A task belongs to a project by `project_id` or a `[[title]]` /
/// bare-title entry in its `projects:` list.
fn belongs(t: &DbTask, p: &ProjectInfo) -> bool {
    if t.project_id == Some(p.id) {
        return true;
    }
    let link = format!("[[{}]]", p.title);
    t.projects.0.iter().any(|x| x == &link || x == &p.title)
}

fn priority_rank(p: &str) -> u8 {
    match p {
        "p0" | "urgent" | "critical" => 0,
        "p1" | "high" => 1,
        "p2" | "normal" | "" => 2,
        "p3" | "low" => 3,
        _ => 4,
    }
}

/// The single next task for a project: open, soonest due (None last),
/// then highest priority, then title.
fn next_task<'a>(p: &ProjectInfo, tasks: &'a [DbTask]) -> Option<&'a DbTask> {
    let mut candidates: Vec<&DbTask> = tasks
        .iter()
        .filter(|t| is_open_task(t) && belongs(t, p))
        .collect();
    candidates.sort_by(|a, b| {
        match (a.due.clone(), b.due.clone()) {
            (Some(x), Some(y)) => x.cmp(&y),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => std::cmp::Ordering::Equal,
        }
        .then_with(|| priority_rank(&a.priority).cmp(&priority_rank(&b.priority)))
        .then_with(|| a.title.to_lowercase().cmp(&b.title.to_lowercase()))
    });
    candidates.into_iter().next()
}

/// Format an ISO `YYYY-MM-DD` due string into a relative label + a
/// token-based pill class (overdue → destructive, today → amber).
fn parse_due(s: &str) -> Option<(String, &'static str)> {
    let d = chrono::NaiveDate::parse_from_str(s.trim(), "%Y-%m-%d").ok()?;
    let today = chrono::Local::now().date_naive();
    let label = if d == today {
        "Today".to_string()
    } else if d == today + chrono::Duration::days(1) {
        "Tomorrow".to_string()
    } else {
        d.format("%b %-d").to_string()
    };
    let cls = match d.cmp(&today) {
        std::cmp::Ordering::Less => {
            "border border-destructive/50 bg-destructive/15 text-destructive"
        }
        std::cmp::Ordering::Equal => "border border-amber-400/50 bg-amber-500/15 text-amber-200",
        std::cmp::Ordering::Greater => "border border-border bg-muted/40 text-muted-foreground",
    };
    Some((label, cls))
}

fn status_variant(status: &str) -> StatusBadgeVariant {
    match status {
        "active" | "open" | "in_progress" => StatusBadgeVariant::Success,
        "on_hold" | "on-hold" | "paused" | "waiting" => StatusBadgeVariant::Warning,
        "cancelled" | "canceled" | "abandoned" | "blocked" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Neutral,
    }
}
