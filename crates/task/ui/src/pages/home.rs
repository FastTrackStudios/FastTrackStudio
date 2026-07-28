//! `/home` — the project dashboard.
//!
//! Situational awareness across every active project: a compact grid
//! where each card carries the project's pulse (status, done/total
//! progress) and its **first action** — the soonest-due open task,
//! behind the same three-state checkbox as the board, so the next
//! thing can be started (timer and all) or completed without leaving
//! the dashboard. Honors the org switcher: `All` aggregates every
//! hosted org, or scope to one.
//!
//! Store-backed like every route page (`plans/atom-store-migration.md`)
//! — checkbox clicks are optimistic `TaskMutations` against the shared
//! task store, so the board and the dashboard can't disagree.

use crate::format::status_variant;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{CalendarDays, CircleCheck};
use fts_ui::prelude::*;
use project_proto::ProjectInfo;
use task_proto::TaskInfo as DbTask;

use crate::routes::Route;
use crate::stores;
use crate::task_sort::{belongs, is_active, is_open_task, priority_rank};

/// One dashboard card's data: an active project with its task tally
/// and single next action. Derived in a memo so the
/// filter/sort/next-task walk over every project × task re-runs on
/// data changes only, not every render.
#[derive(Clone, PartialEq)]
struct Card {
    project: ProjectInfo,
    next: DbTask,
    done: usize,
    total: usize,
}

#[component]
pub fn HomeView() -> Element {
    let projects = stores::use_project_list();
    let tasks = stores::use_task_list();
    let muts = stores::use_task_mutations();
    let project_muts = stores::use_project_mutations();
    let selection = use_context::<Signal<crate::orgs::OrgSelection>>();
    let org_list = use_context::<Signal<Vec<crate::orgs::OrgMeta>>>();
    let project_store = stores::use_project_store();
    let task_store = stores::use_task_store();

    let cards = use_memo(move || {
        let projects = project_store.list();
        let tasks = task_store.list();
        let project_refs: Vec<&ProjectInfo> = projects.iter().map(|r| &r.project).collect();
        let task_refs: Vec<&DbTask> = tasks.iter().map(|r| &r.task).collect();
        build_cards(&project_refs, &task_refs)
    });

    let view = match (
        projects.value().as_ref(),
        tasks.value().as_ref(),
        projects.error().or(tasks.error()),
    ) {
        (Some(_), Some(_), _) => {
            let org_choices: Vec<(String, String)> = org_list
                .read()
                .iter()
                .map(|o| (o.slug.clone(), o.name.clone()))
                .collect();
            let quick_add = rsx! {
                super::projects::ProjectQuickAdd {
                    compact: true,
                    orgs: org_choices,
                    default_slug: crate::orgs::create_target(&selection.read(), &org_list.read()),
                    on_create: move |(slug, title): (String, String)| {
                        project_muts.create(slug, stores::draft_project(title));
                    },
                }
            };
            render_loaded(cards(), quick_add, move |id, status| {
                muts.apply(
                    &crate::orgs::create_target(&selection.read(), &org_list.read()),
                    task_ui::TaskMutation::SetStatus { id, status },
                );
            })
        }
        (_, _, Some(e)) => rsx! {
            div { class: "rounded-xl border border-destructive/40 bg-destructive/10 px-4 py-3 text-sm",
                "Couldn't load your workspace: {e}"
            }
        },
        _ => render_loading(),
    };

    rsx! {
        div { class: "mx-auto flex w-full max-w-6xl flex-col gap-4 p-3 sm:p-5 lg:px-8 lg:py-6",
            {view}
        }
    }
}

fn render_loading() -> Element {
    rsx! {
        div { class: "grid grid-cols-1 gap-3 md:grid-cols-2 xl:grid-cols-3",
            for i in 0..6 {
                div { key: "{i}", class: "flex flex-col gap-3 rounded-xl border border-border/70 bg-card p-4",
                    div { class: "h-5 w-40 animate-pulse rounded-md bg-muted" }
                    div { class: "h-1.5 w-full animate-pulse rounded-full bg-muted" }
                    div { class: "h-4 w-full animate-pulse rounded-md bg-muted" }
                }
            }
        }
    }
}

/// Each active project with its task tally + single next action;
/// projects with nothing open drop out — the dashboard is only
/// "what's next".
fn build_cards(projects: &[&ProjectInfo], tasks: &[&DbTask]) -> Vec<Card> {
    let mut cards: Vec<Card> = projects
        .iter()
        .filter(|p| !p.archived && is_active(&p.status))
        .filter_map(|p| {
            let mine: Vec<&&DbTask> = tasks.iter().filter(|t| belongs(t, p)).collect();
            let total = mine.len();
            let done = mine.iter().filter(|t| !is_open_task(t)).count();
            next_task(p, tasks).map(|t| Card {
                project: (*p).clone(),
                next: t.clone(),
                done,
                total,
            })
        })
        .collect();
    // Soonest due first (undated last), then project title.
    cards.sort_by(|a, b| {
        match (a.next.due.as_deref(), b.next.due.as_deref()) {
            (Some(x), Some(y)) => x.cmp(y),
            (Some(_), None) => std::cmp::Ordering::Less,
            (None, Some(_)) => std::cmp::Ordering::Greater,
            (None, None) => std::cmp::Ordering::Equal,
        }
        .then_with(|| {
            a.project
                .title
                .to_lowercase()
                .cmp(&b.project.title.to_lowercase())
        })
    });
    cards
}

fn render_loaded(
    cards: Vec<Card>,
    quick_add: Element,
    on_status: impl Fn(uuid::Uuid, String) + Copy + 'static,
) -> Element {
    let due_this_week = cards
        .iter()
        .filter(|c| {
            c.next.due.as_deref().is_some_and(|d| {
                chrono::NaiveDate::parse_from_str(d.trim(), "%Y-%m-%d").is_ok_and(|d| {
                    let today = chrono::Local::now().date_naive();
                    d <= today + chrono::Duration::days(7)
                })
            })
        })
        .count();

    if cards.is_empty() {
        return rsx! {
            div { class: "flex flex-col items-center gap-3 rounded-2xl border border-dashed border-border/70 bg-card/40 px-6 py-16 text-center",
                div { class: "flex size-12 items-center justify-center rounded-2xl bg-muted text-muted-foreground",
                    CircleCheck { size: 24 }
                }
                Heading { level: HeadingLevel::H3, "You're all caught up" }
                Text { variant: TextVariant::Muted, "No active projects have open tasks right now." }
                {quick_add}
            }
        };
    }

    rsx! {
        // One header line: what this page is + the week at a glance +
        // the door for new work — projects get made where they're seen.
        div { class: "flex flex-wrap items-center gap-x-3 gap-y-2",
            Heading { level: HeadingLevel::H1, class: "tracking-tight", "Active work" }
            span { class: "text-xs tabular-nums text-muted-foreground",
                "{cards.len()} projects"
                if due_this_week > 0 {
                    " · {due_this_week} due within a week"
                }
            }
            div { class: "ml-auto",
                {quick_add}
            }
        }
        div { class: "grid grid-cols-1 gap-3 md:grid-cols-2 xl:grid-cols-3",
            for card in cards.into_iter() {
                ProjectCard {
                    key: "{card.project.id}",
                    project: card.project,
                    next: card.next,
                    done: card.done,
                    total: card.total,
                    on_status: move |(id, status)| on_status(id, status),
                }
            }
        }
    }
}

/// One project's pulse: title + status, a thin done/total progress
/// bar, and the first action behind a live checkbox.
#[component]
fn ProjectCard(
    project: ProjectInfo,
    next: DbTask,
    done: usize,
    total: usize,
    on_status: EventHandler<(uuid::Uuid, String)>,
) -> Element {
    let pid = project.id.to_string();
    let pct = if total == 0 { 0 } else { done * 100 / total };
    let due = next.due.as_deref().and_then(parse_due);
    let next_id = next.id;
    let next_status = next.status.clone();
    let ui_status = task_ui::Status::from_str(&next.status).unwrap_or(task_ui::Status::Open);
    let ui_priority =
        task_ui::Priority::from_str(&next.priority).unwrap_or(task_ui::Priority::Normal);

    rsx! {
        div { class: "group flex flex-col gap-2.5 rounded-xl border border-border/70 bg-card/70 p-3.5 transition-colors hover:border-border",
            div { class: "flex items-center justify-between gap-2",
                Link {
                    to: Route::ProjectDetailRoute { id: pid },
                    class: "min-w-0 text-sm font-semibold text-foreground hover:underline",
                    span { class: "truncate", "{project.title}" }
                }
                StatusBadge {
                    variant: status_variant(&project.status),
                    label: project.status.clone(),
                }
            }
            // Done/total as a hairline bar — the project's pulse in
            // 6 vertical pixels.
            div { class: "flex items-center gap-2",
                div { class: "h-1.5 min-w-0 flex-1 overflow-hidden rounded-full bg-muted/20",
                    div {
                        class: "h-full rounded-full bg-primary transition-[width]",
                        style: "width: {pct}%",
                    }
                }
                span { class: "shrink-0 text-[11px] tabular-nums text-muted-foreground",
                    "{done}/{total}"
                }
            }
            // The first action — live checkbox, same click cycle as
            // the board (start the clock, complete, reopen).
            div { class: "flex items-center gap-2.5 rounded-lg bg-muted/30 px-2.5 py-2",
                task_ui::CheckboxButton {
                    status: ui_status,
                    priority: ui_priority,
                    on_click: move |()| {
                        let s = task_proto::click_transition(&next_status, None);
                        on_status.call((next_id, s.to_string()));
                    },
                }
                span { class: "min-w-0 flex-1 truncate text-sm text-foreground", "{next.title}" }
                if let Some((label, cls)) = due {
                    span { class: "inline-flex shrink-0 items-center gap-1 rounded-full px-2 py-0.5 text-[11px] {cls}",
                        CalendarDays { size: 11 }
                        "{label}"
                    }
                }
            }
        }
    }
}

// ── helpers ─────────────────────────────────────────────────────────

/// The single next task for a project: open, soonest due (None last),
/// then highest priority, then title.
fn next_task<'a>(p: &ProjectInfo, tasks: &[&'a DbTask]) -> Option<&'a DbTask> {
    let mut candidates: Vec<&DbTask> = tasks
        .iter()
        .filter(|t| is_open_task(t) && belongs(t, p))
        .copied()
        .collect();
    candidates.sort_by(|a, b| {
        match (a.due.as_deref(), b.due.as_deref()) {
            (Some(x), Some(y)) => x.cmp(y),
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
