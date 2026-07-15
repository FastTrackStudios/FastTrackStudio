//! `/tasks` — live task workspace over the org's `TaskService`.
//!
//! Thin page: the shared task store ([`crate::stores`]) loads the
//! selected orgs' tasks as one `AtomResult` (slug-tagged rows, so
//! "All" mode routes each edit back to its owning org), renders
//! [`task_ui::TasksApp`] through the shared forward converter, and
//! applies board mutations through the optimistic
//! [`crate::stores::TaskMutations`] (instant patch, reconcile or
//! rollback + tray notification).
//!
//! Defaults to **Active + Relevant** (`plans/relevancy-and-inbox.md`):
//! the filtering itself is `task::relevance` — the same domain
//! functions the server's `TaskService::query` and the CLI's
//! `task task list --relevant` run — this page only assembles the
//! [`task::RelevanceContext`] (browser clock + the running timer
//! session's project) and renders toggle chips. Toggles persist
//! per-account ([`crate::prefs`]).
//!
//! The page opens with the **Now bar**: the in-progress task, its
//! live clock, and a complete button — the checkbox-timer made
//! spatial. Idle (nothing running) renders no bar at all.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Plus, SlidersHorizontal};
use task_ui::{QuickAdd, TaskInfo as UiTask, TaskMutation, TasksApp};

use crate::chrome::use_second_tick;
use crate::orgs::{OrgMeta, OrgSelection};
use crate::prefs::PrefsCtx;
use crate::shell::mobile::{BottomSheet, MobileActionBar};
use crate::stores;

/// The "I'm at:" choices — matched against `@<location>` gate
/// contexts by the relevance rules. FUTURE: personal named
/// locations on the prefs entity.
const LOCATIONS: &[&str] = &["home", "studio", "out"];

/// Everything the loaded board renders, derived once per **data**
/// change (tasks / projects / sessions / prefs) instead of on every
/// render of [`TasksView`] — the filter → condense → sort → convert
/// pipeline walks and clones the whole task list.
#[derive(Clone, PartialEq)]
struct BoardData {
    ui_tasks: Vec<UiTask>,
    /// `(id, title)` of every known project — chip resolution + the
    /// quick-add `[[Project]]` picker.
    projects: Vec<(uuid::Uuid, String)>,
    /// Rows the Active/Relevant chips filtered out.
    hidden: usize,
    /// The in-progress task with a live time entry, if any.
    running: Option<UiTask>,
}

#[component]
pub fn TasksView() -> Element {
    let nav = use_navigator();
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let prefs_ctx = use_context::<PrefsCtx>();

    // Mobile sheets: filters + quick-add, opened from the sticky
    // bottom action bar (thumb reach) below `md`.
    let mut filters_open = use_signal(|| false);
    let mut add_open = use_signal(|| false);

    // The list hooks drive the fetches; the derivation below reads the
    // backing stores directly (inside the memo) so it re-runs on data
    // changes only.
    let result = stores::use_task_list();
    let muts = stores::use_task_mutations();
    let store = stores::use_task_store();
    let _sessions = stores::use_session_list();
    let _projects = stores::use_project_list();
    let session_store = stores::use_session_store();
    let project_store = stores::use_project_store();

    // Server-backed per-user prefs — reading the signal makes the
    // chips follow the account (switch users, keep your setup).
    let user_prefs = prefs_ctx.prefs.read().clone();
    let active_only = user_prefs.tasks_active;
    let relevant_only = user_prefs.tasks_relevant;
    let at_location = user_prefs.location.clone();

    let prefs_signal = prefs_ctx.prefs;
    let board = use_memo(move || -> Option<BoardData> {
        let result = store.entries_result();
        let rows = result.value()?;

        let prefs = prefs_signal.read();
        let active_only = prefs.tasks_active;
        let relevant_only = prefs.tasks_relevant;
        let at_location = prefs.location.clone();
        drop(prefs);

        // The running timer session's project — relevance boosts its
        // sibling tasks to the top ("if my timer is on a project,
        // prioritize that project's items").
        let active_project = session_store
            .list()
            .iter()
            .find(|r| r.session.end_time.is_none())
            .and_then(|r| r.session.project_id);

        let now = chrono::Local::now();
        let ctx = task::RelevanceContext {
            local_hhmm: Some(now.format("%H:%M").to_string()),
            local_date: Some(now.format("%Y-%m-%d").to_string()),
            // FUTURE(plans/relevancy-and-inbox.md): device from
            // user-agent.
            location: (!at_location.is_empty()).then(|| at_location.clone()),
            device: None,
            active_project,
        };
        let mut domain: Vec<&task::TaskInfo> = rows.iter().map(|(_, r)| &r.task).collect();
        let total = domain.len();
        if active_only {
            domain.retain(|t| task::status_is_open(&t.status));
        }
        if relevant_only {
            domain.retain(|t| task::is_relevant(t, &ctx));
            // One next action per project — the Relevant view is
            // "what would I do right now", not the project backlog.
            task::condense_next_per_project(&mut domain);
            domain.sort_by_key(|t| task::relevance_rank(t, &ctx));
        }
        let hidden = total - domain.len();
        // Project indication: resolve the authoritative project_id
        // to its title when the frontmatter wikilink array is empty,
        // so every project task carries its #project chip.
        let project_names: std::collections::HashMap<uuid::Uuid, String> = project_store
            .list()
            .iter()
            .map(|r| (r.project.id, r.project.title.clone()))
            .collect();
        let ui_tasks: Vec<UiTask> = domain
            .iter()
            .map(|t| {
                let mut ui = stores::to_ui(t);
                if ui.projects.is_empty() {
                    if let Some(name) = t.project_id.and_then(|id| project_names.get(&id)) {
                        ui.projects.push(name.clone());
                    }
                }
                ui
            })
            .collect();

        // The running task (in-progress with a live entry) — the
        // whole board's rows are candidates, filters or not: a
        // running clock is never invisible.
        let running: Option<UiTask> = rows
            .iter()
            .map(|(_, r)| &r.task)
            .find(|t| {
                task::Status::from_str(&t.status) == Some(task::Status::InProgress)
                    && t.time_entries.0.iter().any(|e| e.end_time.is_none())
            })
            .map(|t| stores::to_ui(t));

        Some(BoardData {
            ui_tasks,
            projects: project_names.into_iter().collect(),
            hidden,
            running,
        })
    });

    let body = match (board(), result.error()) {
        (Some(data), _) => {
            let BoardData {
                ui_tasks,
                projects: project_choices,
                hidden,
                running,
            } = data;
            let location_value = at_location.clone();
            // Inline chips are desktop chrome; on phones the same
            // controls live in the Filters bottom sheet below.
            let chips = rsx! {
                div { class: "hidden items-center gap-1.5 md:flex",
                    FilterChip {
                        label: "Active",
                        title: "Hide done/cancelled tasks",
                        on: active_only,
                        on_toggle: move |on| prefs_ctx.update(|p| p.tasks_active = on),
                    }
                    FilterChip {
                        label: "Relevant",
                        title: "Only what matters right now — routines in their time windows, deadlines always, timer project first",
                        on: relevant_only,
                        on_toggle: move |on| prefs_ctx.update(|p| p.tasks_relevant = on),
                    }
                    // "I'm at:" — feeds the @location relevance gates
                    // and persists per user.
                    select {
                        class: "rounded-full border border-border bg-transparent px-2 py-0.5 text-xs text-muted-foreground outline-none focus:border-primary/50",
                        title: "Where you are — @home/@studio/@out tasks gate on this",
                        value: "{location_value}",
                        onchange: move |e| prefs_ctx.update(|p| p.location = e.value()),
                        option { value: "", selected: location_value.is_empty(), "anywhere" }
                        for loc in LOCATIONS {
                            option { key: "{loc}", value: "{loc}", selected: location_value == *loc, "@{loc}" }
                        }
                    }
                    if hidden > 0 {
                        span { class: "text-xs tabular-nums text-muted-foreground", "{hidden} hidden" }
                    }
                }
            };
            // How many filters are narrowing the board — badged on the
            // mobile Filters trigger so a filtered board is never a
            // mystery on a phone.
            let filter_count = usize::from(active_only)
                + usize::from(relevant_only)
                + usize::from(!at_location.is_empty());
            let sheet_location = at_location.clone();
            let sheet_projects = project_choices.clone();
            rsx! {
                div { class: "flex h-full w-full flex-col pb-14 md:pb-0",
                    if let Some(t) = running {
                        NowBar {
                            task: t,
                            on_complete: move |id| {
                                muts.apply(
                                    &crate::orgs::create_target(&selection.read(), &org_list.read()),
                                    TaskMutation::SetStatus { id, status: "done".into() },
                                );
                            },
                        }
                    }
                    div { class: "min-h-0 flex-1",
                        TasksApp {
                            tasks: ui_tasks,
                            header_extra: chips,
                            projects: project_choices,
                            on_event: move |mu: TaskMutation| {
                                let create_slug =
                                    crate::orgs::create_target(&selection.read(), &org_list.read());
                                muts.apply(&create_slug, mu);
                            },
                            on_open_full: move |id| {
                                nav.push(crate::routes::Route::TaskDetailRoute { id });
                            },
                        }
                    }
                    // ── Mobile: sticky actions + sheets ────────────────
                    MobileActionBar {
                        button {
                            r#type: "button",
                            class: "flex min-h-11 flex-1 items-center justify-center gap-2 rounded-lg border border-border px-3 py-2 text-sm font-medium text-foreground active:bg-accent",
                            onclick: move |_| filters_open.set(true),
                            SlidersHorizontal { size: 16 }
                            if filter_count > 0 { "Filters · {filter_count}" } else { "Filters" }
                        }
                        button {
                            r#type: "button",
                            class: "flex min-h-11 flex-1 items-center justify-center gap-2 rounded-lg bg-primary px-3 py-2 text-sm font-medium text-primary-foreground active:bg-primary/85",
                            onclick: move |_| add_open.set(true),
                            Plus { size: 16 }
                            "Add task"
                        }
                    }
                    BottomSheet {
                        open: filters_open(),
                        title: "Filters".to_string(),
                        on_close: move |()| filters_open.set(false),
                        div { class: "flex flex-col gap-3 pb-2",
                            SheetToggleRow {
                                label: "Active",
                                hint: "Hide done and cancelled tasks",
                                on: active_only,
                                on_toggle: move |on| prefs_ctx.update(|p| p.tasks_active = on),
                            }
                            SheetToggleRow {
                                label: "Relevant",
                                hint: "Only what matters right now",
                                on: relevant_only,
                                on_toggle: move |on| prefs_ctx.update(|p| p.tasks_relevant = on),
                            }
                            div { class: "flex flex-col gap-1.5",
                                span { class: "px-1 text-xs font-semibold uppercase tracking-widest text-muted-foreground",
                                    "I'm at"
                                }
                                select {
                                    class: "min-h-11 w-full rounded-lg border border-border bg-background px-3 py-2 text-sm text-foreground outline-none focus:border-primary/50",
                                    value: "{sheet_location}",
                                    onchange: move |e| prefs_ctx.update(|p| p.location = e.value()),
                                    option { value: "", selected: sheet_location.is_empty(), "anywhere" }
                                    for loc in LOCATIONS {
                                        option { key: "{loc}", value: "{loc}", selected: sheet_location == *loc, "@{loc}" }
                                    }
                                }
                            }
                            if hidden > 0 {
                                span { class: "px-1 text-xs tabular-nums text-muted-foreground",
                                    "{hidden} tasks hidden by these filters"
                                }
                            }
                        }
                    }
                    BottomSheet {
                        open: add_open(),
                        title: "Add task".to_string(),
                        on_close: move |()| add_open.set(false),
                        div { class: "pb-2",
                            QuickAdd {
                                projects: sheet_projects,
                                on_create: move |task: task_ui::TaskInfo| {
                                    let create_slug =
                                        crate::orgs::create_target(&selection.read(), &org_list.read());
                                    muts.apply(&create_slug, TaskMutation::Create { task });
                                    add_open.set(false);
                                },
                            }
                        }
                    }
                }
            }
        }
        (None, Some(e)) => rsx! {
            crate::states::ErrorState {
                title: "Couldn't reach the task service",
                message: e,
                on_retry: move |()| store.reload(),
            }
        },
        (None, None) => rsx! { crate::states::LoadingState {} },
    };

    rsx! {
        div { class: "h-full w-full", {body} }
    }
}

/// The page's opening statement: what's running right now. A slim
/// full-width strip — pulsing dot, task title, live elapsed clock,
/// one button to land it. Rendered only while a clock is live, so an
/// idle board stays clean.
#[component]
fn NowBar(task: UiTask, on_complete: EventHandler<uuid::Uuid>) -> Element {
    let tick = use_signal(|| 0u64);
    use_second_tick(tick);
    let _ = tick(); // subscribe: the clock re-renders each second

    let id = task.id;
    let elapsed = task.tracked_seconds(chrono::Utc::now());
    rsx! {
        div { class: "flex items-center gap-3 border-b border-sky-500/30 bg-sky-500/10 px-4 py-2 sm:px-6 lg:px-8",
            span { class: "h-2 w-2 shrink-0 animate-pulse rounded-full bg-sky-400" }
            span { class: "min-w-0 truncate text-sm font-medium text-foreground", "{task.title}" }
            span { class: "shrink-0 font-mono text-sm tabular-nums text-sky-300",
                {task_ui::clock_label(elapsed)}
            }
            button {
                r#type: "button",
                class: "ml-auto shrink-0 rounded-md border border-sky-500/40 px-2.5 py-0.5 text-xs font-medium text-sky-200 transition-colors hover:bg-sky-500/20",
                onclick: move |_| on_complete.call(id),
                "Complete"
            }
        }
    }
}

/// Touch-sized on/off row for the mobile Filters sheet — the same
/// toggles the desktop chips drive, at thumb size (≥44px), with a
/// switch-style state indicator.
#[component]
fn SheetToggleRow(
    label: &'static str,
    hint: &'static str,
    on: bool,
    on_toggle: EventHandler<bool>,
) -> Element {
    let (track, knob) = if on {
        ("bg-primary", "translate-x-4")
    } else {
        ("bg-muted", "translate-x-0")
    };
    rsx! {
        button {
            r#type: "button",
            class: "flex min-h-12 w-full items-center justify-between gap-3 rounded-lg border border-border/60 bg-card/40 px-3 py-2 text-left active:bg-accent",
            onclick: move |_| on_toggle.call(!on),
            div { class: "flex min-w-0 flex-col",
                span { class: "text-sm font-medium text-foreground", "{label}" }
                span { class: "text-xs text-muted-foreground", "{hint}" }
            }
            span { class: "relative h-5 w-9 shrink-0 rounded-full transition-colors {track}",
                span { class: "absolute left-0.5 top-0.5 h-4 w-4 rounded-full bg-background shadow transition-transform {knob}" }
            }
        }
    }
}

/// Small on/off filter chip.
#[component]
fn FilterChip(label: String, title: String, on: bool, on_toggle: EventHandler<bool>) -> Element {
    let cls = if on {
        "rounded-full border border-primary/50 bg-primary/10 px-2.5 py-0.5 text-xs font-medium text-primary transition-colors"
    } else {
        "rounded-full border border-border px-2.5 py-0.5 text-xs text-muted-foreground transition-colors hover:text-foreground"
    };
    rsx! {
        button {
            r#type: "button",
            class: "{cls}",
            title: "{title}",
            onclick: move |_| on_toggle.call(!on),
            "{label}"
        }
    }
}
