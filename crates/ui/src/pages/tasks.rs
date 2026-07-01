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

use dioxus::prelude::*;
use task_ui::{TaskInfo as UiTask, TaskMutation, TasksApp};

use crate::auth::AuthCtx;
use crate::orgs::{OrgMeta, OrgSelection};
use crate::{prefs, stores};

const PREF_ACTIVE: &str = "tasks.active";
const PREF_RELEVANT: &str = "tasks.relevant";

/// The signed-in account's email, for pref scoping ("" until auth
/// resolves — reads fall back to the defaults).
fn account_email(ctx: &AuthCtx) -> String {
    ctx.active
        .read()
        .as_ref()
        .map(|a| a.email.clone())
        .unwrap_or_default()
}

#[component]
pub fn TasksView() -> Element {
    let nav = use_navigator();
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let auth = use_context::<AuthCtx>();

    let result = stores::use_task_list();
    let muts = stores::use_task_mutations();
    let store = stores::use_task_store();
    let sessions = stores::use_session_list();

    let email = account_email(&auth);
    let mut active_only = use_signal(|| prefs::load_bool(&email, PREF_ACTIVE, true));
    let mut relevant_only = use_signal(|| prefs::load_bool(&email, PREF_RELEVANT, true));

    // The running timer session's project — relevance boosts its
    // sibling tasks to the top ("if my timer is on a project,
    // prioritize that project's items").
    let active_project = sessions.value().as_ref().and_then(|rows| {
        rows.iter()
            .find(|(_, r)| r.session.end_time.is_none())
            .and_then(|(_, r)| r.session.project_id)
    });

    let body = match (&result.value(), result.error()) {
        (Some(rows), _) => {
            let now = chrono::Local::now();
            let ctx = task::RelevanceContext {
                local_hhmm: Some(now.format("%H:%M").to_string()),
                local_date: Some(now.format("%Y-%m-%d").to_string()),
                // FUTURE(plans/relevancy-and-inbox.md): location +
                // device from an "I'm at:" switcher / user-agent.
                location: None,
                device: None,
                active_project,
            };
            let mut domain: Vec<&task::TaskInfo> = rows.iter().map(|(_, r)| &r.task).collect();
            let total = domain.len();
            if active_only() {
                domain.retain(|t| task::status_is_open(&t.status));
            }
            if relevant_only() {
                domain.retain(|t| task::is_relevant(t, &ctx));
                domain.sort_by_key(|t| task::relevance_rank(t, &ctx));
            }
            let hidden = total - domain.len();
            let ui_tasks: Vec<UiTask> = domain.iter().map(|t| stores::to_ui(t)).collect();

            let email_a = email.clone();
            let email_r = email.clone();
            rsx! {
                div { class: "flex h-full w-full flex-col",
                    // Filter chips — the page's representation of the
                    // domain filter, nothing more.
                    div { class: "mx-auto flex w-full max-w-5xl items-center gap-2 px-4 pt-3 sm:px-6 lg:px-10",
                        FilterChip {
                            label: "Active",
                            title: "Hide done/cancelled tasks",
                            on: active_only(),
                            on_toggle: move |on| {
                                active_only.set(on);
                                prefs::save_bool(&email_a, PREF_ACTIVE, on);
                            },
                        }
                        FilterChip {
                            label: "Relevant",
                            title: "Only what matters right now — routines in their time windows, deadlines always, timer project first",
                            on: relevant_only(),
                            on_toggle: move |on| {
                                relevant_only.set(on);
                                prefs::save_bool(&email_r, PREF_RELEVANT, on);
                            },
                        }
                        if hidden > 0 {
                            span { class: "text-xs text-muted-foreground", "{hidden} hidden" }
                        }
                    }
                    div { class: "min-h-0 flex-1",
                        TasksApp {
                            tasks: ui_tasks,
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

/// Small on/off filter chip.
#[component]
fn FilterChip(label: String, title: String, on: bool, on_toggle: EventHandler<bool>) -> Element {
    let cls = if on {
        "rounded-full border border-primary/50 bg-primary/10 px-3 py-0.5 text-xs font-medium text-primary transition-colors"
    } else {
        "rounded-full border border-border px-3 py-0.5 text-xs text-muted-foreground transition-colors hover:text-foreground"
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
