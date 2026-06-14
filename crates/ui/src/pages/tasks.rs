//! `/tasks` — live task workspace over the org's `TaskService`.
//!
//! Thin page: the shared task store ([`crate::stores`]) loads the
//! selected orgs' tasks as one `AtomResult` (slug-tagged rows, so
//! "All" mode routes each edit back to its owning org), renders
//! [`task_ui::TasksApp`] through the shared forward converter, and
//! applies board mutations through the optimistic
//! [`crate::stores::TaskMutations`] (instant patch, reconcile or
//! rollback + tray notification).

use dioxus::prelude::*;
use task_ui::{TaskInfo as UiTask, TaskMutation, TasksApp};

use crate::orgs::{OrgMeta, OrgSelection};
use crate::stores;

#[component]
pub fn TasksView() -> Element {
    let nav = use_navigator();
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    let result = stores::use_task_list();
    let muts = stores::use_task_mutations();

    let body = match (&result.value(), result.error()) {
        (Some(rows), _) => {
            let ui_tasks: Vec<UiTask> = rows
                .iter()
                .map(|(_, row)| stores::to_ui(&row.task))
                .collect();
            rsx! {
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
        (None, Some(e)) => rsx! {
            crate::states::ErrorState {
                title: "Couldn't reach the task service",
                message: e,
            }
        },
        (None, None) => rsx! { crate::states::LoadingState {} },
    };

    rsx! {
        div { class: "h-full w-full", {body} }
    }
}
