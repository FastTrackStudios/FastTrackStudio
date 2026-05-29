//! `/tasks` — live task workspace over the org's `TaskService`.
//!
//! Thin page: loads via [`crate::task_wiring`], renders
//! [`task_ui::TasksApp`] through the shared forward converter, and
//! routes mutations back through the shared handler (optimistic +
//! write-through). The native target shows an offline notice.

use std::collections::HashMap;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use task::TaskInfo as DbTask;
use task_ui::{TaskInfo as UiTask, TaskMutation, TasksApp};
use uuid::Uuid;

use crate::orgs::{OrgMeta, OrgSelection};
use crate::task_wiring::{handle, to_ui};

#[component]
pub fn TasksView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let loader = use_resource(move || async move {
        let slugs = crate::orgs::selected_slugs(&selection.read(), &org_list.read());
        crate::feeds::fetch_tasks_tagged(&slugs).await
    });
    let mut tasks = use_signal(Vec::<DbTask>::new);
    // task id → owning org slug, so edits in "All" mode write back to
    // the right org's TaskService.
    let mut org_of = use_signal(HashMap::<Uuid, String>::new);

    use_effect(move || {
        if let Some(Ok(rows)) = &*loader.read_unchecked() {
            tasks.set(rows.iter().map(|(_, t)| t.clone()).collect());
            org_of.set(rows.iter().map(|(slug, t)| (t.id, slug.clone())).collect());
        }
    });

    let body = match &*loader.read_unchecked() {
        Some(Ok(_)) => {
            let ui_tasks: Vec<UiTask> = tasks.read().iter().map(to_ui).collect();
            rsx! {
                TasksApp {
                    tasks: ui_tasks,
                    on_event: move |mu: TaskMutation| {
                        let create_slug =
                            crate::orgs::create_target(&selection.read(), &org_list.read());
                        handle(&mut tasks, &mut org_of, &create_slug, mu);
                    },
                }
            }
        }
        Some(Err(e)) => rsx! {
            div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                "Couldn't reach the task service: {e}"
            }
        },
        None => rsx! { Text { variant: TextVariant::Muted, "Loading tasks…" } },
    };

    rsx! {
        div { class: "h-full w-full", {body} }
    }
}
