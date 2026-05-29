//! `/tasks` — live task workspace over the org's `TaskService`.
//!
//! Thin page: loads via [`crate::task_wiring`], renders
//! [`task_ui::TasksApp`] through the shared forward converter, and
//! routes mutations back through the shared handler (optimistic +
//! write-through). The native target shows an offline notice.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use task::TaskInfo as DbTask;
use task_ui::{TaskInfo as UiTask, TaskMutation, TasksApp};

use crate::task_wiring::{fetch_tasks, handle, to_ui};

#[component]
pub fn TasksView() -> Element {
    let loader = use_resource(|| async move { fetch_tasks().await });
    let mut tasks = use_signal(Vec::<DbTask>::new);

    use_effect(move || {
        if let Some(Ok(rows)) = &*loader.read_unchecked() {
            tasks.set(rows.clone());
        }
    });

    let body = match &*loader.read_unchecked() {
        Some(Ok(_)) => {
            let ui_tasks: Vec<UiTask> = tasks.read().iter().map(to_ui).collect();
            rsx! {
                TasksApp {
                    tasks: ui_tasks,
                    on_event: move |mu: TaskMutation| handle(&mut tasks, mu),
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
