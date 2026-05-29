//! `/gantt` — interactive Gantt chart over the org's live tasks.
//!
//! Loads the selected org(s)' tasks via [`crate::feeds`], adapts them
//! to Gantt bars + dependency links through [`crate::gantt_adapt`], and
//! feeds the result into the uncontrolled [`view::gantt::Gantt`]
//! component (it copies the props into its own signal and self-manages
//! drag/resize/edit). Only tasks with a `scheduled`/`due` date land on
//! the chart; when none do, we fall back to the crate's demo seed so
//! the page demonstrates the chart instead of rendering blank.
//!
//! Write-back of bar edits to the `TaskService` is a follow-up — the
//! component is storage-agnostic, so mutations currently stay local.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use view::gantt::store::default_state;
use view::gantt::{Gantt, GanttLink, GanttTask};

use crate::orgs::{OrgMeta, OrgSelection};

#[component]
pub fn GanttView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();
    let loader = use_resource(move || async move {
        let slugs = crate::orgs::selected_slugs(&selection.read(), &org_list.read());
        crate::feeds::fetch_tasks_tagged(&slugs).await
    });

    let body = match &*loader.read_unchecked() {
        Some(Ok(rows)) => {
            let tasks: Vec<_> = rows.iter().map(|(_, t)| t.clone()).collect();
            let (bars, links) = crate::gantt_adapt::to_gantt(&tasks);
            chart(bars, links)
        }
        Some(Err(e)) => rsx! {
            div { class: "rounded-md border border-destructive/40 bg-destructive/10 px-3 py-2 text-sm",
                "Couldn't reach the task service: {e}"
            }
        },
        None => rsx! { Text { variant: TextVariant::Muted, "Loading tasks…" } },
    };

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] lg:h-screen p-4 flex flex-col gap-3 overflow-hidden",
            {body}
        }
    }
}

/// Render the chart, falling back to demo seed data when no live task
/// is schedulable (so the page never renders an empty timeline).
fn chart(bars: Vec<GanttTask>, links: Vec<GanttLink>) -> Element {
    if bars.is_empty() {
        let seed = default_state();
        return rsx! {
            div { class: "flex flex-col gap-2 h-full",
                Text {
                    variant: TextVariant::Muted,
                    "No tasks have a scheduled or due date yet — showing demo data."
                }
                Gantt { tasks: seed.tasks, links: seed.links }
            }
        };
    }
    rsx! { Gantt { tasks: bars, links } }
}
