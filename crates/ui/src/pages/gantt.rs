//! `/gantt` — interactive Gantt chart.
//!
//! Wires [`view::gantt::Gantt`] against an in-memory [`GanttState`]
//! seeded with demo tasks + links. The component is uncontrolled —
//! it copies the props into its own internal signal and applies
//! drag/resize/edit mutations itself. Swapping the seed for a real
//! project/task → `GanttTask` adapter is the follow-up (issue #186).

use dioxus::prelude::*;
use view::gantt::Gantt;
use view::gantt::store::default_state;

#[component]
pub fn GanttView() -> Element {
    let seed = default_state();

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] lg:h-screen p-4 flex flex-col gap-3 overflow-hidden",
            Gantt {
                tasks: seed.tasks,
                links: seed.links,
            }
        }
    }
}
