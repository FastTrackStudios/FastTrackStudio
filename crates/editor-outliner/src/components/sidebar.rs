//! Left sidebar. Lists pages and switches the top-level pane.
//! Mirrors a stripped-down `frontend.components.sidebar`.

use dioxus::prelude::*;

use crate::handler::open_page;
use crate::state::{AppState, Pane};

#[component]
pub fn Sidebar() -> Element {
    let state = use_context::<AppState>();
    let vault = state.vault.read().clone();
    let active = *state.active_page.read();
    let pane = *state.pane.read();
    let mut sorted = vault.pages.clone();
    sorted.sort_by(|a, b| a.basename.to_lowercase().cmp(&b.basename.to_lowercase()));
    rsx! {
        aside { class: "editor-sidebar",
            div { class: if pane == Pane::Outliner { "nav-item active" } else { "nav-item" },
                onclick: move |_| state.pane.clone().set(Pane::Outliner),
                "Outliner"
            }
            div { class: if pane == Pane::Settings { "nav-item active" } else { "nav-item" },
                onclick: move |_| state.pane.clone().set(Pane::Settings),
                "Settings"
            }
            div { class: if pane == Pane::Graph { "nav-item active" } else { "nav-item" },
                onclick: move |_| state.pane.clone().set(Pane::Graph),
                "Graph"
            }
            h2 { "Pages" }
            for p in sorted {
                {
                    let id = p.id;
                    let cls = if active == Some(id) { "page-link active" } else { "page-link" };
                    rsx! {
                        div { key: "{id}", class: "{cls}",
                            onclick: move |_| open_page(state, id),
                            "{p.basename}"
                        }
                    }
                }
            }
        }
    }
}
