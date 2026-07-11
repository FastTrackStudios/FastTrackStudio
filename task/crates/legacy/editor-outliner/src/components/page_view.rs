//! Active page render — title + recursive block tree. Mirrors
//! `frontend.components.page`.

use dioxus::prelude::*;

use crate::components::BlockNode;
use crate::state::AppState;

#[component]
pub fn PageView() -> Element {
    let state = use_context::<AppState>();
    let vault = state.vault.read().clone();
    let active = *state.active_page.read();
    let Some(active_id) = active else {
        return rsx! {
            div { class: "editor-pane",
                p { style: "color: var(--ed-fg-muted);",
                    "Pick a page from the sidebar or create one (Cmd-K)."
                }
            }
        };
    };
    let Some(page) = vault.page_by_id(active_id).cloned() else {
        return rsx! {
            div { class: "editor-pane",
                p { style: "color: var(--ed-fg-muted);", "Page not found." }
            }
        };
    };
    let roots = vault
        .root_blocks(active_id)
        .into_iter()
        .cloned()
        .collect::<Vec<_>>();
    rsx! {
        div { class: "editor-pane",
            h1 { "{page.basename}" }
            div { class: "editor-block-tree",
                for b in roots {
                    BlockNode { key: "{b.id}", block_id: b.id }
                }
            }
        }
    }
}
