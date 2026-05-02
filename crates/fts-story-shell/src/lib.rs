//! Interactive Lookbook-style shell.
//!
//! Renderer-agnostic Dioxus component. Drop into any Dioxus 0.7 app
//! (web/desktop/native/mobile) and it renders:
//!
//! - Sidebar: every story registered via [`STORIES`](fts_story_core::STORIES),
//!   grouped by `category`.
//! - Preview pane: the selected story rendered with the current knob values.
//! - Knob editor: typed inputs based on each `KnobSpec`.
//! - State matrix toggle: cycle through `auto_states` entries.
//!
//! Stub for now. The MVP build does the sidebar + preview pane against
//! hand-rolled `Story` values; knob editor + state matrix follow.

use dioxus::prelude::*;

#[component]
pub fn Lookbook() -> Element {
    rsx! {
        div { class: "fts-story-shell",
            p { "fts-story: shell stub. Wire stories via fts_story_core::STORIES." }
        }
    }
}
