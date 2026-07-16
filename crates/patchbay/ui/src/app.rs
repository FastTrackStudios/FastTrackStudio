//! Root component: toolbar + canvas + side panel + status bar.
//!
//! Takes no props — the shell provides a [`crate::PatchbayHandle`] via
//! context and feeds [`crate::apply_graph_event`] from the subscribe
//! stream, so the same component mounts in the desktop app and any
//! future browser remote.

use dioxus::prelude::*;

use crate::canvas::GraphCanvas;
use crate::panels::{SidePanel, StatusBar, Toolbar};

static CSS: &str = include_str!("style.css");

#[component]
pub fn PatchbayApp() -> Element {
    rsx! {
        document::Style { {CSS} }
        div { class: "patchbay-root",
            div { class: "topbar",
                span { class: "app-title", "FTS Patchbay" }
                Toolbar {}
            }
            div { class: "main-split",
                GraphCanvas {}
                SidePanel {}
            }
            StatusBar {}
        }
    }
}
