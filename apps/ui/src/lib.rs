//! Shared Dioxus UI — used by both `apps/web` and `apps/desktop`.
//!
//! The pattern this crate is showcasing:
//!
//! 1. Import the wasm-clean `example_proto::*` types + the
//!    auto-generated `ExampleServiceClient`.
//! 2. On mount, open a vox WebSocket connection to the server and
//!    establish the `ExampleServiceClient`.
//! 3. Render data using the same `Example` struct that the server
//!    persists — no parallel DTOs, no manual JSON.

use dioxus::prelude::*;

#[component]
pub fn App() -> Element {
    rsx! {
        div { class: "container",
            h1 { "architect" }
            p {
                "Wire example: this component would call "
                code { "ExampleServiceClient::list_examples()" }
                " over vox-websocket and render the result. "
                "Service wiring lands next — this scaffold establishes the crate graph."
            }
        }
    }
}
