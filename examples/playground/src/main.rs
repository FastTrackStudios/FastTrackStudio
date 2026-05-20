//! Dogfooding app for the editor crate. Hosts an `<Editor>`
//! plus a debug panel that mirrors the live `EditorState` so we
//! can see selection / doc / transactions while typing.

use dioxus::prelude::*;
use editor::{Editor, EditorState};

const STYLE: Asset = asset!("/assets/playground.css");

fn main() {
    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    // The whole editor state lives in this signal — the
    // `<Editor>` component reads it for rendering and writes a
    // new state on every input. We mirror it into the debug
    // panel so changes are visible as you type.
    let state = use_signal(|| {
        EditorState::new(
            "Welcome to the Editor playground.\n\nType anywhere — \
             every keystroke flows through a Transaction and you'll \
             see the resulting state on the right.",
        )
    });

    rsx! {
        document::Link { rel: "stylesheet", href: STYLE }
        div { class: "page",
            header { class: "page-header",
                h1 { "Editor" }
                p { class: "subtitle", "Dogfooding playground" }
            }
            div { class: "split",
                section { class: "editor-pane",
                    h2 { "Editor" }
                    div { class: "editor-frame",
                        Editor { state }
                    }
                }
                section { class: "debug-pane",
                    h2 { "State" }
                    DebugPanel { state }
                }
            }
        }
    }
}

/// Shows the live document text + selection so we can verify
/// that transactions are actually flowing through state.
#[component]
fn DebugPanel(state: Signal<EditorState>) -> Element {
    let s = state.read();
    let text = s.doc.to_string();
    let len = s.doc.len();
    let primary = s.selection.primary();
    let ranges = s.selection.ranges().len();

    rsx! {
        dl { class: "debug-grid",
            dt { "doc length" }
            dd { "{len} bytes" }
            dt { "ranges" }
            dd { "{ranges}" }
            dt { "primary anchor" }
            dd { "{primary.anchor}" }
            dt { "primary head" }
            dd { "{primary.head}" }
        }
        h3 { "doc.to_string()" }
        pre { class: "debug-text", "{text}" }
    }
}
