//! Dogfooding app for the editor crate. Hosts an `<Editor>`
//! plus a debug panel that mirrors the live `EditorState` so we
//! can see selection / doc / transactions while typing.

use dioxus::prelude::*;
use editor::{Editor, EditorState, Keymap, commands};

const STYLE: Asset = asset!("/assets/playground.css");

fn main() {
    init_tracing();
    tracing::info!("playground starting");
    dioxus::launch(App);
}

/// Initialize tracing for the desktop binary: stdout + a rolling
/// logfile so we can tail edits while developing. The logfile
/// goes in the repo's `target/` (gitignored). Web target skips
/// this entirely — wasm can't write files; that path will get a
/// `tracing-wasm` subscriber in a follow-up commit.
#[cfg(not(target_arch = "wasm32"))]
fn init_tracing() {
    use tracing_subscriber::{
        EnvFilter, Layer, fmt, layer::SubscriberExt, util::SubscriberInitExt,
    };
    // Logfile in the repo's target/ dir so it tags along with
    // builds and is gitignored. `daily` rolls without bound; for
    // a dev playground that's fine.
    let log_dir = std::path::Path::new("target");
    let _ = std::fs::create_dir_all(log_dir);
    let file_appender = tracing_appender::rolling::daily(log_dir, "playground.log");
    // `_guard` must outlive the process so we don't lose buffered
    // writes on shutdown. Leak it intentionally — the binary's
    // lifetime is the right scope.
    let (nb_writer, guard) = tracing_appender::non_blocking(file_appender);
    std::mem::forget(guard);

    let env_filter = || {
        EnvFilter::try_from_env("EDITOR_LOG")
            .or_else(|_| EnvFilter::try_new("info,editor=debug,editor_view=debug,playground=debug"))
            .unwrap()
    };

    let stdout_layer = fmt::layer()
        .with_target(true)
        .with_thread_ids(false)
        .with_thread_names(false)
        .with_filter(env_filter());
    let file_layer = fmt::layer()
        .with_writer(nb_writer)
        .with_ansi(false)
        .with_target(true)
        .with_thread_ids(false)
        .with_thread_names(false)
        .with_filter(env_filter());

    tracing_subscriber::registry()
        .with(stdout_layer)
        .with(file_layer)
        .init();
}

#[cfg(target_arch = "wasm32")]
fn init_tracing() {
    // No-op on wasm for now. A follow-up commit will wire
    // `tracing-wasm` so the browser DevTools console gets the
    // same stream.
}

#[component]
fn App() -> Element {
    // The whole editor state lives in this signal — the
    // `<Editor>` component reads it for rendering and writes a
    // new state on every input. We mirror it into the debug
    // panel so changes are visible as you type.
    let state = use_signal(|| {
        // Plain text seed for now. The `markdown::live_preview`
        // decoration source is correct in principle but exposes
        // a known bug in our DOM-input path: when Hidden tiles
        // remove bytes from the rendered output, textContent is
        // shorter than state.doc. The full-re-read input handler
        // then diffs the *visible* text against the full doc
        // and drops hidden bytes on every keystroke. Re-enable
        // live preview here once the input path builds a typed
        // visible-text mirror and translates offsets through the
        // tile tree (FUTURE: src/editor.rs handle_bridge_msg).
        EditorState::new(
            "Welcome to the Editor playground.\n\n\
             Type anywhere. Every keystroke flows through a \
             Transaction; the state is mirrored to the right.",
        )
    });

    // Minimal demo keymap. The browser already handles
    // Backspace/Delete/Enter for the textarea — these bindings
    // intercept and route them through commands instead, so we
    // can see them flow through the State → Transaction loop in
    // the debug panel.
    let keymap = Keymap::new()
        .with("Mod-a", commands::select_all as _)
        .with("Backspace", commands::delete_backward as _)
        .with("Delete", commands::delete_forward as _);

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
                        Editor {
                            state,
                            keymap: keymap.clone(),
                            // FUTURE: re-wire markdown::live_preview
                            // once the input bridge handles
                            // Hidden→visible-text offset
                            // translation through the tile tree.
                            // For now plain text only so the
                            // doc-DOM round-trip is 1:1.
                        }
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
            dd { id: "dbg-len", "{len}" }
            dt { "ranges" }
            dd { id: "dbg-ranges", "{ranges}" }
            dt { "primary anchor" }
            dd { id: "dbg-anchor", "{primary.anchor}" }
            dt { "primary head" }
            dd { id: "dbg-head", "{primary.head}" }
        }
        h3 { "doc.to_string()" }
        pre { id: "dbg-text", class: "debug-text", "{text}" }
    }
}
