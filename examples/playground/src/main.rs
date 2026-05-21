//! Testing app for the editor crate. Hosts an `<Editor>`
//! plus a debug panel that mirrors the live `EditorState` so we
//! can see selection / doc / transactions while typing.

use dioxus::prelude::*;
use editor::{
    bracket_match, commands, editor_view, markdown, DecoratedRange, Editor, EditorState, Keymap,
};

/// Combined decoration source — markdown live-preview plus
/// bracket-pair highlighting. The view's `DecorationSource` is a
/// plain `fn(&EditorState) -> Vec<DecoratedRange>` so composition
/// is just concatenation; the inner builders dedupe nothing, but
/// our overlapping mark spans on brackets sit next to each other
/// without conflict.
fn combined_decorations(state: &EditorState) -> Vec<DecoratedRange> {
    let mut out = markdown::live_preview(state);
    out.extend(bracket_match::bracket_match(state));
    out
}

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
        fmt, layer::SubscriberExt, util::SubscriberInitExt, EnvFilter, Layer,
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
    // Browser DevTools console gets the structured log stream.
    // Filter via `?log=trace` query for verbose; otherwise default
    // to `debug` for the editor crates and `info` everywhere else.
    let level = if read_query_flag("log") {
        tracing::Level::TRACE
    } else {
        tracing::Level::DEBUG
    };
    let cfg = tracing_wasm::WASMLayerConfigBuilder::new()
        .set_max_level(level)
        .build();
    tracing_wasm::set_as_global_default_with_config(cfg);
}

/// Look for a `?seed=...` query param in the page URL and
/// percent-decode it. Returns `Some` only on the web target;
/// on desktop there's no URL so we always use the default
/// seed.
///
/// Hand-rolled parser instead of `web_sys::UrlSearchParams` —
/// the latter pulls in a transitive `getrandom 0.3` dep that
/// needs the `wasm_js` feature flag we're not configuring.
#[cfg(target_arch = "wasm32")]
fn read_seed_query() -> Option<String> {
    let window = web_sys::window()?;
    let search = window.location().search().ok()?;
    // search starts with `?` — strip it and split on `&`.
    let trimmed = search.strip_prefix('?').unwrap_or(&search);
    for pair in trimmed.split('&') {
        if let Some(rest) = pair.strip_prefix("seed=") {
            return Some(percent_decode(rest));
        }
    }
    None
}

/// Look for `?flag` or `?flag=1` etc. — returns true when the
/// query string contains the named flag with a truthy value.
fn read_query_flag(_name: &str) -> bool {
    #[cfg(target_arch = "wasm32")]
    {
        let window = match web_sys::window() {
            Some(w) => w,
            None => return false,
        };
        let search = window.location().search().unwrap_or_default();
        let trimmed = search.strip_prefix('?').unwrap_or(&search);
        for pair in trimmed.split('&') {
            let (k, v) = match pair.split_once('=') {
                Some((k, v)) => (k, v),
                None => (pair, "1"),
            };
            if k == _name {
                return matches!(v, "1" | "true" | "yes" | "on" | "");
            }
        }
        false
    }
    #[cfg(not(target_arch = "wasm32"))]
    {
        false
    }
}

#[cfg(target_arch = "wasm32")]
fn percent_decode(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    let mut bytes = s.bytes();
    while let Some(b) = bytes.next() {
        if b == b'%' {
            let h = bytes.next().and_then(|x| (x as char).to_digit(16));
            let l = bytes.next().and_then(|x| (x as char).to_digit(16));
            if let (Some(h), Some(l)) = (h, l) {
                out.push(((h * 16 + l) as u8) as char);
            }
        } else if b == b'+' {
            out.push(' ');
        } else {
            out.push(b as char);
        }
    }
    out
}

#[cfg(not(target_arch = "wasm32"))]
fn read_seed_query() -> Option<String> {
    None
}

#[component]
fn App() -> Element {
    // The whole editor state lives in this signal — the
    // `<Editor>` component reads it for rendering and writes a
    // new state on every input. We mirror it into the debug
    // panel so changes are visible as you type.
    // Initial seed text. Tests can override via `?seed=` query
    // (URL-decoded) so they don't have to start from the
    // welcome message — useful for isolating decoration-aware
    // typing tests from the markdown in the default seed.
    let state = use_signal(|| {
        let seed = read_seed_query().unwrap_or_else(|| {
            String::from(
                "# Welcome to the Editor playground\n\
                 \n\
                 ## Inline styles\n\
                 \n\
                 **bold**, *italic*, ***bold italic***, ~~strikethrough~~, \
                 ==highlight==, and `inline code`.\n\
                 \n\
                 Links: [Anthropic](https://anthropic.com), \
                 wikilinks like [[Page Name]], \
                 tags like #editor #live-preview, \
                 and footnote refs like this[^1].\n\
                 \n\
                 ## Block styles\n\
                 \n\
                 > Blockquotes look like this.\n\
                 > Multi-line works too.\n\
                 \n\
                 - Unordered list item\n\
                 - Another item\n\
                 \n\
                 1. Ordered list\n\
                 2. Stays numbered\n\
                 \n\
                 - [ ] Click the checkbox to toggle\n\
                 - [x] Done\n\
                 \n\
                 > [!note] Callouts\n\
                 > Callouts share the blockquote syntax — just\n\
                 > prepend `[!type]` (note, tip, warning, danger,\n\
                 > quote, success, info, todo, question, …).\n\
                 \n\
                 > [!warning]+ Collapsible\n\
                 > The `+`/`-` on the type marker is parsed but\n\
                 > not yet wired to toggling.\n\
                 \n\
                 | Feature | Status |\n\
                 |---------|--------|\n\
                 | Headings | ✅ |\n\
                 | Tables  | ✅ (this one!) |\n\
                 | Math    | not yet |\n\
                 \n\
                 Comments like %% this %% hide on focus-away.\n\
                 \n\
                 ---\n\
                 \n\
                 ## Code fences with syntax highlighting\n\
                 \n\
                 ```rust\n\
                 fn greet(name: &str) -> String {\n\
                     format!(\"Hello, {name}!\")\n\
                 }\n\
                 ```\n\
                 \n\
                 ```python\n\
                 def greet(name):\n\
                     return f\"Hello, {name}!\"\n\
                 ```\n\
                 \n\
                 Markers stay visible while your caret is on the span — \
                 move away and they fade out.",
            )
        });
        EditorState::new(seed)
    });

    // Minimal demo keymap. The browser already handles
    // Backspace/Delete/Enter for the textarea — these bindings
    // intercept and route them through commands instead, so we
    // can see them flow through the State → Transaction loop in
    // the debug panel.
    // Enter is handled by the view's beforeinput bridge (which
    // routes it through `enter_continue_list` Rust-side) rather
    // than the keymap, so the browser's default
    // `insertParagraph` never sneaks a stray `\n` in alongside
    // our authored change.
    let keymap = Keymap::new()
        .with("Mod-a", commands::select_all as _)
        .with("Mod-b", commands::toggle_bold as _)
        .with("Mod-i", commands::toggle_italic as _)
        .with("Mod-e", commands::toggle_reading_mode as _)
        .with("Tab", commands::indent_more as _)
        .with("Shift-Tab", commands::indent_less as _)
        .with("Backspace", commands::delete_backward as _)
        .with("Delete", commands::delete_forward as _);

    // Vim modal state. Default-on per user preference — toggle
    // with `?novim=1` in the URL to fall back to plain editing.
    let vim = use_signal(editor::editor_vim::VimState::new);
    let vim_enabled = !read_query_flag("novim");

    rsx! {
        document::Link { rel: "stylesheet", href: STYLE }
        div { class: "page",
            header { class: "page-header",
                h1 { "Editor" }
                p { class: "subtitle", "Text playground" }
            }
            div { class: "split",
                section { class: "editor-pane",
                    h2 { "Editor" }
                    div { class: "editor-frame",
                        if read_query_flag("nodeco") {
                            Editor {
                                state,
                                keymap: keymap.clone(),
                                vim: if vim_enabled { Some(vim) } else { None },
                            }
                        } else {
                            Editor {
                                state,
                                keymap: keymap.clone(),
                                decorations: combined_decorations
                                    as editor_view::DecorationSource,
                                vim: if vim_enabled { Some(vim) } else { None },
                            }
                        }
                    }
                }
                section { class: "debug-pane",
                    h2 { "State" }
                    if vim_enabled {
                        VimStatus { vim }
                    }
                    DebugPanel { state }
                }
            }
        }
    }
}

/// Vim mode badge + pending-command preview. Mirrors the
/// vim-status strip an Obsidian / Neovim user would see in the
/// status bar.
#[component]
fn VimStatus(vim: Signal<editor::editor_vim::VimState>) -> Element {
    let v = vim.read();
    let (mode_label, mode_class) = match v.mode {
        editor::editor_vim::Mode::Normal => ("NORMAL", "mode-normal"),
        editor::editor_vim::Mode::Insert => ("INSERT", "mode-insert"),
        editor::editor_vim::Mode::VisualChar => ("VISUAL", "mode-visual"),
        editor::editor_vim::Mode::VisualLine => ("V-LINE", "mode-visual"),
        editor::editor_vim::Mode::VisualBlock => ("V-BLOCK", "mode-visual"),
        editor::editor_vim::Mode::Replace => ("REPLACE", "mode-replace"),
        editor::editor_vim::Mode::Command => ("COMMAND", "mode-command"),
    };
    let pending = format!(
        "{}{}{}",
        v.pending_count.map(|n| n.to_string()).unwrap_or_default(),
        v.pending_register.map(|r| format!("\"{r:?}")).unwrap_or_default(),
        v.pending_operator
            .map(|op| format!("{op:?}").chars().next().unwrap().to_string())
            .unwrap_or_default(),
    );
    rsx! {
        div { class: "vim-status",
            span { class: "vim-mode {mode_class}", "{mode_label}" }
            span { class: "vim-pending", "{pending}" }
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
