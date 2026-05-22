//! Testing app for the editor crate. Hosts an `<Editor>`
//! plus a debug panel that mirrors the live `EditorState` so we
//! can see selection / doc / transactions while typing.

use dioxus::prelude::*;
use editor::{
    DecoratedRange, Editor, EditorState, Keymap, bracket_match, commands, editor_view, markdown,
};

/// Combined decoration source — markdown live-preview plus
/// bracket-pair highlighting. The view's `DecorationSource` is a
/// plain `fn(&EditorState) -> Vec<DecoratedRange>` so composition
/// is just concatenation; the inner builders dedupe nothing, but
/// our overlapping mark spans on brackets sit next to each other
/// without conflict.
///
/// When the playground is running on desktop AND a vault has
/// been loaded into `VAULT`, we route the markdown pass through
/// `live_preview_with` so cross-doc `((uuid))`, `[[Page]]`, and
/// `![[Page#Heading]]` resolve against the on-disk vault.
fn combined_decorations(state: &EditorState) -> Vec<DecoratedRange> {
    #[cfg(not(target_arch = "wasm32"))]
    let mut out = {
        let guard = VAULT.read().expect("vault rwlock poisoned");
        match guard.as_ref() {
            Some((v, idx)) => {
                let view = vault::VaultLookupView::new(v, idx);
                markdown::live_preview_with(state, Some(&view))
            }
            None => markdown::live_preview(state),
        }
    };
    #[cfg(target_arch = "wasm32")]
    let mut out = markdown::live_preview(state);
    out.extend(bracket_match::bracket_match(state));
    out
}

/// Native vault snapshot. The watcher thread swaps fresh
/// `(Vault, BlockIndex)` pairs in on debounced FS events;
/// reads from `combined_decorations` take the read lock for
/// the duration of one render pass. `None` while the vault
/// is still loading (or absent).
#[cfg(not(target_arch = "wasm32"))]
static VAULT: std::sync::RwLock<Option<(vault::Vault, vault::BlockIndex)>> =
    std::sync::RwLock::new(None);

#[cfg(not(target_arch = "wasm32"))]
fn init_vault() {
    let Some(home) = std::env::var_os("HOME").map(std::path::PathBuf::from) else {
        return;
    };
    // Prefer the Observatory if it exists (8k-page real
    // vault, useful for actually using the playground);
    // fall back to the demo vault at `~/Documents/Task/`
    // (created by `make_example_vault` when the user runs
    // the playground for the first time).
    let candidates = [
        home.join("Documents/Task"),
        home.join("Documents/The Observatory"),
    ];
    let root = candidates.into_iter().find(|p| p.exists());
    let Some(root) = root else {
        tracing::info!("no vault under ~/Documents — skipping");
        return;
    };
    if let Err(e) = reload_vault(&root) {
        tracing::warn!(?e, "vault open failed — running without cross-doc refs");
        return;
    }
    // Spawn a background watcher so changes on disk reload
    // the in-memory snapshot. Debounce keeps this cheap on a
    // bulk save / `rsync`.
    spawn_watcher(root);
}

#[cfg(not(target_arch = "wasm32"))]
fn reload_vault(root: &std::path::Path) -> Result<(), vault::LoadError> {
    let t0 = std::time::Instant::now();
    let v = vault::Vault::open(root)?;
    let idx = vault::BlockIndex::build(&v);
    tracing::info!(
        pages = v.pages.len(),
        bases = v.bases.len(),
        ids = idx.len(),
        property_hints = v.property_types.map.len(),
        elapsed = ?t0.elapsed(),
        root = %root.display(),
        "vault loaded"
    );
    let mut guard = VAULT.write().expect("vault rwlock poisoned");
    *guard = Some((v, idx));
    Ok(())
}

#[cfg(not(target_arch = "wasm32"))]
fn spawn_watcher(root: std::path::PathBuf) {
    use std::time::Duration;
    std::thread::spawn(move || {
        let (rx, _guard) = match vault::watch(root.clone(), Duration::from_millis(250)) {
            Ok(p) => p,
            Err(e) => {
                tracing::warn!(?e, "watcher failed to start");
                return;
            }
        };
        tracing::info!(root = %root.display(), "vault watcher running");
        // Process events on a coarse "burst" — wait for the
        // first one, then drain any others that arrived
        // within 100ms before doing the actual reload. A
        // single save in another editor often triggers 2-3
        // mutation events; collapsing them keeps the rebuild
        // cost down.
        while let Ok(_evt) = rx.recv() {
            while let Ok(_more) = rx.recv_timeout(Duration::from_millis(100)) {}
            if let Err(e) = reload_vault(&root) {
                tracing::warn!(?e, "watcher reload failed");
            }
        }
    });
}

const STYLE: Asset = asset!("/assets/playground.css");

fn main() {
    init_tracing();
    #[cfg(not(target_arch = "wasm32"))]
    init_vault();
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
        // Desktop with a vault loaded: open `Welcome.md` so the
        // cross-doc resolution paths are visible immediately.
        // No vault / wasm / explicit `?seed=` override: use the
        // built-in showcase seed below.
        #[cfg(not(target_arch = "wasm32"))]
        let seed_from_vault = read_seed_query()
            .is_none()
            .then(|| {
                let guard = VAULT.read().ok()?;
                let (v, _) = guard.as_ref()?;
                v.page_by_basename("Welcome").map(|p| p.raw.clone())
            })
            .flatten();
        #[cfg(not(target_arch = "wasm32"))]
        if let Some(text) = seed_from_vault {
            return EditorState::new(text);
        }
        let seed = read_seed_query().unwrap_or_else(|| {
            String::from(
                "---\n\
                 title: Editor playground\n\
                 tags: [editor, demo, live-preview]\n\
                 published: true\n\
                 draft: false\n\
                 author: cody\n\
                 created: 2026-05-20\n\
                 priority: 3\n\
                 description: |\n\
                   Multi-line YAML scalar — used to demo block-\n\
                   scalar parsing and editing in the Properties\n\
                   panel above.\n\
                 aliases:\n\
                   - playground\n\
                   - demo doc\n\
                 ---\n\
                 # Welcome to the Editor playground\n\
                 \n\
                 Setext-style heading too\n\
                 ========================\n\
                 \n\
                 Subtitle via dashes\n\
                 -------------------\n\
                 \n\
                 ## Inline styles\n\
                 \n\
                 **bold**, *italic*, ***bold italic***, ~~strikethrough~~, \
                 ==highlight==, `inline code`, and an inline footnote^[click the marker to edit me].\n\
                 \n\
                 Links: standard [Anthropic](https://anthropic.com), \
                 an autolink <https://obsidian.md>, \
                 wikilinks: [[Editor Roadmap]], [[Project README|the readme]], \
                 and to a header [[Editor Roadmap#Goals]]. Unresolved targets render red \
                 (no vault yet, so every wikilink is unresolved). \
                 Tags like #editor #live-preview #notes/howto, \
                 and a footnote ref [^1]. \
                 Block id at end of paragraph ^demo-block-id\n\
                 \n\
                 ## Block styles\n\
                 \n\
                 > Blockquotes are just blockquotes.\n\
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
                 - [/] In progress (custom Tasks-plugin status)\n\
                 - [>] Forwarded\n\
                 - [-] Cancelled\n\
                 \n\
                 ### Callouts (all 13 types)\n\
                 \n\
                 > [!note] Note\n\
                 > Callouts share the blockquote syntax.\n\
                 \n\
                 > [!tip] Tip\n\
                 > Press `/` anywhere to open the slash-command menu.\n\
                 \n\
                 > [!warning]+ Collapsible warning\n\
                 > The `+`/`-` on the type marker controls folded default.\n\
                 \n\
                 > [!danger] Danger\n\
                 > High-stakes call-out style.\n\
                 \n\
                 > [!info] Info\n\
                 > Use the slash menu `/callout` to insert any of the others — \
                 abstract, info, success, question, failure, bug, example, quote.\n\
                 \n\
                 > [!example] Nested callouts\n\
                 > The outer is an example callout.\n\
                 > > [!warning] Two levels deep\n\
                 > > The body inherits the inner kind.\n\
                 > > > [!danger] Three levels\n\
                 > > > Rare in practice but supported.\n\
                 > Back to the outer level.\n\
                 \n\
                 ### Table\n\
                 \n\
                 | Feature             | Status        | Notes                          |\n\
                 |---------------------|---------------|--------------------------------|\n\
                 | Headings (1-6)      | ✅ Mod-1..6   | Mod-0 strips                   |\n\
                 | Tables              | ✅            | GFM pipe form                  |\n\
                 | Math (inline+block) | ✅ Typst      | Compiled per-pass via cache    |\n\
                 | Mermaid             | ✅ pure Rust  | mermaid-rs-renderer            |\n\
                 | Frontmatter         | ✅ editable   | bool/number/date/list/text     |\n\
                 | Vim                 | ✅ default-on | C/D/Y / gg / gu/gU/g~ / */#/n  |\n\
                 | Slash menu          | ✅ `/` opens  | `/callout`, `/typst`, …        |\n\
                 \n\
                 ### Math\n\
                 \n\
                 Inline math compiles via Typst: $E = m c^2$, and a longer one — \
                 $sum_(i=1)^n i = n(n+1)/2$.\n\
                 \n\
                 $$ integral_0^1 x^2 d x = 1/3 $$\n\
                 \n\
                 ### Typst block\n\
                 \n\
                 ```typst\n\
                 = Typst block heading\n\
                 \n\
                 Full Typst documents render in-place.\n\
                 \n\
                 $ A = mat(1, 2; 3, 4) $\n\
                 ```\n\
                 \n\
                 ### Mermaid diagram\n\
                 \n\
                 ```mermaid\n\
                 flowchart LR\n\
                   A[Keystroke] --> B{Live preview}\n\
                   B -->|markdown| C[Decorations]\n\
                   B -->|math| D[Typst SVG]\n\
                   B -->|diagram| E[Mermaid SVG]\n\
                   C --> F[DOM patch]\n\
                   D --> F\n\
                   E --> F\n\
                 ```\n\
                 \n\
                 ### Editor commands\n\
                 \n\
                 - **Mod-B** / **Mod-I** — bold / italic\n\
                 - **Mod-K** — wrap as `[…](url)`\n\
                 - **Mod-L** — cycle list marker (none → `-` → `1.` → `- [ ]`)\n\
                 - **Mod-T** — toggle task on current line\n\
                 - **Mod-1**..**Mod-6** — heading levels; **Mod-0** strips\n\
                 - **Mod-E** — toggle reading mode\n\
                 - **`/`** — open the slash-command palette\n\
                 \n\
                 ### Embeds\n\
                 \n\
                 Wikilink embed: `![[diagram.png|320]]` (renders an `<img>` when the file resolves).\n\
                 \n\
                 ### Block IDs (Logseq-style references)\n\
                 \n\
                 This is a referenceable block — press Mod-Shift-K on any block to give it an id.\n\
                 id:: 5f9c1234-abcd-4ef0-8123-fedcba012345\n\
                 \n\
                 You can reference it inline like this: ((5f9c1234-abcd-4ef0-8123-fedcba012345)).\n\
                 \n\
                 Or embed the whole block as a card:\n\
                 \n\
                 {{embed ((5f9c1234-abcd-4ef0-8123-fedcba012345))}}\n\
                 \n\
                 ### Page + section embeds (Obsidian-style)\n\
                 \n\
                 Embed a whole page (placeholder until multi-file lookup):\n\
                 \n\
                 ![[Project README]]\n\
                 \n\
                 Embed a section by heading — resolves intra-doc if the heading lives in this file:\n\
                 \n\
                 ![[#Math]]\n\
                 \n\
                 Embed a block by Obsidian short-id (the `^demo-block-id` anchor near the top):\n\
                 \n\
                 ![[#^demo-block-id]]\n\
                 \n\
                 ### Code fences (syntax highlighting)\n\
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
                 ```ts\n\
                 const greet = (name: string) => `Hello, ${name}!`;\n\
                 ```\n\
                 \n\
                 Comments like %% this %% hide on focus-away.\n\
                 \n\
                 ---\n\
                 \n\
                 [^1]: Footnote definitions live at the bottom of the file.\n\
                 \n\
                 Markers stay visible while your caret is on the span — move away and they fade out.",
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
        .with("Mod-k", commands::toggle_link as _)
        .with("Mod-Shift-k", |s: &_| {
            commands::add_block_id(s).map(|(t, _)| t)
        })
        .with("Mod-l", commands::cycle_list as _)
        .with("Mod-t", commands::toggle_task as _)
        .with("Mod-1", |s: &_| commands::set_heading(s, 1))
        .with("Mod-2", |s: &_| commands::set_heading(s, 2))
        .with("Mod-3", |s: &_| commands::set_heading(s, 3))
        .with("Mod-4", |s: &_| commands::set_heading(s, 4))
        .with("Mod-5", |s: &_| commands::set_heading(s, 5))
        .with("Mod-6", |s: &_| commands::set_heading(s, 6))
        .with("Mod-0", |s: &_| commands::set_heading(s, 0))
        .with("Mod-e", commands::toggle_reading_mode as _)
        .with("Tab", commands::indent_more as _)
        .with("Shift-Tab", commands::indent_less as _)
        .with("Backspace", commands::delete_backward as _)
        .with("Delete", commands::delete_forward as _);

    // Vim modal state. Default-on per user preference — toggle
    // with `?novim=1` in the URL to fall back to plain editing.
    let vim = use_signal(editor::editor_vim::VimState::new);
    let vim_enabled = !read_query_flag("novim");

    // Slash-command palette. The Editor refreshes this on every
    // doc change via `detect_slash`; the `SlashMenu` component
    // renders the popup directly inside the editor frame.
    let slash = use_signal(|| None::<editor::editor_view::slash::SlashState>);

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
                                slash: Some(slash),
                            }
                        } else {
                            Editor {
                                state,
                                keymap: keymap.clone(),
                                decorations: combined_decorations
                                    as editor_view::DecorationSource,
                                vim: if vim_enabled { Some(vim) } else { None },
                                slash: Some(slash),
                            }
                        }
                        editor::editor_view::slash::SlashMenu { state, slash }
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
        v.pending_register
            .map(|r| format!("\"{r:?}"))
            .unwrap_or_default(),
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
