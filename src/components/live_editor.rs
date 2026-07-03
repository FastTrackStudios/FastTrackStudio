//! Keyflow Editor — standalone full-screen studio page.
//!
//! Mounts the shared `Editor` component (from the standalone Editor repo) with
//! keyflow syntax highlighting + IDE diagnostics, beside an engraved SVG
//! preview (rendered in a real paper aspect ratio) that re-renders on a
//! debounce as you type. Wiring matches keyflow's `examples/web-editor`;
//! export (PDF/SVG) reuses `components::ExportButton` from the site's older
//! chart_editor, fed by a signal kept in sync with the live editor text.
//!
//! Web/wasm only — the contenteditable view doesn't run outside the browser.

use dioxus::prelude::*;
use editor::{Editor, EditorState, editor_view};
use editor_keyflow::{font_face_css, render_svg_live};
use editor_keyflow_lang::{
    HighlightTheme, highlight_css, keyflow_decorations, keyflow_hover, overlays_enabled,
    toggle_overlays,
};

use crate::components::ExportButton;

/// Idle delay before re-engraving the preview — see keyflow's web-editor for
/// the full rationale (avoids blocking the main thread on every keystroke).
#[cfg(target_arch = "wasm32")]
const PREVIEW_DEBOUNCE_MS: u32 = 150;

/// Layout chrome for the split editor/preview panes (grid + frame sizing).
/// Token colors come from `highlight_css`, injected at runtime.
const LIVE_EDITOR_STYLE: Asset = asset!("/assets/live-editor.css");

/// Seed chart shown on first load — Nashville number system in the key of C
/// so the resolved-chord overlays (1 → C, 5 → G, 6m → Am, …) are visible.
const SEED: &str = "FastTrackStudio Demo\n\
                    4/4 120bpm #C\n\
                    \n\
                    VS\n\
                    1 | 5 | 6m | 4\n\
                    1 | 5 | 4 1 | 1\n\
                    \n\
                    CH\n\
                    4 | 5 | 1 | 6m\n\
                    4 | 5 | 1 | 1\n";

/// Full-screen Keyflow editor studio: toolbar (export, overlay toggle) above
/// a half/half editor + paper-aspect preview split.
#[component]
pub fn LiveEditor() -> Element {
    let mut state = use_signal(|| EditorState::new(SEED.to_string()));

    let mut overlays_on = use_signal(overlays_enabled);
    let flip_overlays = move |_| {
        overlays_on.set(toggle_overlays());
        state.with_mut(|_| {}); // mark dirty → editor re-runs the decoration source
    };

    let keymap = editor::standard_markdown_keymap();
    let vim = use_signal(editor::editor_vim::VimState::new);
    let slash = use_signal(|| None::<editor_view::slash::SlashState>);

    // Plain text mirror of the editor content — `ExportButton` needs an
    // owned `Signal<String>` (it re-parses/re-lays-out the chart itself for
    // export, independent of the live preview's font-less SVG).
    let mut source_text = use_signal(|| SEED.to_string());

    // Debounced live preview — see keyflow's web-editor for the generation-
    // counter rationale (a newer edit supersedes a pending render).
    let mut preview = use_signal(String::new);
    let mut preview_gen = use_signal(|| 0u64);
    use_effect(move || {
        let src = state.read().doc.to_string();
        source_text.set(src.clone());
        let my_gen = preview_gen.peek().wrapping_add(1);
        preview_gen.set(my_gen);
        spawn(async move {
            #[cfg(target_arch = "wasm32")]
            gloo_timers::future::TimeoutFuture::new(PREVIEW_DEBOUNCE_MS).await;
            if *preview_gen.peek() != my_gen {
                return;
            }
            let svg = render_svg_live(&src)
                .unwrap_or_else(|e| format!("<pre class=\"kf-render-error\">{e}</pre>"));
            preview.set(svg);
        });
    });

    let css = use_memo(|| highlight_css(&HighlightTheme::default_dark()));
    let font_css = use_memo(|| font_face_css().unwrap_or_default());

    rsx! {
        document::Link { rel: "stylesheet", href: editor::EDITOR_STYLE }
        document::Link { rel: "stylesheet", href: LIVE_EDITOR_STYLE }
        style { dangerous_inner_html: "{css}" }
        style { dangerous_inner_html: "{font_css}" }

        div {
            class: "kf-studio",

            // Toolbar
            div {
                class: "kf-studio-toolbar",
                div {
                    class: "flex items-center gap-2",
                    span { class: "text-sm font-semibold text-foreground", "Keyflow Editor" }
                    span { class: "text-xs text-muted-foreground hidden md:inline", "Charts as code — edit on the left, engraved live on the right." }
                }
                div {
                    class: "flex items-center gap-2",
                    button {
                        class: "px-3 py-1.5 rounded-lg text-xs font-medium text-muted-foreground border border-border/50 hover:text-foreground hover:bg-accent/50 transition-colors",
                        onclick: flip_overlays,
                        if overlays_on() { "Resolved overlays: on" } else { "Resolved overlays: off" }
                    }
                    ExportButton { source: source_text }
                }
            }

            // Editor / preview split
            div {
                class: "kf-studio-split",
                section {
                    class: "kf-studio-editor-pane",
                    div {
                        class: "kf-live-editor-frame",
                        Editor {
                            state,
                            keymap: keymap.clone(),
                            decorations: keyflow_decorations as editor_view::DecorationSource,
                            hover: keyflow_hover as editor::HoverSource,
                            vim: Some(vim),
                            slash: Some(slash),
                        }
                        editor_view::slash::SlashMenu { state, slash }
                    }
                }
                section {
                    class: "kf-studio-preview-pane",
                    div {
                        class: "kf-studio-page",
                        div { class: "kf-render", dangerous_inner_html: "{preview}" }
                    }
                }
            }
        }
    }
}
