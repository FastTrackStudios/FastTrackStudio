//! Live Keyflow Editor — homepage demo.
//!
//! Mounts the shared `Editor` component (from the standalone Editor repo) with
//! keyflow syntax highlighting + IDE diagnostics, beside an engraved SVG
//! preview that re-renders on a debounce as you type. This is the same
//! wiring as keyflow's `examples/web-editor`, restyled to sit inside the
//! site's card/tailwind design language instead of its own full-page chrome.
//!
//! Web/wasm only — the contenteditable view doesn't run outside the browser.

use dioxus::prelude::*;
use editor::{Editor, EditorState, editor_view};
use editor_keyflow::{font_face_css, render_svg_live};
use editor_keyflow_lang::{
    HighlightTheme, highlight_css, keyflow_decorations, keyflow_hover, overlays_enabled,
    toggle_overlays,
};

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

/// Live split-pane keyflow editor with a real-time engraved preview.
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

    // Debounced live preview — see keyflow's web-editor for the generation-
    // counter rationale (a newer edit supersedes a pending render).
    let mut preview = use_signal(String::new);
    let mut preview_gen = use_signal(|| 0u64);
    use_effect(move || {
        let src = state.read().doc.to_string();
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
            class: "kf-live-demo rounded-xl border border-border/40 bg-gradient-to-br from-card/50 to-card/10 overflow-hidden shadow-lg shadow-black/10",

            div {
                class: "flex items-center justify-between gap-3 px-5 py-3 border-b border-border/40 bg-card/40",
                div {
                    span { class: "text-sm font-semibold text-foreground", "Try Keyflow live" }
                    p { class: "text-xs text-muted-foreground mt-0.5", "Edit the chart on the left — colors, resolved-chord overlays, and hover info. The engraved preview updates as you type." }
                }
                button {
                    class: "shrink-0 px-3 py-1.5 rounded-lg text-xs font-medium text-muted-foreground border border-border/50 hover:text-foreground hover:bg-accent/50 transition-colors",
                    onclick: flip_overlays,
                    if overlays_on() { "Resolved overlays: on" } else { "Resolved overlays: off" }
                }
            }

            div {
                class: "kf-live-split",
                section {
                    class: "kf-live-editor-pane",
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
                    class: "kf-live-preview-pane",
                    div { class: "kf-render", dangerous_inner_html: "{preview}" }
                }
            }
        }
    }
}
