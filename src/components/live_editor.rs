//! Keyflow Editor — standalone full-screen studio page.
//!
//! Mounts the shared `Editor` component (from the standalone Editor repo) with
//! keyflow syntax highlighting + IDE diagnostics, beside an engraved preview
//! rendered at true A4 page size (not stretched to fit the pane) that
//! re-renders on a debounce as you type. The preview viewport supports
//! drag-to-pan + wheel-to-zoom, the same interaction pattern keyflow-ui's
//! native chart panels use (crates/keyflow-ui/src/panels/preview_panel.rs,
//! layouts/chart_editor.rs). Export (PDF/SVG) reuses `components::ExportButton`
//! from the site's older chart_editor, fed by a signal kept in sync with the
//! live editor text.
//!
//! Web/wasm only — the contenteditable view doesn't run outside the browser.

use dioxus::prelude::*;
use editor::{Editor, EditorState, editor_view};
use editor_keyflow::font_face_css;
use editor_keyflow_lang::{
    HighlightTheme, highlight_css, keyflow_decorations, keyflow_hover, overlays_enabled,
    toggle_overlays,
};

use crate::components::ExportButton;
use crate::renderer::{ChartLayoutManager, DPI_SCALE};

/// Idle delay before re-engraving the preview — see keyflow's web-editor for
/// the full rationale (avoids blocking the main thread on every keystroke).
#[cfg(target_arch = "wasm32")]
const PREVIEW_DEBOUNCE_MS: u32 = 150;

/// Gap between stacked pages in the preview, in CSS px.
const PAGE_GAP_PX: f64 = 24.0;

/// Zoom bounds, matching keyflow-ui's native chart viewports.
const ZOOM_MIN: f64 = 0.1;
const ZOOM_MAX: f64 = 8.0;

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

/// One engraved page: real point dimensions (converted to CSS px for
/// display) plus its SVG markup.
#[derive(Clone, PartialEq)]
struct RenderedPage {
    width_px: f64,
    height_px: f64,
    svg: String,
}

/// A blank A4 sheet (no SVG content — `.kf-studio-page`'s white background
/// alone reads as an empty page). Used both as the initial state (before the
/// first debounced render lands) and whenever the source doesn't yet produce
/// any laid-out page (still writing the title/tempo/key, no section started
/// yet, or a parse error) — the page should never simply disappear.
fn blank_page() -> RenderedPage {
    RenderedPage {
        width_px: 595.0 * DPI_SCALE,
        height_px: 842.0 * DPI_SCALE,
        svg: String::new(),
    }
}

/// Full-screen Keyflow editor studio: toolbar (export, overlay toggle) above
/// a half/half editor + true-size, pannable/zoomable A4 preview split.
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
    // export, independent of the live preview below).
    let mut source_text = use_signal(|| SEED.to_string());

    // Debounced live preview, laid out in real A4 page mode (not the
    // continuous-scroll/snippet mode `editor_keyflow::render_svg_live` uses)
    // — see keyflow's web-editor for the generation-counter rationale (a
    // newer edit supersedes a pending render). Font-less
    // (`export_pages_to_svg_live`), same performance reasoning as
    // `render_svg_live`: fonts are injected once below, so each pass
    // serializes glyph-less paths, not several MB of embedded font data.
    let mut pages = use_signal(|| vec![blank_page()]);
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

            let rendered = (|| -> Result<Vec<RenderedPage>, String> {
                // No `sections.is_empty()` filter — a metadata-only chart
                // (title/tempo/key, no section yet) still lays out and
                // engraves its header, it just has no staff systems yet.
                let Some(chart) = keyflow::parse(&src).ok() else {
                    return Ok(vec![blank_page()]);
                };
                let mut manager = ChartLayoutManager::new()?;
                // `snippet_mode: false` -> full A4 paginated layout; the
                // viewport-width arg is ignored in that mode (Page layout is
                // always paginated_a4 regardless), so any value works here.
                manager.layout_chart_for_export(&chart, 800.0, false);
                let pages: Vec<RenderedPage> = manager
                    .export_pages_to_svg_live()?
                    .into_iter()
                    .map(|(w_pt, h_pt, svg)| RenderedPage {
                        width_px: w_pt * DPI_SCALE,
                        height_px: h_pt * DPI_SCALE,
                        svg,
                    })
                    .collect();
                Ok(if pages.is_empty() {
                    vec![blank_page()]
                } else {
                    pages
                })
            })();
            let rendered = rendered.unwrap_or_else(|e| {
                tracing::warn!("keyflow live preview render failed: {e}");
                vec![blank_page()]
            });
            pages.set(rendered);
        });
    });

    let css = use_memo(|| highlight_css(&HighlightTheme::default_dark()));
    let font_css = use_memo(|| font_face_css().unwrap_or_default());

    // --- Preview viewport: drag-to-pan (any button) + wheel-to-zoom, same
    // interaction as keyflow-ui's native chart panels. ---
    let mut zoom = use_signal(|| 0.75_f64);
    let mut pan_x = use_signal(|| 24.0_f64);
    let mut pan_y = use_signal(|| 24.0_f64);
    let mut dragging = use_signal(|| false);
    let mut last_mouse = use_signal(|| (0.0_f64, 0.0_f64));

    let transform = use_memo(move || {
        format!(
            "transform: translate({}px, {}px) scale({}); transform-origin: 0 0;",
            pan_x(),
            pan_y(),
            zoom()
        )
    });

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

                    // Wheel → zoom, anchored roughly at the cursor.
                    onwheel: move |evt| {
                        evt.prevent_default();
                        let delta_y = evt.delta().strip_units().y;
                        let old_zoom = zoom();
                        let zoom_factor = if delta_y < 0.0 { 1.05 } else { 0.95 };
                        let new_zoom = (old_zoom * zoom_factor).clamp(ZOOM_MIN, ZOOM_MAX);
                        let coords = evt.element_coordinates();
                        let scale_change = new_zoom / old_zoom;
                        pan_x.set(coords.x - (coords.x - pan_x()) * scale_change);
                        pan_y.set(coords.y - (coords.y - pan_y()) * scale_change);
                        zoom.set(new_zoom);
                    },

                    // Drag → pan.
                    onmousedown: move |evt| {
                        dragging.set(true);
                        let coords = evt.client_coordinates();
                        last_mouse.set((coords.x, coords.y));
                    },
                    onmousemove: move |evt| {
                        if !dragging() {
                            return;
                        }
                        let coords = evt.client_coordinates();
                        let (lx, ly) = last_mouse();
                        pan_x.set(pan_x() + (coords.x - lx));
                        pan_y.set(pan_y() + (coords.y - ly));
                        last_mouse.set((coords.x, coords.y));
                    },
                    onmouseup: move |_| dragging.set(false),
                    onmouseleave: move |_| dragging.set(false),

                    div {
                        class: "kf-studio-pages",
                        style: "{transform}",
                        for (i, page) in pages.read().iter().enumerate() {
                            div {
                                key: "{i}",
                                class: "kf-studio-page",
                                style: "width: {page.width_px}px; height: {page.height_px}px; margin-bottom: {PAGE_GAP_PX}px;",
                                div { class: "kf-render", dangerous_inner_html: "{page.svg}" }
                            }
                        }
                    }
                }
            }
        }
    }
}
