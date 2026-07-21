//! Setlist chart pane — the active song's keyflow chart, engraved as a real
//! A4 page **document** and synced to the session playhead.
//!
//! Renders with the CPU engraver pipeline (`keyflow::engraver` layout →
//! fontless SVG string, wasm-safe — no canvas/wgpu), using the same
//! **Master Rhythm / paginated A4** layout the site's chart editor uses so the
//! measures fill each system to the page edge. One page shows at a time, fit to
//! the viewport, with Prev/Next page controls and drag-to-pan + wheel-to-zoom
//! (the interaction pattern from the site's `live_editor`).
//!
//! - **chart source**: `SONG_CHARTS[project_guid].chart_text` (or the song's
//!   own `chart_text`), keyed off `ACTIVE_INDICES.song_index` +
//!   `SETLIST_STRUCTURE`.
//! - **static layer**: chart text → `keyflow::parse` → A4 paginated layout →
//!   one **fontless** SVG *per page*, re-generated only when the text changes.
//!   Each SVG is rendered inline via `dangerous_inner_html` (NOT an `<img
//!   blob>`), and the engraving fonts are injected once as `@font-face`
//!   (`editor_keyflow::font_face_css()`) so the SMuFL / chord / text glyphs
//!   resolve. (An SVG loaded through `<img>` does not apply `@font-face` web
//!   fonts — that was the old tofu-glyph bug.)
//! - **highlight overlay**: a second SVG with the active page's viewBox stacked
//!   on top; `ChartCursor` turns the playhead time into draw commands which
//!   become overlay `rect`/`line` elements, so the active-measure highlight
//!   scales pixel-perfectly with the page under it. During playback the view
//!   auto-follows the cursor's page.
//!
//! Playhead model: `ACTIVE_INDICES.song_progress` (0..1 over the transport
//! timeline, whose 0 is the count-in start) maps onto the chart's own timeline,
//! whose 0 is the first real measure — the count-in is a header snippet with
//! negative-time positions. The transport's section starts already include the
//! count-in lead-in (`count_in_seconds`, or — for hydrated setlists that leave
//! it `None` — the first section's `start_seconds`), so we subtract it and the
//! seek lands on the right measure (`ChartCursor::compute_at_time`).

use dioxus::prelude::*;
use std::cell::RefCell;

use keyflow::engraver::export::{SvgExportConfig, SvgSerializer};
use keyflow::engraver::fonts::ChartFontBundle;
use keyflow::engraver::layout::ChartLayoutMode;
use keyflow::engraver::layout::chart::cursor::{
    ChartCursor, CursorConfig, CursorState, CursorStyle, HighlightCommand,
};
use keyflow::engraver::layout::chart::{ChartLayoutConfig, ChartLayoutEngine, ChartLayoutResult};
use keyflow::engraver::style::MStyle;
use session_ui::{ACTIVE_INDICES, SETLIST_STRUCTURE, SONG_CHARTS};

/// Zoom bounds for the pannable page viewport (matches the site's editor).
const ZOOM_MIN: f64 = 0.1;
const ZOOM_MAX: f64 = 8.0;
/// Fraction of the viewport a fitted page fills (leaves a small margin).
const FIT_MARGIN: f64 = 0.96;

// ─── Layout cache (one per pane — wasm is single-threaded) ────────────────

/// One engraved page: its scene-space box (viewBox origin + size) and SVG.
#[derive(Clone)]
struct PageRender {
    svg: String,
    /// Scene-coordinate box of this page (also the overlay viewBox).
    x: f64,
    y: f64,
    w: f64,
    h: f64,
}

struct Pane {
    engine: ChartLayoutEngine,
    /// (key, layout, per-page renders)
    layout: Option<(u64, ChartLayoutResult, Vec<PageRender>)>,
}

thread_local! {
    static PANE: RefCell<Option<Pane>> = const { RefCell::new(None) };
}

fn with_pane<R>(f: impl FnOnce(&mut Pane) -> R) -> Option<R> {
    PANE.with(|cell| {
        let mut slot = cell.borrow_mut();
        if slot.is_none() {
            let font_bundle = match ChartFontBundle::new() {
                Ok(b) => b,
                Err(e) => {
                    tracing::error!("chart pane: font bundle failed: {e}");
                    return None;
                }
            };
            let style: &'static MStyle = Box::leak(Box::new(MStyle::new()));
            let engine = font_bundle.create_layout_engine(style);
            *slot = Some(Pane {
                engine,
                layout: None,
            });
        }
        slot.as_mut().map(f)
    })
}

fn text_key(text: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = std::collections::hash_map::DefaultHasher::new();
    text.hash(&mut h);
    h.finish()
}

impl Pane {
    /// Parse + lay out + serialize each page of `text` if it isn't cached.
    /// Returns the per-page renders on success.
    fn ensure(&mut self, text: &str) -> Option<Vec<PageRender>> {
        let key = text_key(text);
        if let Some((cached_key, _, pages)) = &self.layout {
            if *cached_key == key {
                return Some(pages.clone());
            }
        }

        let chart = match keyflow::parse(text) {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!("chart pane: parse failed: {e}");
                return None;
            }
        };

        // A4 page document with the **Master Rhythm** preset — the same layout
        // the site's chart editor uses: measures fill each system to the page
        // edge (not content-sized), true A4 page proportions. The paginated
        // path derives the count-in from the chart's `CountIn` section and gives
        // its beats negative-time positions (real measures at t=0), so the
        // playhead maps cleanly (see `ChartCursorOverlay`).
        //
        // `use_page_offsets(true)` lays pages out in scene space; we serialize
        // each page independently (its own `for_page` viewBox) so they can be
        // shown one at a time, fit and pannable, rather than as a filmstrip.
        let config = ChartLayoutConfig::master_rhythm().with_page_offsets(true);
        let mode = ChartLayoutMode::paginated_a4();
        let layout = self.engine.layout_chart_with_config(&chart, &mode, &config);

        let pages: Vec<PageRender> = if layout.pages.is_empty() {
            // Unpaginated fallback: the whole scene as a single page.
            let w = layout.total_width.max(1.0);
            let h = layout.total_height.max(60.0);
            let cfg = SvgExportConfig::for_page(0.0, 0.0, w, h);
            let svg = SvgSerializer::new(cfg).serialize(&layout.scene);
            vec![PageRender {
                svg,
                x: 0.0,
                y: 0.0,
                w,
                h,
            }]
        } else {
            layout
                .pages
                .iter()
                .map(|p| {
                    let cfg = SvgExportConfig::for_page(p.x_offset, p.y_offset, p.width, p.height);
                    let svg = SvgSerializer::new(cfg).serialize(&layout.scene);
                    PageRender {
                        svg,
                        x: p.x_offset,
                        y: p.y_offset,
                        w: p.width,
                        h: p.height,
                    }
                })
                .collect()
        };

        self.layout = Some((key, layout, pages.clone()));
        Some(pages)
    }

    /// Playhead time on the chart's own timeline → cursor state.
    fn cursor_state_at_time(&self, key: u64, chart_seconds: f64) -> Option<CursorState> {
        let (cached_key, layout, ..) = self.layout.as_ref()?;
        if *cached_key != key {
            return None;
        }
        let cursor = ChartCursor::new(CursorConfig {
            style: CursorStyle::MeasureHighlight,
            accent_color: [59, 130, 246, 255], // blue-500
            fill_alpha: 0.18,
            highlight_notehead: false,
            show_when_stopped: true,
            ..CursorConfig::default()
        });
        cursor.compute_at_time(layout, chart_seconds)
    }
}

/// `@font-face` CSS with the engraver's embedded fonts as data URIs — injected
/// once so the fontless chart SVGs resolve their families.
fn font_face_css() -> String {
    thread_local! {
        static CSS: RefCell<Option<String>> = const { RefCell::new(None) };
    }
    CSS.with(|cell| {
        let mut slot = cell.borrow_mut();
        if let Some(css) = slot.as_ref() {
            return css.clone();
        }
        let css = editor_keyflow::font_face_css().unwrap_or_else(|e| {
            tracing::error!("chart pane: font-face css failed: {e}");
            String::new()
        });
        *slot = Some(css.clone());
        css
    })
}

/// Compute the transport time on the chart timeline for `progress`.
fn chart_seconds_for(progress: Option<f64>) -> Option<f64> {
    let p = progress?;
    let indices = ACTIVE_INDICES.read();
    let idx = indices.song_index?;
    drop(indices);
    let setlist = SETLIST_STRUCTURE.read();
    let song = setlist.songs.get(idx)?;
    let duration = song.duration();
    if duration <= 0.0 {
        return None;
    }
    // Count-in / lead-in before the first real measure. Prefer the explicit
    // `count_in_seconds`; hydrated setlists leave it `None`, where the first
    // section's `start_seconds` IS the lead-in (a 2-measure count is a ~3.78 s
    // gap @127 bpm). Section starts sit on measure boundaries and both values
    // are rounded seconds, so a seek lands exactly on a boundary where float
    // noise can drop it into the previous measure — bias forward ~15 ms.
    let count_in = song
        .count_in_seconds
        .or_else(|| song.sections.first().map(|s| s.start_seconds))
        .unwrap_or(0.0);
    const BOUNDARY_BIAS_S: f64 = 0.015;
    Some(p.clamp(0.0, 1.0) * duration - count_in + BOUNDARY_BIAS_S)
}

// ─── Components ────────────────────────────────────────────────────────────

/// The chart pane: active song's chart document + playhead highlight. Shows a
/// quiet placeholder when the song has no chart.
#[component]
pub fn SessionChartPane() -> Element {
    // (guid-independent) chart text for the ACTIVE song. Recomputes only when
    // the cursor's song, the setlist structure, or the hydrated chart changes.
    let chart_text = use_memo(move || {
        let indices = ACTIVE_INDICES.read();
        let idx = indices.song_index?;
        drop(indices);
        let setlist = SETLIST_STRUCTURE.read();
        let song = setlist.songs.get(idx)?;
        let charts = SONG_CHARTS.read();
        charts
            .get(&song.project_guid)
            .map(|c| c.chart_text.clone())
            .or_else(|| song.chart_text.clone())
    });

    match chart_text() {
        Some(text) => rsx! {
            ChartCanvas { text }
        },
        None => rsx! {
            div { style: "display:flex; align-items:center; justify-content:center; min-height:80px;",
                span { style: "font-size:12px; color:#52525b;", "No chart for this song." }
            }
        },
    }
}

/// Static page document + pan/zoom viewport + page nav. Re-renders only when the
/// chart text changes; the playhead lives in `ChartCursorOverlay`.
#[component]
fn ChartCanvas(text: String) -> Element {
    let key = text_key(&text);
    let pages = with_pane(|pane| pane.ensure(&text)).flatten();

    let Some(pages) = pages else {
        return rsx! {
            div { style: "display:flex; align-items:center; justify-content:center; min-height:80px;",
                span { style: "font-size:12px; color:#ef4444;", "Chart failed to render." }
            }
        };
    };
    let n_pages = pages.len().max(1);
    // Cheap per-page dimensions for fitting (avoids cloning SVG strings).
    let dims: Vec<(f64, f64)> = pages.iter().map(|p| (p.w, p.h)).collect();

    // Which page is shown; pan/zoom of the viewport; measured viewport size.
    let mut current = use_signal(|| 0usize);
    let mut zoom = use_signal(|| 1.0_f64);
    let mut pan_x = use_signal(|| 0.0_f64);
    let mut pan_y = use_signal(|| 0.0_f64);
    let mut dragging = use_signal(|| false);
    let mut last_mouse = use_signal(|| (0.0_f64, 0.0_f64));
    let mut viewport = use_signal(|| None::<(f64, f64)>);

    // Clamp the page index if the document shrank (song switch).
    if current() >= n_pages {
        current.set(0);
    }

    // Auto-fit: whenever the shown page or the viewport size changes, scale the
    // page to fill the viewport (with a margin) and center it. Manual zoom/pan
    // don't touch `current`/`viewport`, so a user's zoom survives until they
    // change pages.
    {
        let dims = dims.clone();
        let cur_val = current();
        let vp = viewport();
        use_effect(use_reactive!(|(cur_val, vp)| {
            let Some((vw, vh)) = vp else { return };
            let (pw, ph) = dims.get(cur_val).copied().unwrap_or((595.0, 842.0));
            if pw <= 0.0 || ph <= 0.0 {
                return;
            }
            let z = ((vw / pw).min(vh / ph) * FIT_MARGIN).clamp(ZOOM_MIN, ZOOM_MAX);
            zoom.set(z);
            pan_x.set((vw - pw * z) / 2.0);
            pan_y.set((vh - ph * z) / 2.0);
        }));
    }

    let transform = use_memo(move || {
        format!(
            "transform: translate({}px, {}px) scale({}); transform-origin: 0 0;",
            pan_x(),
            pan_y(),
            zoom()
        )
    });

    let cur = current().min(n_pages - 1);
    let page = pages[cur].clone();
    let at_first = cur == 0;
    let at_last = cur + 1 >= n_pages;

    rsx! {
        document::Style { {font_face_css()} }
        div {
            style: "position:relative; width:100%; height:100%; min-height:0; overflow:hidden; background:#ffffff; user-select:none; touch-action:none; cursor:{drag_cursor(dragging())};",

            // Measure the viewport once mounted, then fit.
            onmounted: move |evt| {
                spawn(async move {
                    if let Ok(rect) = evt.data().get_client_rect().await {
                        viewport.set(Some((rect.size.width, rect.size.height)));
                    }
                });
            },
            // Wheel → zoom, anchored at the cursor.
            onwheel: move |evt| {
                evt.prevent_default();
                let delta_y = evt.delta().strip_units().y;
                let old = zoom();
                let factor = if delta_y < 0.0 { 1.08 } else { 0.925 };
                let new = (old * factor).clamp(ZOOM_MIN, ZOOM_MAX);
                let c = evt.element_coordinates();
                let k = new / old;
                pan_x.set(c.x - (c.x - pan_x()) * k);
                pan_y.set(c.y - (c.y - pan_y()) * k);
                zoom.set(new);
            },
            // Drag → pan.
            onmousedown: move |evt| {
                dragging.set(true);
                let c = evt.client_coordinates();
                last_mouse.set((c.x, c.y));
            },
            onmousemove: move |evt| {
                if !dragging() { return; }
                let c = evt.client_coordinates();
                let (lx, ly) = last_mouse();
                pan_x.set(pan_x() + (c.x - lx));
                pan_y.set(pan_y() + (c.y - ly));
                last_mouse.set((c.x, c.y));
            },
            onmouseup: move |_| dragging.set(false),
            onmouseleave: move |_| dragging.set(false),

            // The transformed stage holds exactly the current page + overlay.
            div {
                style: "position:absolute; top:0; left:0; {transform}",
                div {
                    style: "position:relative; width:{page.w}px; height:{page.h}px; box-shadow:0 1px 8px rgba(0,0,0,0.18);",
                    div { dangerous_inner_html: "{page.svg}" }
                    ChartCursorOverlay {
                        layout_key: key,
                        page_index: cur,
                        page_x: page.x,
                        page_y: page.y,
                        page_w: page.w,
                        page_h: page.h,
                        current,
                    }
                }
            }

            // Page controls — only when the chart is more than one page.
            if n_pages > 1 {
                div {
                    style: "position:absolute; bottom:10px; left:50%; transform:translateX(-50%); display:flex; align-items:center; gap:8px; background:rgba(24,24,27,0.82); color:#fff; padding:5px 8px; border-radius:9px; font-size:12px; box-shadow:0 2px 10px rgba(0,0,0,0.25);",
                    onmousedown: move |evt| evt.stop_propagation(),
                    button {
                        style: "border:0; background:transparent; color:{nav_color(!at_first)}; cursor:{nav_cursor(!at_first)}; font-size:15px; padding:0 6px;",
                        disabled: at_first,
                        onclick: move |_| { if current() > 0 { current.set(current() - 1); } },
                        "‹ Prev"
                    }
                    span { style: "opacity:0.85; min-width:52px; text-align:center;", "Page {cur + 1} / {n_pages}" }
                    button {
                        style: "border:0; background:transparent; color:{nav_color(!at_last)}; cursor:{nav_cursor(!at_last)}; font-size:15px; padding:0 6px;",
                        disabled: at_last,
                        onclick: move |_| { if current() + 1 < n_pages { current.set(current() + 1); } },
                        "Next ›"
                    }
                }
            }
        }
    }
}

fn drag_cursor(dragging: bool) -> &'static str {
    if dragging { "grabbing" } else { "grab" }
}
fn nav_color(enabled: bool) -> &'static str {
    if enabled { "#ffffff" } else { "#71717a" }
}
fn nav_cursor(enabled: bool) -> &'static str {
    if enabled { "pointer" } else { "default" }
}

/// The playhead overlay for the active page. Same viewBox as the page's SVG,
/// absolutely positioned over it. Re-renders at cursor rate (only this small
/// component), and — while playing — advances `current` to follow the cursor
/// across pages.
#[component]
fn ChartCursorOverlay(
    layout_key: u64,
    page_index: usize,
    page_x: f64,
    page_y: f64,
    page_w: f64,
    page_h: f64,
    current: Signal<usize>,
) -> Element {
    let (progress, playing) = {
        let indices = ACTIVE_INDICES.read();
        (indices.song_progress, indices.is_playing)
    };

    let chart_seconds = chart_seconds_for(progress);
    let state = chart_seconds
        .and_then(|t| with_pane(|pane| pane.cursor_state_at_time(layout_key, t)).flatten());

    // Auto-follow the cursor's page while playing (pages are 1-indexed).
    let cursor_page = state.as_ref().map(|s| s.page.saturating_sub(1) as usize);
    let mut current = current;
    use_effect(use_reactive!(|(cursor_page, playing)| {
        if playing
            && let Some(p) = cursor_page
            && p != *current.peek()
        {
            current.set(p);
        }
    }));

    // Scroll the highlight into view while playing (after a page switch).
    let measure = state.as_ref().map(|s| s.measure);
    use_effect(use_reactive!(|(measure, playing)| {
        if playing && measure.is_some() {
            document::eval(
                "document.getElementById('kf-playhead')?.scrollIntoView({block:'center', behavior:'smooth'});",
            );
        }
    }));

    // Only draw when the cursor is on THIS page.
    let Some(state) = state.filter(|s| s.page.saturating_sub(1) as usize == page_index) else {
        return rsx! {};
    };

    rsx! {
        svg {
            view_box: "{page_x} {page_y} {page_w} {page_h}",
            preserve_aspect_ratio: "xMinYMin meet",
            style: "position:absolute; inset:0; width:100%; height:100%; pointer-events:none;",
            circle {
                id: "kf-playhead",
                cx: "{state.cursor_x}",
                cy: "{state.cursor_y + state.cursor_height / 2.0}",
                r: "1",
                fill: "none",
            }
            for (i, cmd) in state.commands.iter().enumerate() {
                {render_command(i, cmd)}
            }
            line {
                x1: "{state.cursor_x}",
                y1: "{state.cursor_y - 4.0}",
                x2: "{state.cursor_x}",
                y2: "{state.cursor_y + state.cursor_height + 4.0}",
                stroke: "rgba(59,130,246,0.9)",
                stroke_width: "1.5",
            }
        }
    }
}

fn rgba_css(c: &[u8; 4], alpha_mul: f32) -> String {
    let a = (c[3] as f32 / 255.0 * alpha_mul).clamp(0.0, 1.0);
    format!("rgba({},{},{},{a:.3})", c[0], c[1], c[2])
}

/// One cursor draw command → overlay SVG element. Glyph commands (notehead
/// glow) are skipped — the pane uses measure highlighting only.
fn render_command(i: usize, cmd: &HighlightCommand) -> Element {
    match cmd {
        HighlightCommand::FillRect {
            x,
            y,
            width,
            height,
            color,
        } => rsx! {
            rect {
                key: "{i}",
                x: "{x}",
                y: "{y}",
                width: "{width}",
                height: "{height}",
                fill: rgba_css(color, 1.0),
            }
        },
        HighlightCommand::FillRoundedRect {
            x,
            y,
            width,
            height,
            radius,
            color,
        } => rsx! {
            rect {
                key: "{i}",
                x: "{x}",
                y: "{y}",
                width: "{width}",
                height: "{height}",
                rx: "{radius}",
                fill: rgba_css(color, 1.0),
            }
        },
        HighlightCommand::StrokeLine {
            x,
            y_top,
            y_bottom,
            color,
            width,
        } => rsx! {
            line {
                key: "{i}",
                x1: "{x}",
                y1: "{y_top}",
                x2: "{x}",
                y2: "{y_bottom}",
                stroke: rgba_css(color, 1.0),
                stroke_width: "{width}",
            }
        },
        HighlightCommand::StrokeGlyph { .. } | HighlightCommand::FillGlyph { .. } => rsx! {},
    }
}
