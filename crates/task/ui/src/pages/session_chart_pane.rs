//! Browser chart pane — the active song's keyflow chart, synced to the
//! session playhead.
//!
//! Adapted from `apps/fasttrackstudio/src/session_chart_pane.rs` for the
//! website's browser session player. It renders the current song's chart with
//! the same CPU engraver pipeline the site already uses (`keyflow::engraver`
//! layout → fontless SVG string, wasm-safe — no canvas/wgpu), and drives a
//! highlight overlay from the session-ui global signals which the streaming
//! player populates:
//!
//! - **chart source**: `SONG_CHARTS[project_guid].chart_text` (or the song's
//!   own `chart_text`), keyed off `ACTIVE_INDICES.song_index` +
//!   `SETLIST_STRUCTURE`.
//! - **static layer**: chart text → `keyflow::parse` → continuous-scroll
//!   layout → one **fontless** SVG, re-generated only when the text changes.
//!   The SVG is rendered **inline** via `dangerous_inner_html` (NOT an
//!   `<img blob>`), and the engraving fonts are injected once at page level as
//!   `@font-face` (`editor_keyflow::font_face_css()`) so the SMuFL / chord /
//!   text glyphs resolve. (An SVG loaded through `<img>` does not apply
//!   `@font-face` web fonts — that was the old tofu-glyph bug.)
//! - **highlight overlay**: a second SVG with the same viewBox stacked on top;
//!   `ChartCursor` turns the playhead tick into renderer-agnostic draw
//!   commands which become overlay `rect`/`line` elements, so the active
//!   measure highlight scales pixel-perfectly with the chart under it.
//!
//! Playhead model: the cursor rides `ACTIVE_INDICES.song_progress` mapped onto
//! the chart's own timeline — `chart seconds = count_in + progress × song
//! duration` — resolved through the layout's per-beat table
//! (`ChartCursor::compute_at_time`).

use dioxus::prelude::*;
use std::cell::RefCell;

use keyflow::engraver::export::{SvgExportConfig, SvgSerializer};
use keyflow::engraver::fonts::ChartFontBundle;
use keyflow::engraver::layout::ChartLayoutMode;
use keyflow::engraver::layout::chart::cursor::{
    ChartCursor, CursorConfig, CursorState, CursorStyle, HighlightCommand,
};
use keyflow::engraver::layout::chart::{
    Breakpoint, ChartLayoutConfig, ChartLayoutEngine, ChartLayoutResult,
};
use keyflow::engraver::style::MStyle;
use session_ui::{ACTIVE_INDICES, SETLIST_STRUCTURE, SONG_CHARTS};

/// Layout width in points. Fixed (tablet/desktop breakpoint); the SVG scales
/// to the pane's CSS width, so this only picks type sizes / measures-per-line.
const LAYOUT_WIDTH_PT: f64 = 640.0;

// ─── Layout cache (one per page — wasm is single-threaded) ────────────────

struct Pane {
    engine: ChartLayoutEngine,
    /// (key, layout, static svg, content width pt, content height pt)
    layout: Option<(u64, ChartLayoutResult, String, f64, f64)>,
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
    /// Parse + layout + serialize `text` if it isn't the cached chart.
    /// Returns `(svg, width_pt, height_pt)` on success.
    fn ensure(&mut self, text: &str) -> Option<(String, f64, f64)> {
        let key = text_key(text);
        if let Some((cached_key, _, svg, w, h)) = &self.layout {
            if *cached_key == key {
                return Some((svg.clone(), *w, *h));
            }
        }

        let chart = match keyflow::parse(text) {
            Ok(c) => c,
            Err(e) => {
                tracing::warn!("chart pane: parse failed: {e}");
                return None;
            }
        };

        // Paginated (US Letter) — render the whole FIRST page, count-in snippet
        // and all. The paginated path derives the count-in from the chart's
        // `CountIn` section and gives its beats negative-time positions (real
        // measures start at t=0), so the playhead maps cleanly (see
        // `ChartCursorOverlay`). `use_page_offsets = false` renders page 1 at the
        // scene origin, so its box is `0 0 w h` and the overlay (same viewBox)
        // lines up without a scene translation.
        let breakpoint = Breakpoint::from_viewport_pt(LAYOUT_WIDTH_PT);
        let mut config = ChartLayoutConfig::responsive_for(breakpoint);
        config.use_page_offsets = false;
        let mode = ChartLayoutMode::paginated_letter();
        let layout = self.engine.layout_chart_with_config(&chart, &mode, &config);

        // First page box (fall back to content bounds if the result is unpaginated).
        let (px, py, w, h) = layout
            .pages
            .first()
            .map(|p| (p.x_offset, p.y_offset, p.width, p.height))
            .unwrap_or((
                0.0,
                0.0,
                layout.total_width.max(LAYOUT_WIDTH_PT),
                layout.total_height.max(60.0),
            ));
        // Fontless SVG (the page carries @font-face for the same families).
        let config = SvgExportConfig::for_page(px, py, w, h);
        let mut serializer = SvgSerializer::new(config);
        let mut svg = serializer.serialize(&layout.scene);
        // Make the inline SVG fluid: width tracks the pane, the viewBox
        // keeps the aspect ratio (browsers derive height from it).
        if let Some(pos) = svg.find("width=\"") {
            if let Some(end) = svg[pos..].find(" viewBox") {
                svg.replace_range(pos..pos + end, "width=\"100%\"");
            }
        }

        self.layout = Some((key, layout, svg.clone(), w, h));
        Some((svg, w, h))
    }

    /// Playhead (seconds on the chart's own timeline, count-in at 0) →
    /// cursor state against the cached layout.
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

/// `@font-face` CSS with the engraver's embedded fonts as data URIs —
/// injected once so the fontless chart SVGs resolve their families. Reuses the
/// site's existing `editor_keyflow` helper (same font bundle) so this pane does
/// not need its own base64 embedding path.
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

// ─── Components ────────────────────────────────────────────────────────────

/// The chart pane: active song's chart + playhead highlight, auto-following
/// the setlist cursor. Shows a quiet placeholder when the song has no chart.
#[component]
pub fn SessionChartPane() -> Element {
    // (guid-independent) chart text for the ACTIVE song. Reads the cursor,
    // the setlist structure and the hydrated chart cache — recomputes only
    // when one of those changes, not on transport frames.
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

/// Static chart layer + overlay stack. Re-renders only when the chart text
/// changes; the playhead lives in `ChartCursorOverlay` below.
#[component]
fn ChartCanvas(text: String) -> Element {
    let key = text_key(&text);
    let rendered = with_pane(|pane| pane.ensure(&text)).flatten();

    let Some((svg, w, h)) = rendered else {
        return rsx! {
            div { style: "display:flex; align-items:center; justify-content:center; min-height:80px;",
                span { style: "font-size:12px; color:#ef4444;", "Chart failed to render." }
            }
        };
    };

    rsx! {
        document::Style { {font_face_css()} }
        div {
            id: "kf-chart-scroll",
            style: "max-height:70vh; overflow-y:auto; overflow-x:hidden; background:#ffffff;",
            div { style: "position:relative; width:100%;",
                div { dangerous_inner_html: "{svg}" }
                ChartCursorOverlay { layout_key: key, view_w: w, view_h: h }
            }
        }
    }
}

/// The playhead overlay: same viewBox as the static SVG, absolutely
/// positioned over it, containing only the cursor's highlight commands.
#[component]
fn ChartCursorOverlay(layout_key: u64, view_w: f64, view_h: f64) -> Element {
    // Cursor-rate signal (progress + structural changes) — only this small
    // component re-renders on it.
    let (progress, playing) = {
        let indices = ACTIVE_INDICES.read();
        (indices.song_progress, indices.is_playing)
    };

    // progress (0..1 over the transport timeline, whose 0 is the count-in
    // start) → seconds on the paginated chart's timeline, whose 0 is the first
    // REAL measure (count-in measures sit at negative time). The transport's
    // section starts already include the count-in (Intro begins at
    // `count_in_seconds`), so we subtract it: at the very start the playhead
    // sits in the count-in snippet (negative), and seeking to Intro lands
    // exactly on the first real measure instead of `count_in` measures late.
    let chart_seconds = progress.and_then(|p| {
        let indices = ACTIVE_INDICES.read();
        let idx = indices.song_index?;
        drop(indices);
        let setlist = SETLIST_STRUCTURE.read();
        let song = setlist.songs.get(idx)?;
        let duration = song.duration();
        // Count-in / lead-in before the first real measure. Prefer the explicit
        // `count_in_seconds`, but hydrated setlists leave it `None` — there the
        // first section's `start_seconds` IS the lead-in (e.g. a 2-measure count
        // renders as the first section starting at ~3.78 s @127 bpm). The
        // paginated chart puts real measures at t=0 with the count-in at negative
        // time, so subtracting this lead-in lands section seeks on the right
        // measure (Intro → first real measure, not `count_in` measures late).
        let count_in = song
            .count_in_seconds
            .or_else(|| song.sections.first().map(|s| s.start_seconds))
            .unwrap_or(0.0);
        // Section starts coincide with measure boundaries, and both the seek
        // time and the count-in are rounded seconds — so a section seek lands
        // *exactly* on a chart measure boundary, where float noise can drop it
        // into the previous measure. Bias forward by a hair (~15 ms, well under
        // a beat) so a boundary landing resolves into the measure it begins.
        const BOUNDARY_BIAS_S: f64 = 0.015;
        (duration > 0.0).then(|| p.clamp(0.0, 1.0) * duration - count_in + BOUNDARY_BIAS_S)
    });

    let state = chart_seconds
        .and_then(|t| with_pane(|pane| pane.cursor_state_at_time(layout_key, t)).flatten());

    // Auto-follow: keep the cursor in view while playing. Runs when the
    // active measure changes (the marker element moved), not per tick.
    let measure = state.as_ref().map(|s| s.measure);
    use_effect(use_reactive!(|(measure, playing)| {
        if playing && measure.is_some() {
            document::eval(
                "document.getElementById('kf-playhead')?.scrollIntoView({block:'center', behavior:'smooth'});",
            );
        }
    }));

    let Some(state) = state else {
        return rsx! {};
    };

    rsx! {
        svg {
            view_box: "0 0 {view_w} {view_h}",
            preserve_aspect_ratio: "xMinYMin meet",
            style: "position:absolute; inset:0; width:100%; height:100%; pointer-events:none;",
            // Scroll anchor at the cursor position (invisible).
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
            // Thin playhead line inside the highlighted measure.
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
