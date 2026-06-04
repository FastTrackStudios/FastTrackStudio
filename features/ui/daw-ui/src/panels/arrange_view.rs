//! ArrangeView — the main timeline, Reaper-style.
//!
//! Composes a [`TrackControlPanel`] sidebar on the left with a scrollable
//! timeline on the right: region/marker/tempo lanes + a time ruler across the
//! top and one lane per track (height `track.height`, aligned with the TCP
//! rows) carrying its clips, plus an envelope lane per visible envelope.
//!
//! Themed by [`crate::theming::ArrangeTheme`] — REAPER's palette-driven
//! arrange vocabulary, rendered the way REAPER composites it:
//! - ruler: `col_tl_bg/fg/fg2`, loop-point band (`col_tl_bgsel2`), the
//!   time-selection band (`col_tl_bgsel`) + its `timesel_drawmode` shading
//!   over the arrange body; the tempo lane (`ts_lane_*`/`col_tsigmark`)
//!   sits at the bottom of the ruler;
//! - grid: the `col_gridlines2/3/''` measure→beat→sub hierarchy (musical
//!   when a tempo is supplied, time-based otherwise), with REAPER's
//!   zoom-gating (levels drop out when their spacing gets too dense);
//! - lanes: alternating `col_tr1/2_bg` (+ `selcol_*` when selected),
//!   `arrange_vgrid` shading in the empty area below the last track;
//! - items: per-parity `col_mi_bg/2` bodies tinted by the item colour at
//!   `itembg_drawmode` strength, waveform peaks (`col_tr*_peaks`),
//!   `col_mi_label(_sel)` text, selected bodies (`col_tr*_itembgsel` +
//!   `selitem_tag` bar), fade triangles (`fadezone_color` fill,
//!   `col_mi_fades` line) and the mute overlay;
//! - envelopes: one lane per visible envelope (`col_env*` curve over a
//!   dimmed row, `col_envlane*_divline` dividers);
//! - marker/region lanes: `marker*`/`region*` flags and bands.
//!
//! The ruler shares the lanes' horizontal scroll: the lane scroller's
//! `onscroll` mirrors `scroll_left` into the ruler content's offset.

use crate::panels::model::{EnvelopeView, MarkerView, RegionView, TempoMarkerView, TrackView};
use crate::panels::track_control_panel::TrackControlPanel;
use crate::prelude::*;
use crate::theming::{Color, use_theme};

/// Adaptive tick spacing: the smallest "nice" step whose px spacing at this
/// zoom clears `min_px`. Steps follow ruler conventions (1/2/5/10/15/30s,
/// then minutes).
fn pick_step(pps: f64, min_px: f64) -> f64 {
    const STEPS: [f64; 12] = [
        0.1, 0.25, 0.5, 1.0, 2.0, 5.0, 10.0, 15.0, 30.0, 60.0, 120.0, 300.0,
    ];
    for s in STEPS {
        if s * pps >= min_px {
            return s;
        }
    }
    600.0
}

/// Ruler label for a time in seconds (`m:ss` / `m:ss.t` under a second-level
/// zoom).
fn time_label(t: f64, step: f64) -> String {
    let m = (t / 60.0).floor() as i64;
    let s = t - m as f64 * 60.0;
    if step < 1.0 {
        format!("{m}:{s:04.1}")
    } else {
        format!("{m}:{s:02.0}")
    }
}

/// The grid/ruler step hierarchy: REAPER's measure → beat → sub-beat levels.
/// Musical when a tempo is known; "nice" time steps otherwise. Levels are
/// zoom-gated: a level collapses into the one above when its px spacing gets
/// too dense (REAPER stops drawing in-between lines at low zoom).
struct GridSteps {
    /// Labeled level (measure / major time step), seconds.
    major: f64,
    /// Beat level, seconds (== major when gated out).
    beat: f64,
    /// Sub-beat level, seconds (== beat when gated out).
    sub: f64,
    /// Whether `major` is a bar (label = bar number).
    musical: bool,
}

fn grid_steps(pps: f64, bpm: Option<f64>, beats_per_measure: u32) -> GridSteps {
    match bpm {
        Some(bpm) if bpm > 1.0 => {
            let beat = 60.0 / bpm;
            let mut major = beat * beats_per_measure.max(1) as f64;
            // Zoomed far out: label every 2^n measures.
            while major * pps < 60.0 {
                major *= 2.0;
            }
            // Beat lines drop out below ~10px spacing (REAPER stops drawing
            // in-between lines once they crowd).
            let beat = if beat * pps >= 10.0 { beat } else { major };
            // Sub-beat: halve while the spacing stays readable.
            let mut sub = beat;
            while sub / 2.0 * pps >= 8.0 {
                sub /= 2.0;
            }
            GridSteps {
                major,
                beat,
                sub,
                musical: true,
            }
        }
        _ => {
            let major = pick_step(pps, 90.0);
            let beat = pick_step(pps, 18.0).min(major);
            let sub = pick_step(pps, 7.0).min(beat);
            GridSteps {
                major,
                beat,
                sub,
                musical: false,
            }
        }
    }
}

/// The arrange view. `pps` = pixels per second (horizontal zoom); `tcp_width`
/// is the control-sidebar width; `seconds` is the visible timeline length.
///
/// Optional project data: `playhead`/`cursor` (s) draw the cursors,
/// `markers`/`regions`/`tempo_markers` add their ruler lanes,
/// `time_sel`/`loop_range` draw the selection/loop bands, and `bpm`
/// (+ `beats_per_measure`) switches the grid + ruler to musical bars.
#[component]
pub fn ArrangeView(
    tracks: Vec<TrackView>,
    #[props(default = 12.0)] pps: f64,
    #[props(default = 380)] tcp_width: u32,
    #[props(default = 120.0)] seconds: f64,
    #[props(default)] playhead: Option<f64>,
    #[props(default)] cursor: Option<f64>,
    #[props(default)] markers: Vec<MarkerView>,
    #[props(default)] regions: Vec<RegionView>,
    #[props(default)] tempo_markers: Vec<TempoMarkerView>,
    #[props(default)] time_sel: Option<(f64, f64)>,
    #[props(default)] loop_range: Option<(f64, f64)>,
    #[props(default)] bpm: Option<f64>,
    #[props(default = 4)] beats_per_measure: u32,
) -> Element {
    let content_w = (seconds * pps).max(600.0) as u32;

    let g = grid_steps(pps, bpm, beats_per_measure);
    let n_at = |step: f64| (seconds / step).ceil() as i64;
    // Total lane height — track rows + visible envelope lanes (the grid
    // covers the tracks; `arrange_vgrid` shades the empty area below).
    let lanes_h: u32 = tracks.iter().map(|t| t.total_height()).sum();

    // The lanes scroller owns horizontal scroll; the ruler mirrors it.
    let mut scroll_x = use_signal(|| 0.0f64);

    let theme = use_theme().theme;
    let ar = theme.arrange;
    let border = theme.tokens.border.css();

    // Ruler lanes: regions on top, markers under them, the time scale, and
    // the tempo/time-signature lane at the bottom (REAPER's stacking).
    let region_lane_h = if regions.is_empty() { 0 } else { 14u32 };
    let marker_lane_h = if markers.is_empty() { 0 } else { 14u32 };
    let ts_lane_h = if tempo_markers.is_empty() { 0 } else { 13u32 };
    let scale_h = 26u32;
    let ruler_h = region_lane_h + marker_lane_h + scale_h + ts_lane_h;
    let scale_top = region_lane_h + marker_lane_h;

    let ruler_bg = ar.ruler_bg.css();
    let ruler_fg = ar.ruler_fg.css();
    let ruler_fg2 = ar.ruler_fg2.css();
    let empty_bg = ar.empty_bg.css();
    let arrange_bg = ar.bg.css();
    let grid_measure = ar.grid_measure.css();
    let grid_beat = ar.grid_beat.css();
    let grid_sub = ar.grid_sub.css();
    let vgrid = ar.vgrid.css();
    let edit_cursor = ar.edit_cursor.css();
    let play_cursor = ar.play_cursor.css();

    let span_px = |range: (f64, f64)| {
        let (a, b) = (range.0.min(range.1), range.0.max(range.1));
        (a * pps, ((b - a) * pps).max(1.0))
    };

    rsx! {
        div {
            style: format!(
                "display:flex; flex-direction:column; height:100%; min-height:0; background:{empty_bg};"
            ),

            // ── Ruler block: spacer over the TCP, lanes + time scale right ──
            div {
                style: format!(
                    "flex:0 0 {ruler_h}px; height:{ruler_h}px; display:flex; \
                     border-bottom:1px solid {border}; background:{ruler_bg};"
                ),
                div { style: format!("flex:0 0 {tcp_width}px; border-right:1px solid {border};") }
                div {
                    style: "flex:1 1 0; position:relative; overflow:hidden;",
                    div {
                        // Mirrors the lane scroller's horizontal offset.
                        style: format!(
                            "position:relative; width:{content_w}px; height:100%; \
                             left:{x:.1}px;",
                            x = -scroll_x(),
                        ),

                        // Region lane.
                        if region_lane_h > 0 {
                            div {
                                style: format!(
                                    "position:absolute; left:0; right:0; top:0; height:{region_lane_h}px; \
                                     background:{bg};",
                                    bg = ar.region_lane_bg.css(),
                                ),
                                for r in regions.iter() {
                                    {
                                        let (x, w) = span_px((r.start, r.end));
                                        let fill = r.color.as_deref().and_then(Color::hex).unwrap_or(ar.region);
                                        rsx! {
                                            div {
                                                key: "r{r.idx}",
                                                title: "{r.name}",
                                                style: format!(
                                                    "position:absolute; left:{x:.1}px; width:{w:.1}px; top:0; bottom:0; \
                                                     background:{fill}; border-left:1px solid {edge}; \
                                                     border-right:1px solid {edge}; color:{fg}; font-size:9px; \
                                                     padding:1px 4px; white-space:nowrap; overflow:hidden;",
                                                    fill = fill.css(),
                                                    edge = ar.region_edge.css(),
                                                    fg = ar.region_lane_text.css(),
                                                ),
                                                "{r.name}"
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // Marker lane.
                        if marker_lane_h > 0 {
                            div {
                                style: format!(
                                    "position:absolute; left:0; right:0; top:{region_lane_h}px; \
                                     height:{marker_lane_h}px; background:{bg};",
                                    bg = ar.marker_lane_bg.css(),
                                ),
                                for m in markers.iter() {
                                    {
                                        let fill = m.color.as_deref().and_then(Color::hex).unwrap_or(ar.marker);
                                        rsx! {
                                            div {
                                                key: "m{m.idx}",
                                                title: "{m.name}",
                                                style: format!(
                                                    "position:absolute; left:{x:.1}px; top:0; bottom:0; \
                                                     border-left:2px solid {edge}; background:{fill}; \
                                                     color:{fg}; font-size:9px; font-weight:700; \
                                                     padding:1px 4px 1px 3px; white-space:nowrap;",
                                                    x = m.time * pps,
                                                    edge = ar.marker_edge.css(),
                                                    fill = fill.css(),
                                                    fg = ar.marker_lane_text.css(),
                                                ),
                                                "{m.idx} {m.name}"
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        // Time scale (loop band under everything, then ticks).
                        div {
                            style: format!(
                                "position:absolute; left:0; right:0; top:{scale_top}px; height:{scale_h}px;"
                            ),
                            if let Some(range) = loop_range {
                                {
                                    let (x, w) = span_px(range);
                                    rsx! { div { style: format!(
                                        "position:absolute; left:{x:.1}px; width:{w:.1}px; top:0; bottom:0; \
                                         background:{bg};", bg = ar.ruler_loop_bg.css()) } }
                                }
                            }
                            if let Some(range) = time_sel {
                                {
                                    let (x, w) = span_px(range);
                                    rsx! { div { style: format!(
                                        "position:absolute; left:{x:.1}px; width:{w:.1}px; top:0; bottom:0; \
                                         background:{bg};", bg = ar.ruler_sel_bg.css()) } }
                                }
                            }
                            // Minor ticks along the bottom edge.
                            if g.beat < g.major {
                                for i in 0..n_at(g.beat) {
                                    if (i as f64 * g.beat / g.major).fract() > 1e-9 {
                                        div {
                                            key: "n{i}",
                                            style: format!(
                                                "position:absolute; bottom:0; height:7px; left:{x:.1}px; \
                                                 width:1px; background:{ruler_fg2};",
                                                x = i as f64 * g.beat * pps,
                                            ),
                                        }
                                    }
                                }
                            }
                            // Major ticks + labels (bar numbers when musical).
                            for i in 0..n_at(g.major) {
                                div {
                                    key: "M{i}",
                                    style: format!(
                                        "position:absolute; top:0; bottom:0; left:{x:.1}px; \
                                         border-left:1px solid {ruler_fg2}; padding:2px 0 0 4px; \
                                         font-size:9px; color:{ruler_fg}; \
                                         font-variant-numeric:tabular-nums; white-space:nowrap;",
                                        x = i as f64 * g.major * pps,
                                    ),
                                    if g.musical {
                                        "{i as f64 * g.major * bpm.unwrap_or(120.0) / 60.0 / beats_per_measure.max(1) as f64 + 1.0:.0}"
                                    } else {
                                        "{time_label(i as f64 * g.major, g.major)}"
                                    }
                                }
                            }
                        }

                        // Tempo / time-signature lane (`ts_lane_*`).
                        if ts_lane_h > 0 {
                            div {
                                style: format!(
                                    "position:absolute; left:0; right:0; bottom:0; height:{ts_lane_h}px; \
                                     background:{bg};",
                                    bg = ar.ts_lane_bg.css(),
                                ),
                                for (i, t) in tempo_markers.iter().enumerate() {
                                    div {
                                        key: "t{i}",
                                        title: "Tempo {t.bpm} BPM, {t.num}/{t.den}",
                                        style: format!(
                                            "position:absolute; left:{x:.1}px; top:0; bottom:0; \
                                             border-left:2px solid {mark}; color:{fg}; \
                                             background:{mark_bg}; font-size:8px; font-weight:700; \
                                             padding:1px 4px 0 3px; white-space:nowrap;",
                                            x = t.time * pps,
                                            mark = ar.tsig.css(),
                                            mark_bg = ar.tsig.with_alpha(48).css(),
                                            fg = ar.ts_lane_text.css(),
                                        ),
                                        "{t.bpm:.0} {t.num}/{t.den}"
                                    }
                                }
                            }
                        }

                        // Cursors carry into the ruler, REAPER-style.
                        if let Some(t) = cursor {
                            div { style: format!(
                                "position:absolute; top:0; bottom:0; left:{x:.1}px; width:1px; \
                                 background:{edit_cursor};", x = t * pps) }
                        }
                        if let Some(t) = playhead {
                            div { style: format!(
                                "position:absolute; top:0; bottom:0; left:{x:.1}px; width:1px; \
                                 background:{play_cursor};", x = t * pps) }
                        }
                    }
                }
            }

            // ── Body: TCP sidebar + lanes, sharing one vertical scroll ──
            div {
                style: "flex:1 1 0; min-height:0; display:flex; overflow-y:auto;",

                TrackControlPanel { tracks: tracks.clone(), width: tcp_width, scroll: false }

                // Timeline lanes (own horizontal scroll, mirrored to the ruler).
                div {
                    style: format!(
                        "flex:1 1 0; min-width:0; overflow-x:auto; position:relative; \
                         background:{empty_bg};"
                    ),
                    onscroll: move |evt| {
                        scroll_x.set(evt.data.scroll_left());
                    },
                    // `arrange_vgrid` shading in the empty area below the tracks.
                    for i in 0..n_at(g.major) {
                        div {
                            key: "v{i}",
                            style: format!(
                                "position:absolute; top:{lanes_h}px; bottom:0; left:{x:.1}px; \
                                 width:1px; background:{vgrid}; pointer-events:none;",
                                x = i as f64 * g.major * pps,
                            ),
                        }
                    }
                    // NOTE: no `min-height:100%` here — blitz resolves the
                    // percentage against an indefinite scroll height and
                    // culls the *in-flow* children (the lanes) entirely.
                    div {
                        style: format!(
                            "position:relative; width:{content_w}px; background:{arrange_bg};"
                        ),
                        // Lane rows (the grid draws over them, like REAPER).
                        for (idx, track) in tracks.iter().enumerate() {
                            TrackLanes { key: "{track.id}", track: track.clone(), pps, alt: idx % 2 == 1 }
                        }

                        // Grid hierarchy over the tracks: sub, beat, measure.
                        if g.sub < g.beat {
                            for i in 0..n_at(g.sub) {
                                if (i as f64 * g.sub / g.beat).fract() > 1e-9 {
                                    div {
                                        key: "s{i}",
                                        style: format!(
                                            "position:absolute; top:0; height:{lanes_h}px; left:{x:.1}px; \
                                             width:1px; background:{grid_sub}; pointer-events:none;",
                                            x = i as f64 * g.sub * pps,
                                        ),
                                    }
                                }
                            }
                        }
                        if g.beat < g.major {
                            for i in 0..n_at(g.beat) {
                                if (i as f64 * g.beat / g.major).fract() > 1e-9 {
                                    div {
                                        key: "b{i}",
                                        style: format!(
                                            "position:absolute; top:0; height:{lanes_h}px; left:{x:.1}px; \
                                             width:1px; background:{grid_beat}; pointer-events:none;",
                                            x = i as f64 * g.beat * pps,
                                        ),
                                    }
                                }
                            }
                        }
                        for i in 0..n_at(g.major) {
                            div {
                                key: "g{i}",
                                style: format!(
                                    "position:absolute; top:0; height:{lanes_h}px; left:{x:.1}px; \
                                     width:1px; background:{grid_measure}; pointer-events:none;",
                                    x = i as f64 * g.major * pps,
                                ),
                            }
                        }

                        // Time-selection shading over the arrange body.
                        if let Some(range) = time_sel {
                            {
                                let (x, w) = span_px(range);
                                rsx! { div { style: format!(
                                    "position:absolute; left:{x:.1}px; width:{w:.1}px; top:0; \
                                     height:{lanes_h}px; background:{bg}; pointer-events:none;",
                                    bg = ar.timesel.css()) } }
                            }
                        }

                        // Cursors over everything.
                        if let Some(t) = cursor {
                            div { style: format!(
                                "position:absolute; top:0; bottom:0; left:{x:.1}px; width:1px; \
                                 background:{edit_cursor}; pointer-events:none;", x = t * pps) }
                        }
                        if let Some(t) = playhead {
                            div { style: format!(
                                "position:absolute; top:0; bottom:0; left:{x:.1}px; width:1px; \
                                 background:{play_cursor}; pointer-events:none;", x = t * pps) }
                        }
                    }
                }
            }
        }
    }
}

/// One track's arrange rows: the clip lane plus an envelope lane per visible
/// envelope (heights match the TCP side via [`TrackView::total_height`]).
#[component]
fn TrackLanes(track: TrackView, pps: f64, alt: bool) -> Element {
    let envelopes: Vec<EnvelopeView> = track
        .envelopes
        .iter()
        .filter(|e| e.visible)
        .cloned()
        .collect();
    rsx! {
        Lane { track: track.clone(), pps, alt }
        for (i, env) in envelopes.into_iter().enumerate() {
            EnvelopeLane { key: "e{i}", envelope: env, pps, alt }
        }
    }
}

/// One arrangement lane: alternating row background (`col_tr1/2_bg`,
/// `selcol_*` when the track is selected), the divider line, and the track's
/// items rendered REAPER-style (parity body + colour tint, peaks, label,
/// fades, selection + mute states).
#[component]
fn Lane(track: TrackView, pps: f64, alt: bool) -> Element {
    let theme = use_theme().theme;
    let ar = theme.arrange;
    let i = alt as usize;
    let selected = (track.selected)();
    let row_bg = if selected {
        ar.sel_row_bg[i].css()
    } else {
        ar.row_bg[i].css()
    };
    let divider = ar.row_divider[i].css();
    let item_edge = ar.item_edge.css();
    let track_color = track.color.as_deref().and_then(Color::hex);
    let item_h = track.height.saturating_sub(4) as f64;

    rsx! {
        div {
            style: format!(
                "position:relative; height:{h}px; background:{row_bg}; \
                 border-bottom:1px solid {divider}; box-sizing:border-box;",
                h = track.height,
            ),
            for (ci, clip) in track.clips.iter().enumerate() {
                {
                    // Item body: the item/track colour tinted over the parity
                    // background at `itembg_drawmode` strength; selected items
                    // switch to the `itembgsel` body.
                    let color = clip.color.as_deref().and_then(Color::hex).or(track_color);
                    let body = if clip.selected {
                        ar.item_bg_sel[i]
                    } else {
                        match color {
                            Some(c) => ar.item_bg[i].mix(c, ar.item_blend),
                            None => ar.item_bg[i],
                        }
                    };
                    let label = if clip.selected { ar.item_label_sel } else { ar.item_label };
                    let x = clip.start * pps;
                    let w = (clip.length * pps).max(2.0);
                    let fade_in_w = (clip.fade_in * pps).min(w);
                    let fade_out_w = (clip.fade_out * pps).min(w);
                    // Waveform peaks: REAPER's asymmetric model — the top
                    // boundary follows each column's max, the bottom its min,
                    // around the zero line at the item's vertical centre.
                    let peaks_path = (!clip.peaks.is_empty()).then(|| {
                        let mid = item_h / 2.0;
                        let half = mid - 1.0;
                        let n = clip.peaks.len().max(2) as f64;
                        let step = w / (n - 1.0);
                        let mut top = String::new();
                        let mut bottom = String::new();
                        for (pi, (pmax, pmin)) in clip.peaks.iter().enumerate() {
                            let px = pi as f64 * step;
                            let up = (*pmax as f64).clamp(-1.0, 1.0) * half;
                            let dn = (*pmin as f64).clamp(-1.0, 1.0) * half;
                            top.push_str(&format!("{px:.1},{:.1} ", mid - up));
                            bottom.insert_str(0, &format!("{px:.1},{:.1} ", mid - dn));
                        }
                        format!("{top}{bottom}")
                    });
                    rsx! {
                        div {
                            key: "c{ci}",
                            title: "{clip.name}",
                            style: format!(
                                "position:absolute; top:2px; height:{item_h}px; left:{x:.1}px; \
                                 width:{w:.1}px; background:{body}; border:1px solid {item_edge}; \
                                 border-radius:3px; overflow:hidden; box-sizing:border-box; \
                                 font-size:10px; color:{fg}; font-weight:700; \
                                 white-space:nowrap; text-overflow:ellipsis;",
                                body = body.css(),
                                fg = label.css(),
                            ),

                            // Peaks under the label (`col_tr1/2_peaks`).
                            if let Some(points) = peaks_path {
                                svg {
                                    width: "{w:.0}",
                                    height: "{item_h:.0}",
                                    style: "position:absolute; left:0; top:0; pointer-events:none;",
                                    polygon {
                                        points,
                                        fill: ar.peaks[i].css(),
                                    }
                                }
                            }

                            div { style: "position:relative; padding:1px 5px; pointer-events:none;", "{clip.name}" }

                            // Fade triangles (`fadezone` fill + `col_mi_fades` line).
                            if fade_in_w >= 2.0 {
                                svg {
                                    width: "{fade_in_w:.0}",
                                    height: "{item_h:.0}",
                                    style: "position:absolute; left:0; top:0; pointer-events:none;",
                                    polygon {
                                        points: format!("0,0 {fade_in_w:.1},0 0,{item_h:.1}"),
                                        fill: ar.fadezone.css(),
                                    }
                                    line {
                                        x1: "0", y1: "{item_h:.1}", x2: "{fade_in_w:.1}", y2: "0",
                                        stroke: ar.fade_line.css(),
                                        stroke_width: "1",
                                    }
                                }
                            }
                            if fade_out_w >= 2.0 {
                                svg {
                                    width: "{fade_out_w:.0}",
                                    height: "{item_h:.0}",
                                    style: format!(
                                        "position:absolute; left:{x:.1}px; top:0; pointer-events:none;",
                                        x = w - fade_out_w - 2.0,
                                    ),
                                    polygon {
                                        points: format!("0,0 {fade_out_w:.1},0 {fade_out_w:.1},{item_h:.1}"),
                                        fill: ar.fadezone.css(),
                                    }
                                    line {
                                        x1: "0", y1: "0", x2: "{fade_out_w:.1}", y2: "{item_h:.1}",
                                        stroke: ar.fade_line.css(),
                                        stroke_width: "1",
                                    }
                                }
                            }

                            // Selected-item tag bar (when the theme enables it).
                            if clip.selected && ar.selitem_tag.is_some() {
                                div { style: format!(
                                    "position:absolute; left:0; right:0; bottom:0; height:3px; \
                                     background:{c}; pointer-events:none;",
                                    c = color.unwrap_or(ar.selitem_tag.unwrap()).css()) }
                            }

                            // Mute overlay.
                            if clip.muted {
                                div { style: format!(
                                    "position:absolute; inset:0; background:{c}; pointer-events:none;",
                                    c = ar.mute_overlay.css()) }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// One envelope lane: a dimmed row with the envelope curve drawn over it
/// (`col_env*` colour, filled below the line REAPER-style).
#[component]
fn EnvelopeLane(envelope: EnvelopeView, pps: f64, alt: bool) -> Element {
    let theme = use_theme().theme;
    let ar = theme.arrange;
    let i = alt as usize;
    let h = envelope.height;
    let curve = envelope
        .color
        .as_deref()
        .and_then(Color::hex)
        .unwrap_or(ar.env_default);

    // Curve polyline + the area fill beneath it.
    let inner_h = h.saturating_sub(2) as f64;
    let xy =
        |t: f64, v: f32| -> (f64, f64) { (t * pps, (1.0 - v.clamp(0.0, 1.0)) as f64 * inner_h) };
    let mut line_pts = String::new();
    for (t, v) in &envelope.points {
        let (x, y) = xy(*t, *v);
        line_pts.push_str(&format!("{x:.1},{y:.1} "));
    }
    // Close the curve down to the lane floor for the area fill.
    let fill_pts = match (envelope.points.first(), envelope.points.last()) {
        (Some(first), Some(last)) => {
            let (fx, _) = xy(first.0, first.1);
            let (lx, _) = xy(last.0, last.1);
            format!("{line_pts}{lx:.1},{inner_h:.1} {fx:.1},{inner_h:.1}")
        }
        _ => String::new(),
    };
    let svg_w = envelope
        .points
        .last()
        .map(|p| (p.0 * pps).ceil().max(2.0))
        .unwrap_or(2.0);

    rsx! {
        div {
            style: format!(
                "position:relative; height:{h}px; background:{bg}; \
                 border-bottom:1px solid {divider}; box-sizing:border-box; overflow:hidden;",
                bg = ar.row_bg[i].darken(0.25).css(),
                divider = ar.envlane_divider[i].css(),
            ),
            if !envelope.points.is_empty() {
                svg {
                    width: "{svg_w:.0}",
                    height: "{inner_h:.0}",
                    style: "position:absolute; left:0; top:1px; pointer-events:none;",
                    if !fill_pts.is_empty() {
                        polygon { points: fill_pts, fill: curve.with_alpha(40).css() }
                    }
                    polyline {
                        points: line_pts,
                        fill: "none",
                        stroke: curve.css(),
                        stroke_width: "1.5",
                    }
                    // Envelope points, REAPER-style square handles.
                    for (pi, (t, v)) in envelope.points.iter().enumerate() {
                        {
                            let (x, y) = xy(*t, *v);
                            rsx! {
                                rect {
                                    key: "p{pi}",
                                    x: "{x - 2.0:.1}",
                                    y: "{y - 2.0:.1}",
                                    width: "4",
                                    height: "4",
                                    fill: curve.css(),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
