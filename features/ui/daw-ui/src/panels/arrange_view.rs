//! ArrangeView — the main timeline, Reaper-style.
//!
//! Composes a [`TrackControlPanel`] sidebar on the left with a scrollable
//! timeline on the right: a time ruler across the top and one lane per track
//! (height `track.height`, aligned with the TCP rows) carrying its clips.
//!
//! Themed by [`crate::theming::ArrangeTheme`] — REAPER's palette-driven
//! arrange vocabulary: `col_tl_*` ruler, the three-level `col_gridlines*`
//! hierarchy (measure / beat / sub-beat, drawmode alphas pre-applied),
//! alternating `col_tr1/2_bg` lane rows, `col_mi_*` item colours and the
//! edit/play cursors.

use crate::panels::model::TrackView;
use crate::panels::track_control_panel::TrackControlPanel;
use crate::prelude::*;
use crate::theming::use_theme;

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

/// The arrange view. `pps` = pixels per second (horizontal zoom); `tcp_width`
/// is the control-sidebar width; `seconds` is the visible timeline length.
/// `playhead`/`cursor` (seconds) draw the play/edit cursor lines when set.
#[component]
pub fn ArrangeView(
    tracks: Vec<TrackView>,
    #[props(default = 12.0)] pps: f64,
    #[props(default = 380)] tcp_width: u32,
    #[props(default = 120.0)] seconds: f64,
    #[props(default)] playhead: Option<f64>,
    #[props(default)] cursor: Option<f64>,
) -> Element {
    let ruler_h = 26u32;
    let content_w = (seconds * pps).max(600.0) as u32;

    // Tick hierarchy (REAPER's measure/beat/sub-beat grid, mapped to time):
    // labeled major ticks, minor ticks between them, sub-ticks when zoomed in.
    let major = pick_step(pps, 90.0);
    let minor = pick_step(pps, 18.0).min(major);
    let sub = pick_step(pps, 7.0).min(minor);
    let n_at = |step: f64| (seconds / step).ceil() as i64;

    let theme = use_theme().theme;
    let ar = theme.arrange;
    let border = theme.tokens.border.css();

    let ruler_bg = ar.ruler_bg.css();
    let ruler_fg = ar.ruler_fg.css();
    let ruler_fg2 = ar.ruler_fg2.css();
    let arrange_bg = ar.bg.css();
    let empty_bg = ar.empty_bg.css();
    let grid_measure = ar.grid_measure.css();
    let grid_beat = ar.grid_beat.css();
    let grid_sub = ar.grid_sub.css();
    let edit_cursor = ar.edit_cursor.css();
    let play_cursor = ar.play_cursor.css();

    rsx! {
        div {
            style: format!(
                "display:flex; flex-direction:column; height:100%; min-height:0; background:{empty_bg};"
            ),

            // ── Ruler row: spacer over the TCP, time ruler over the lanes ──
            div {
                style: format!(
                    "flex:0 0 {ruler_h}px; height:{ruler_h}px; display:flex; \
                     border-bottom:1px solid {border}; background:{ruler_bg};"
                ),
                div { style: format!("flex:0 0 {tcp_width}px; border-right:1px solid {border};") }
                div {
                    style: "flex:1 1 0; position:relative; overflow:hidden;",
                    div {
                        style: format!("position:relative; width:{content_w}px; height:100%;"),
                        // Minor ticks: short marks along the bottom edge.
                        if minor < major {
                            for i in 0..n_at(minor) {
                                if (i as f64 * minor / major).fract() > 1e-9 {
                                    div {
                                        key: "n{i}",
                                        style: format!(
                                            "position:absolute; bottom:0; height:7px; left:{x:.1}px; \
                                             width:1px; background:{ruler_fg2};",
                                            x = i as f64 * minor * pps,
                                        ),
                                    }
                                }
                            }
                        }
                        // Major ticks: full-height mark + time label.
                        for i in 0..n_at(major) {
                            div {
                                key: "m{i}",
                                style: format!(
                                    "position:absolute; top:0; bottom:0; left:{x:.1}px; \
                                     border-left:1px solid {ruler_fg2}; padding:2px 0 0 4px; \
                                     font-size:9px; color:{ruler_fg}; \
                                     font-variant-numeric:tabular-nums; white-space:nowrap;",
                                    x = i as f64 * major * pps,
                                ),
                                "{time_label(i as f64 * major, major)}"
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

                // Timeline lanes (own horizontal scroll).
                div {
                    style: format!("flex:1 1 0; min-width:0; overflow-x:auto; background:{empty_bg};"),
                    div {
                        style: format!("position:relative; width:{content_w}px; background:{arrange_bg};"),

                        // Lane rows first (the grid draws over them, like REAPER).
                        for (idx, track) in tracks.iter().enumerate() {
                            Lane { key: "{track.id}", track: track.clone(), pps, alt: idx % 2 == 1 }
                        }

                        // Grid hierarchy: sub-beat, beat, then measure lines on top.
                        if sub < minor {
                            for i in 0..n_at(sub) {
                                if (i as f64 * sub / minor).fract() > 1e-9 {
                                    div {
                                        key: "s{i}",
                                        style: format!(
                                            "position:absolute; top:0; bottom:0; left:{x:.1}px; \
                                             width:1px; background:{grid_sub}; pointer-events:none;",
                                            x = i as f64 * sub * pps,
                                        ),
                                    }
                                }
                            }
                        }
                        if minor < major {
                            for i in 0..n_at(minor) {
                                if (i as f64 * minor / major).fract() > 1e-9 {
                                    div {
                                        key: "b{i}",
                                        style: format!(
                                            "position:absolute; top:0; bottom:0; left:{x:.1}px; \
                                             width:1px; background:{grid_beat}; pointer-events:none;",
                                            x = i as f64 * minor * pps,
                                        ),
                                    }
                                }
                            }
                        }
                        for i in 0..n_at(major) {
                            div {
                                key: "g{i}",
                                style: format!(
                                    "position:absolute; top:0; bottom:0; left:{x:.1}px; \
                                     width:1px; background:{grid_measure}; pointer-events:none;",
                                    x = i as f64 * major * pps,
                                ),
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

/// One arrangement lane: alternating row background (`col_tr1/2_bg`), the
/// divider line, and the track's clips positioned on the timeline.
#[component]
fn Lane(track: TrackView, pps: f64, alt: bool) -> Element {
    let accent = track.hex();
    let theme = use_theme().theme;
    let ar = theme.arrange;
    let i = alt as usize;
    let row_bg = ar.row_bg[i].css();
    let divider = ar.row_divider[i].css();
    let item_edge = ar.item_edge.css();
    let item_label = ar.item_label.css();
    rsx! {
        div {
            style: format!(
                "position:relative; height:{h}px; background:{row_bg}; \
                 border-bottom:1px solid {divider}; box-sizing:border-box;",
                h = track.height,
            ),
            for (ci, clip) in track.clips.iter().enumerate() {
                {
                    let col = clip.color.clone().unwrap_or_else(|| accent.clone());
                    rsx! {
                        div {
                            key: "c{ci}",
                            title: "{clip.name}",
                            style: format!(
                                "position:absolute; top:2px; bottom:2px; left:{x}px; width:{w}px; \
                                 background:linear-gradient(180deg,{col}e6,{col}b3); \
                                 border:1px solid {item_edge}; border-radius:3px; overflow:hidden; \
                                 box-shadow:inset 0 1px 0 rgba(255,255,255,0.15); \
                                 font-size:10px; color:{item_label}; font-weight:700; \
                                 padding:1px 5px; white-space:nowrap; text-overflow:ellipsis;",
                                x = (clip.start * pps) as u32,
                                w = (clip.length * pps).max(2.0) as u32,
                            ),
                            "{clip.name}"
                        }
                    }
                }
            }
        }
    }
}
