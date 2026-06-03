//! ArrangeView — the main timeline, Reaper-style.
//!
//! Composes a [`TrackControlPanel`] sidebar on the left with a scrollable
//! timeline on the right: a time ruler across the top and one lane per track
//! (height `track.height`, aligned with the TCP rows) carrying its clips.

use crate::panels::model::TrackView;
use crate::panels::track_control_panel::TrackControlPanel;
use crate::prelude::*;
use crate::theming::use_theme;

/// The arrange view. `pps` = pixels per second (horizontal zoom); `tcp_width`
/// is the control-sidebar width; `seconds` is the visible timeline length.
#[component]
pub fn ArrangeView(
    tracks: Vec<TrackView>,
    #[props(default = 12.0)] pps: f64,
    #[props(default = 260)] tcp_width: u32,
    #[props(default = 120.0)] seconds: f64,
) -> Element {
    let ruler_h = 26u32;
    let content_w = (seconds * pps).max(600.0) as u32;
    // One tick per 5 seconds.
    let tick_step = 5.0f64;
    let tick_count = (seconds / tick_step).ceil() as i64;

    let tk = use_theme().theme.tokens;
    let timeline_bg = tk.surface_sunken.css();
    let ruler_bg = tk.surface_raised.css();
    let border = tk.border.css();
    let grid_line = tk.border.css();
    let tick_fg = tk.text_faint.css();

    rsx! {
        div {
            style: format!(
                "display:flex; flex-direction:column; height:100%; min-height:0; background:{timeline_bg};"
            ),

            // ── Ruler row: spacer over the TCP, ticks over the timeline ──
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
                        for i in 0..tick_count {
                            div {
                                key: "{i}",
                                style: format!(
                                    "position:absolute; top:0; bottom:0; left:{x}px; \
                                     border-left:1px solid {border}; padding-left:4px; \
                                     font-size:9px; color:{tick_fg}; font-variant-numeric:tabular-nums;",
                                    x = (i as f64 * tick_step * pps) as u32,
                                ),
                                "{(i as f64 * tick_step) as u32}s"
                            }
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
                    style: format!("flex:1 1 0; min-width:0; overflow-x:auto; background:{timeline_bg};"),
                    div {
                        style: format!("position:relative; width:{content_w}px;"),
                        // Vertical grid lines across the whole lane stack.
                        for i in 0..tick_count {
                            div {
                                key: "g{i}",
                                style: format!(
                                    "position:absolute; top:0; bottom:0; left:{x}px; \
                                     width:1px; background:{grid_line}; pointer-events:none;",
                                    x = (i as f64 * tick_step * pps) as u32,
                                ),
                            }
                        }
                        for track in tracks.iter() {
                            Lane { key: "{track.id}", track: track.clone(), pps }
                        }
                    }
                }
            }
        }
    }
}

/// One arrangement lane: the track's clips positioned on the timeline.
#[component]
fn Lane(track: TrackView, pps: f64) -> Element {
    let accent = track.hex();
    let tk = use_theme().theme.tokens;
    let lane_border = tk.border.css();
    let clip_border = tk.surface_sunken.css();
    rsx! {
        div {
            style: format!(
                "position:relative; height:{h}px; border-bottom:1px solid {lane_border}; \
                 box-sizing:border-box;",
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
                                "position:absolute; top:3px; bottom:3px; left:{x}px; width:{w}px; \
                                 background:linear-gradient(180deg,{col}cc,{col}88); \
                                 border:1px solid {clip_border}; border-radius:3px; overflow:hidden; \
                                 box-shadow:inset 0 1px 0 rgba(255,255,255,0.15); \
                                 font-size:10px; color:#0c0c0f; font-weight:700; padding:2px 5px; \
                                 white-space:nowrap; text-overflow:ellipsis;",
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
