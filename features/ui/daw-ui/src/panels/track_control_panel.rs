//! TrackControlPanel (TCP) — the left sidebar of the arrange view.
//!
//! A vertical stack of per-track control rows (Reaper-TCP style): record-arm,
//! a colour stripe, the (folder-indented) name, mute/solo, a pan knob, and a
//! volume slider. Each row is `track.height` px tall so the rows line up with
//! the arrange lanes on the right.

use crate::panels::model::TrackView;
use crate::prelude::*;
use crate::theming::{Color, ThemeState, use_theme};
use crate::widgets::hslider::{HSlider, SliderVariant};
use crate::widgets::knob::{Knob, KnobVariant};
use crate::widgets::mixer::{MuteButton, SoloButton};

/// Per-track indent (px) applied per folder-depth level.
const INDENT: u32 = 12;

/// The track-control sidebar. `width` is the sidebar width in px; rows scroll
/// vertically. Intended to sit left of [`super::ArrangeView`]'s timeline.
#[component]
pub fn TrackControlPanel(
    tracks: Vec<TrackView>,
    #[props(default = 260)] width: u32,
    /// Scroll its own rows vertically. Set `false` when embedded in
    /// [`super::ArrangeView`], where the arrange body owns the shared scroll
    /// so the rows stay aligned with the timeline lanes.
    #[props(default = true)]
    scroll: bool,
) -> Element {
    let overflow = if scroll { "auto" } else { "visible" };
    let surface = use_theme().theme.panel().surface.css();
    rsx! {
        div {
            style: format!(
                "flex:0 0 {width}px; width:{width}px; display:flex; flex-direction:column; \
                 background:{surface}; overflow-y:{overflow}; user-select:none;"
            ),
            for track in tracks.iter() {
                TcpRow { key: "{track.id}", track: track.clone() }
            }
        }
    }
}

/// One TCP control row for a track.
#[component]
fn TcpRow(track: TrackView) -> Element {
    let theme = use_theme().theme;
    let tk = theme.tokens;
    let accent = track.hex();
    let st = ThemeState::new().track(track.color.as_deref().and_then(Color::hex));
    let indent = track.depth * INDENT;
    let mut record_arm = track.record_arm;
    let arm_on = record_arm();
    let arm_bg = if arm_on {
        tk.rec.css()
    } else {
        tk.border.css()
    };
    let arm_fill = if arm_on {
        tk.rec.css()
    } else {
        "transparent".to_string()
    };
    let name_fg = tk.text.css();
    let name_weight = if track.is_folder { 800 } else { 600 };
    let border = tk.border.css();
    let row_bg = if track.is_folder {
        theme.track_tint(tk.surface_raised, &st).css()
    } else {
        tk.surface_raised.css()
    };

    rsx! {
        div {
            style: format!(
                "flex:0 0 {h}px; height:{h}px; display:flex; align-items:center; gap:6px; \
                 padding:0 8px 0 0; background:{row_bg}; border-bottom:1px solid {border};",
                h = track.height,
            ),

            // Colour stripe (left edge), indented by folder depth.
            div { style: format!("flex:0 0 {indent}px; height:100%;") }
            div { style: format!("flex:0 0 4px; height:100%; background:{accent};") }

            // Record-arm.
            button {
                r#type: "button",
                title: "Record arm",
                style: format!(
                    "flex:0 0 auto; width:18px; height:18px; border-radius:50%; \
                     border:1px solid {arm_bg}; background:{arm_fill}; cursor:pointer; padding:0;"
                ),
                onclick: move |_| { let n = !record_arm(); record_arm.set(n); },
            }

            // Name + (optional) folder marker.
            div {
                style: "flex:1 1 0; min-width:0; display:flex; flex-direction:column; gap:2px;",
                span {
                    style: format!(
                        "font-size:12px; font-weight:{name_weight}; color:{name_fg}; \
                         white-space:nowrap; overflow:hidden; text-overflow:ellipsis;"
                    ),
                    if track.is_folder { span { style: "opacity:0.7; margin-right:4px;", "▼" } }
                    "{track.name}"
                }
                // Compact volume slider under the name.
                HSlider {
                    value: track.fader,
                    variant: SliderVariant::Default,
                    width: 130,
                    height: 12,
                }
            }

            // Mute / Solo (fixed-width cluster).
            div {
                style: "flex:0 0 52px; display:flex; gap:4px;",
                MuteButton { active: track.mute }
                SoloButton { active: track.solo }
            }

            // Pan knob.
            div {
                style: "flex:0 0 auto; display:flex; align-items:center;",
                Knob {
                    value: track.pan,
                    min: 0.0,
                    max: 1.0,
                    default: 0.5,
                    variant: KnobVariant::ArcBipolar,
                    size: 28,
                }
            }
        }
    }
}
