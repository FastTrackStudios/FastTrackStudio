//! Header bar and status bar for plugin windows.

use crate::theme::*;
use nih_plug_dioxus::prelude::*;

/// Header bar with plugin title + transport info.
#[component]
pub fn Header(
    title: &'static str,
    #[props(default = 120.0)] tempo: f32,
    #[props(default = 4)] time_sig_num: i32,
    #[props(default = 4)] time_sig_den: i32,
    #[props(default = false)] is_playing: bool,
) -> Element {
    let play_bg = if is_playing { SIGNAL_SAFE } else { "#555" };
    let play_text = if is_playing { "PLAY" } else { "STOP" };

    rsx! {
        div {
            style: format!(
                "display:flex; justify-content:space-between; align-items:center; \
                 margin-bottom:10px; padding-bottom:8px; border-bottom:1px solid {BORDER};"
            ),
            div {
                style: "font-size:18px; font-weight:700;",
                "{title}"
            }
            div {
                style: format!(
                    "display:flex; gap:12px; align-items:center; font-size:12px; \
                     color:{TEXT_DIM};"
                ),
                span { "{tempo:.0} BPM" }
                span { "{time_sig_num}/{time_sig_den}" }
                span {
                    style: format!(
                        "padding:2px 8px; border-radius:4px; font-size:11px; \
                         background:{play_bg}; color:#fff;"
                    ),
                    "{play_text}"
                }
            }
        }
    }
}

/// Status bar showing beat position and optional window size.
#[component]
pub fn StatusBar(beat_position: f32) -> Element {
    rsx! {
        div {
            style: format!(
                "padding-top:6px; margin-top:8px; border-top:1px solid {BORDER}; \
                 font-size:11px; color:{TEXT_DIM};"
            ),
            {
                let info = if let Some(state) = try_use_context::<std::sync::Arc<DioxusState>>() {
                    let (w, h) = state.size();
                    format!("Beat: {beat_position:.2} | {w}x{h}")
                } else {
                    format!("Beat: {beat_position:.2}")
                };
                rsx! { "{info}" }
            }
        }
    }
}
