//! Header bar and status bar for plugin windows.
//!
//! Warm panel with subtle depth and transport indicators.

use crate::theme;
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
    let play_bg = if is_playing { SIGNAL_SAFE } else { TOGGLE_OFF };
    let play_text = if is_playing { "PLAY" } else { "STOP" };
    let play_glow = if is_playing {
        format!("box-shadow:0 0 6px {GLOW};", GLOW = theme::SIGNAL_SAFE_GLOW)
    } else {
        String::new()
    };

    rsx! {
        div {
            style: format!(
                "display:flex; justify-content:space-between; align-items:center; \
                 margin-bottom:{SECTION}; padding-bottom:8px; \
                 border-bottom:1px solid {BORDER};",
                SECTION = theme::SPACING_SECTION,
                BORDER = theme::BORDER,
            ),
            div {
                style: format!(
                    "font-size:{FSIZE}; font-weight:700; color:{TEXT}; \
                     letter-spacing:0.3px;",
                    FSIZE = theme::FONT_SIZE_TITLE,
                    TEXT = theme::TEXT_BRIGHT,
                ),
                "{title}"
            }
            div {
                style: format!(
                    "display:flex; gap:12px; align-items:center; \
                     {VALUE}",
                    VALUE = theme::STYLE_VALUE,
                ),
                span {
                    style: format!("color:{DIM};", DIM = theme::TEXT_DIM),
                    "{tempo:.0} BPM"
                }
                span {
                    style: format!("color:{DIM};", DIM = theme::TEXT_DIM),
                    "{time_sig_num}/{time_sig_den}"
                }
                span {
                    style: format!(
                        "padding:2px 8px; border-radius:{RADIUS}; font-size:11px; \
                         background:{play_bg}; color:#fff; {play_glow}",
                        RADIUS = theme::RADIUS_BUTTON,
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
                 {VALUE} color:{DIM};",
                BORDER = theme::BORDER,
                VALUE = theme::STYLE_VALUE,
                DIM = theme::TEXT_DIM,
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
