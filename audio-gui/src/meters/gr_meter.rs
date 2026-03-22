//! Gain reduction meter — vertical bar with numeric readout.
//!
//! Fills from top down with glowing segments and recessed trough.

use crate::theme;
use crate::theme::*;
use nih_plug_dioxus::prelude::*;

/// Vertical gain reduction meter with numeric readout.
///
/// Fills from top down (gain reduction pushes the bar downward).
/// Color zones: green < 6 dB, amber 6–15 dB, red > 15 dB.
#[component]
pub fn GrMeter(
    /// Current gain reduction in dB (positive = reducing).
    gain_reduction_db: f32,
    /// Maximum GR to display.
    #[props(default = 30.0)]
    max_gr_db: f32,
    /// Widget height in pixels.
    #[props(default = 200.0)]
    height: f32,
    /// Widget width in pixels.
    #[props(default = 16.0)]
    width: f32,
) -> Element {
    let clamped = gain_reduction_db.clamp(0.0, max_gr_db);
    let fill_pct = (clamped / max_gr_db) * 100.0;
    let gr_text = format!("{:.1}", -gain_reduction_db);

    let (color, glow) = if clamped < 6.0 {
        (SIGNAL_SAFE, theme::SIGNAL_SAFE_GLOW)
    } else if clamped < 15.0 {
        (SIGNAL_WARN, theme::SIGNAL_WARN_GLOW)
    } else {
        (SIGNAL_DANGER, theme::SIGNAL_DANGER_GLOW)
    };

    rsx! {
        div {
            style: format!(
                "display:flex; flex-direction:column; align-items:center; gap:4px; \
                 min-width:36px;"
            ),
            div {
                style: format!("{LABEL}", LABEL = theme::STYLE_LABEL),
                "GR"
            }
            div {
                style: format!(
                    "width:{width}px; height:{height}px; \
                     {INSET} position:relative; overflow:hidden;",
                    INSET = theme::STYLE_INSET,
                ),
                div {
                    style: format!(
                        "position:absolute; top:0; left:0; right:0; \
                         height:{fill_pct}%; background:{color}; \
                         box-shadow:0 0 6px {glow}; \
                         transition:height 0.05s;"
                    ),
                }
            }
            div {
                style: format!(
                    "{VALUE} min-width:36px; text-align:center;",
                    VALUE = theme::STYLE_VALUE,
                ),
                "{gr_text}"
            }
        }
    }
}
