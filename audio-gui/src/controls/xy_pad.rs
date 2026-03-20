//! XY Pad — 2D parameter control surface.
//!
//! Ported from FastTrackStudio signal-ui, adapted for Blitz inline styles.

use crate::theme::*;
use nih_plug_dioxus::prelude::*;

/// 2D control output.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct XYValue {
    pub x: f64,
    pub y: f64,
}

/// A 2D parameter control pad.
///
/// A square area where dragging controls two normalized parameters
/// simultaneously (X and Y, both 0.0–1.0). Y-axis: bottom=0, top=1.
#[component]
pub fn XYPad(
    /// Current X value (0.0–1.0).
    #[props(default = 0.5)]
    x: f64,
    /// Current Y value (0.0–1.0, bottom=0, top=1).
    #[props(default = 0.5)]
    y: f64,
    /// Size in pixels (square).
    #[props(default = 120)]
    size: u32,
    /// Whether the pad is disabled.
    #[props(default)]
    disabled: bool,
    /// X-axis label.
    #[props(default)]
    x_label: Option<String>,
    /// Y-axis label.
    #[props(default)]
    y_label: Option<String>,
    /// Callback when value changes.
    #[props(default)]
    on_change: Option<Callback<XYValue>>,
) -> Element {
    let s = size;
    let sf = s as f64;
    let px = (x.clamp(0.0, 1.0) * sf) as u32;
    let py = ((1.0 - y.clamp(0.0, 1.0)) * sf) as u32; // Flip Y (top=1)

    let opacity = if disabled { "0.5" } else { "1.0" };
    let cursor = if disabled { "not-allowed" } else { "crosshair" };

    rsx! {
        div {
            style: format!(
                "display:inline-flex; flex-direction:column; align-items:center; gap:4px; \
                 opacity:{opacity};"
            ),

            div {
                style: format!(
                    "position:relative; border-radius:4px; border:1px solid {BORDER}; \
                     background:rgba(34,34,64,0.3); overflow:hidden; cursor:{cursor}; \
                     width:{s}px; height:{s}px;"
                ),

                // Crosshair lines
                div {
                    style: format!(
                        "position:absolute; left:{px}px; top:0; width:1px; height:100%; \
                         background:rgba(136,136,136,0.3);"
                    ),
                }
                div {
                    style: format!(
                        "position:absolute; left:0; top:{py}px; width:100%; height:1px; \
                         background:rgba(136,136,136,0.3);"
                    ),
                }

                // Dot indicator
                div {
                    style: format!(
                        "position:absolute; left:{px}px; top:{py}px; \
                         width:12px; height:12px; border-radius:6px; \
                         background:{ACCENT}; border:2px solid {BG}; \
                         transform:translate(-50%,-50%);"
                    ),
                }
            }

            // Labels
            div {
                style: format!(
                    "display:flex; justify-content:space-between; width:{s}px; \
                     font-size:10px; color:{TEXT_DIM};"
                ),
                if let Some(x_label) = &x_label {
                    span { "X: {x_label}" }
                }
                if let Some(y_label) = &y_label {
                    span { "Y: {y_label}" }
                }
            }
        }
    }
}
