//! Rotary knob component.

use crate::theme::DARK_THEME;
use dioxus::prelude::*;

/// A rotary knob control.
#[component]
pub fn Knob(
    /// Current value (0.0 to 1.0 normalized)
    value: f32,
    /// Label text
    #[props(default = None)]
    label: Option<String>,
    /// Formatted value string to display
    #[props(default = None)]
    value_text: Option<String>,
    /// Size in pixels
    #[props(default = 64)]
    size: u32,
    /// Callback when value changes
    #[props(default = None)]
    on_change: Option<EventHandler<f32>>,
    /// Whether the knob is disabled
    #[props(default = false)]
    disabled: bool,
) -> Element {
    let theme = DARK_THEME;

    // Convert normalized value to rotation angle (-135 to +135 degrees)
    let rotation = -135.0 + (value.clamp(0.0, 1.0) * 270.0);

    let cursor = if disabled { "not-allowed" } else { "pointer" };
    let opacity = if disabled { "0.5" } else { "1.0" };

    rsx! {
        div {
            style: "display: flex; flex-direction: column; align-items: center; gap: 4px; opacity: {opacity};",

            // Label above knob
            if let Some(lbl) = &label {
                span {
                    style: "color: {theme.text_secondary}; font-size: 11px; text-transform: uppercase;",
                    "{lbl}"
                }
            }

            // Knob container
            div {
                style: "width: {size}px; height: {size}px; position: relative; cursor: {cursor};",

                // Outer ring (track)
                div {
                    style: "position: absolute; inset: 0; border: 2px solid {theme.bg_tertiary}; border-radius: 50%;",
                }

                // Active arc (would need SVG for proper arc, using solid color for now)
                div {
                    style: "position: absolute; inset: 4px; background: {theme.bg_secondary}; border-radius: 50%; display: flex; align-items: center; justify-content: center;",

                    // Indicator line
                    div {
                        style: "width: 2px; height: 40%; background: {theme.accent_primary}; border-radius: 1px; transform-origin: bottom center; transform: rotate({rotation}deg); position: absolute; top: 10%;",
                    }
                }

                // Center dot
                div {
                    style: "position: absolute; inset: 35%; background: {theme.bg_tertiary}; border-radius: 50%;",
                }
            }

            // Value display below knob
            if let Some(val) = &value_text {
                span {
                    style: "color: {theme.text_primary}; font-size: 12px; font-family: monospace;",
                    "{val}"
                }
            }
        }
    }
}
