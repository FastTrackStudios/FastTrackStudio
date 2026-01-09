//! Slider and fader components.

use crate::theme::DARK_THEME;
use dioxus::prelude::*;

/// A horizontal slider control.
#[component]
pub fn Slider(
    /// Current value (0.0 to 1.0 normalized)
    value: f32,
    /// Label text
    #[props(default = None)]
    label: Option<String>,
    /// Formatted value string
    #[props(default = None)]
    value_text: Option<String>,
    /// Width in pixels
    #[props(default = 120)]
    width: u32,
    /// Height of the track
    #[props(default = 4)]
    track_height: u32,
    /// Callback when value changes
    #[props(default = None)]
    on_change: Option<EventHandler<f32>>,
    /// Whether disabled
    #[props(default = false)]
    disabled: bool,
) -> Element {
    let theme = DARK_THEME;

    let pct = (value.clamp(0.0, 1.0) * 100.0) as u32;
    let cursor = if disabled { "not-allowed" } else { "pointer" };
    let opacity = if disabled { "0.5" } else { "1.0" };

    rsx! {
        div {
            style: "display: flex; flex-direction: column; gap: 4px; opacity: {opacity};",

            // Label
            if let Some(lbl) = &label {
                span {
                    style: "color: {theme.text_secondary}; font-size: 11px; text-transform: uppercase;",
                    "{lbl}"
                }
            }

            // Slider track
            div {
                style: "width: {width}px; height: {track_height}px; background: {theme.bg_tertiary}; border-radius: {track_height / 2}px; position: relative; cursor: {cursor};",

                // Filled portion
                div {
                    style: "position: absolute; left: 0; top: 0; bottom: 0; width: {pct}%; background: {theme.accent_primary}; border-radius: {track_height / 2}px;",
                }

                // Thumb
                div {
                    style: "position: absolute; top: 50%; left: {pct}%; transform: translate(-50%, -50%); width: 12px; height: 12px; background: {theme.text_primary}; border-radius: 50%; box-shadow: 0 1px 3px rgba(0,0,0,0.3);",
                }
            }

            // Value text
            if let Some(val) = &value_text {
                span {
                    style: "color: {theme.text_primary}; font-size: 11px; font-family: monospace;",
                    "{val}"
                }
            }
        }
    }
}

/// A vertical fader control (like a mixing console fader).
#[component]
pub fn Fader(
    /// Current value (0.0 to 1.0 normalized)
    value: f32,
    /// Label text
    #[props(default = None)]
    label: Option<String>,
    /// Formatted value string
    #[props(default = None)]
    value_text: Option<String>,
    /// Height in pixels
    #[props(default = 150)]
    height: u32,
    /// Width of the track
    #[props(default = 8)]
    track_width: u32,
    /// Callback when value changes
    #[props(default = None)]
    on_change: Option<EventHandler<f32>>,
    /// Whether disabled
    #[props(default = false)]
    disabled: bool,
) -> Element {
    let theme = DARK_THEME;

    let pct = (value.clamp(0.0, 1.0) * 100.0) as u32;
    let cursor = if disabled { "not-allowed" } else { "ns-resize" };
    let opacity = if disabled { "0.5" } else { "1.0" };

    rsx! {
        div {
            style: "display: flex; flex-direction: column; align-items: center; gap: 4px; opacity: {opacity};",

            // Label
            if let Some(lbl) = &label {
                span {
                    style: "color: {theme.text_secondary}; font-size: 11px; text-transform: uppercase;",
                    "{lbl}"
                }
            }

            // Fader track
            div {
                style: "width: {track_width}px; height: {height}px; background: {theme.bg_tertiary}; border-radius: {track_width / 2}px; position: relative; cursor: {cursor};",

                // Filled portion (from bottom)
                div {
                    style: "position: absolute; left: 0; right: 0; bottom: 0; height: {pct}%; background: {theme.accent_primary}; border-radius: {track_width / 2}px;",
                }

                // Thumb/cap
                div {
                    style: "position: absolute; left: 50%; bottom: {pct}%; transform: translate(-50%, 50%); width: 24px; height: 8px; background: {theme.text_primary}; border-radius: 2px; box-shadow: 0 1px 3px rgba(0,0,0,0.3);",
                }
            }

            // Value text
            if let Some(val) = &value_text {
                span {
                    style: "color: {theme.text_primary}; font-size: 11px; font-family: monospace;",
                    "{val}"
                }
            }
        }
    }
}
