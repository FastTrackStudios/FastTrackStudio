//! Label and value display components.

use crate::theme::DARK_THEME;
use dioxus::prelude::*;

/// A simple value label display.
#[component]
pub fn ValueLabel(
    /// The value to display
    value: String,
    /// Optional label/title
    #[props(default = None)]
    label: Option<String>,
    /// Font size for value
    #[props(default = 14)]
    font_size: u32,
    /// Use monospace font
    #[props(default = true)]
    monospace: bool,
    /// Text alignment
    #[props(default = TextAlign::Center)]
    align: TextAlign,
) -> Element {
    let theme = DARK_THEME;

    let font_family = if monospace { "monospace" } else { "inherit" };
    let text_align = match align {
        TextAlign::Left => "left",
        TextAlign::Center => "center",
        TextAlign::Right => "right",
    };

    rsx! {
        div {
            style: "display: flex; flex-direction: column; gap: 2px; text-align: {text_align};",

            // Label
            if let Some(lbl) = &label {
                span {
                    style: "color: {theme.text_secondary}; font-size: 11px; text-transform: uppercase;",
                    "{lbl}"
                }
            }

            // Value
            span {
                style: "color: {theme.text_primary}; font-size: {font_size}px; font-family: {font_family};",
                "{value}"
            }
        }
    }
}

/// A dB value display with special formatting.
#[component]
pub fn DbLabel(
    /// The dB value
    value_db: f32,
    /// Optional label/title
    #[props(default = None)]
    label: Option<String>,
    /// Number of decimal places
    #[props(default = 1)]
    decimals: u32,
    /// Font size for value
    #[props(default = 14)]
    font_size: u32,
    /// Show color based on value (green/yellow/red)
    #[props(default = false)]
    colored: bool,
) -> Element {
    let theme = DARK_THEME;

    let color = if colored {
        if value_db > 0.0 {
            theme.meter_red
        } else if value_db > -6.0 {
            theme.meter_yellow
        } else {
            theme.meter_green
        }
    } else {
        theme.text_primary
    };

    let formatted = match decimals {
        0 => format!("{:.0} dB", value_db),
        1 => format!("{:.1} dB", value_db),
        2 => format!("{:.2} dB", value_db),
        _ => format!("{:.1} dB", value_db),
    };

    rsx! {
        div {
            style: "display: flex; flex-direction: column; gap: 2px; text-align: center;",

            // Label
            if let Some(lbl) = &label {
                span {
                    style: "color: {theme.text_secondary}; font-size: 11px; text-transform: uppercase;",
                    "{lbl}"
                }
            }

            // Value
            span {
                style: "color: {color}; font-size: {font_size}px; font-family: monospace;",
                "{formatted}"
            }
        }
    }
}

/// Text alignment options.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum TextAlign {
    Left,
    #[default]
    Center,
    Right,
}
