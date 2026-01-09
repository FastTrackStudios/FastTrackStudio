//! Toggle switch component.

use crate::theme::DARK_THEME;
use dioxus::prelude::*;

/// A toggle switch control.
#[component]
pub fn Toggle(
    /// Current state
    value: bool,
    /// Label text
    #[props(default = None)]
    label: Option<String>,
    /// Callback when toggled
    #[props(default = None)]
    on_change: Option<EventHandler<bool>>,
    /// Whether disabled
    #[props(default = false)]
    disabled: bool,
    /// Size variant
    #[props(default = ToggleSize::Medium)]
    size: ToggleSize,
) -> Element {
    let theme = DARK_THEME;

    let (width, height, thumb_size) = match size {
        ToggleSize::Small => (32, 18, 14),
        ToggleSize::Medium => (40, 22, 18),
        ToggleSize::Large => (48, 26, 22),
    };

    let bg_color = if value {
        theme.accent_primary
    } else {
        theme.bg_tertiary
    };
    let thumb_offset = if value { width - thumb_size - 2 } else { 2 };
    let cursor = if disabled { "not-allowed" } else { "pointer" };
    let opacity = if disabled { "0.5" } else { "1.0" };

    rsx! {
        div {
            style: "display: flex; align-items: center; gap: 8px; opacity: {opacity};",

            // Label (if before)
            if let Some(lbl) = &label {
                span {
                    style: "color: {theme.text_secondary}; font-size: 12px;",
                    "{lbl}"
                }
            }

            // Toggle track
            div {
                style: "width: {width}px; height: {height}px; background: {bg_color}; border-radius: {height / 2}px; position: relative; cursor: {cursor}; transition: background 150ms;",
                onclick: move |_| {
                    if !disabled {
                        if let Some(handler) = &on_change {
                            handler.call(!value);
                        }
                    }
                },

                // Thumb
                div {
                    style: "position: absolute; top: 2px; left: {thumb_offset}px; width: {thumb_size}px; height: {thumb_size}px; background: {theme.text_primary}; border-radius: 50%; transition: left 150ms; box-shadow: 0 1px 3px rgba(0,0,0,0.2);",
                }
            }
        }
    }
}

/// Size variants for toggle.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum ToggleSize {
    Small,
    #[default]
    Medium,
    Large,
}
