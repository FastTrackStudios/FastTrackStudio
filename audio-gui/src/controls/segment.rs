//! Pill-style segment button for mutually exclusive selections.
//!
//! Recessed panel background with raised active segment.

use crate::theme;
use crate::theme::*;
use nih_plug_dioxus::prelude::*;

/// Pill-style segment button for enum-style selections.
///
/// Typically used in a row for mutually exclusive options.
/// The parent is responsible for tracking the selected index and calling
/// `ParamContext` to set the param value.
#[component]
pub fn SegmentButton(label: &'static str, selected: bool, on_click: EventHandler<()>) -> Element {
    let bg = if selected { ACCENT } else { SURFACE_RAISED };
    let border_color = if selected { ACCENT } else { BORDER };
    let color = if selected { "#fff" } else { TEXT_DIM };
    let shadow = if selected {
        format!(
            "{SUBTLE}, 0 0 6px {GLOW}",
            SUBTLE = theme::SHADOW_SUBTLE,
            GLOW = theme::ACCENT_GLOW,
        )
    } else {
        theme::SHADOW_INSET.to_string()
    };

    rsx! {
        div {
            style: format!(
                "padding:4px 8px; border-radius:{RADIUS}; font-size:11px; font-weight:500; \
                 cursor:pointer; border:1px solid {border_color}; background:{bg}; \
                 color:{color}; box-shadow:{shadow}; transition:{TRANS};",
                RADIUS = theme::RADIUS_BUTTON,
                TRANS = theme::TRANSITION_FAST,
            ),
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}
