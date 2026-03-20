//! Pill-style segment button for mutually exclusive selections.

use crate::theme::*;
use nih_plug_dioxus::prelude::*;

/// Pill-style segment button for enum-style selections.
///
/// Typically used in a row for mutually exclusive options.
/// The parent is responsible for tracking the selected index and calling
/// `ParamContext` to set the param value.
#[component]
pub fn SegmentButton(label: &'static str, selected: bool, on_click: EventHandler<()>) -> Element {
    let bg = if selected { ACCENT } else { "transparent" };
    let border = if selected { ACCENT } else { BORDER };
    let color = if selected { "#fff" } else { TEXT_DIM };

    rsx! {
        div {
            style: format!(
                "padding:4px 8px; border-radius:4px; font-size:11px; font-weight:500; \
                 cursor:pointer; border:1px solid {border}; background:{bg}; color:{color};"
            ),
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}
