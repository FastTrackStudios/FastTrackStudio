//! Layout components — section cards, action buttons.

use crate::theme::*;
use nih_plug_dioxus::prelude::*;

/// Card-style section wrapper with uppercase title.
#[component]
pub fn Section(title: &'static str, children: Element) -> Element {
    rsx! {
        div {
            style: format!(
                "background:{CARD_BG}; border-radius:6px; padding:10px 12px; \
                 margin-bottom:8px;"
            ),
            div {
                style: format!(
                    "font-size:11px; font-weight:600; text-transform:uppercase; \
                     letter-spacing:0.5px; color:{TEXT_DIM}; margin-bottom:6px;"
                ),
                "{title}"
            }
            {children}
        }
    }
}

/// Full-width action button.
#[component]
pub fn ActionButton(label: &'static str, on_click: EventHandler<()>) -> Element {
    rsx! {
        div {
            style: format!(
                "padding:8px 0; cursor:pointer; text-align:center; \
                 background:{ACCENT}; color:#fff; border-radius:6px; \
                 font-size:13px; font-weight:600; margin-top:4px;"
            ),
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}
