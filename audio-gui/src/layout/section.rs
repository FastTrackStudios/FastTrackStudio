//! Layout components — section cards, action buttons, control groups.

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

/// A labeled group of controls with a sub-heading.
///
/// Renders a column with a tiny uppercase label and a horizontal row
/// of child controls. Used across plugin editors for grouping related
/// knobs/sliders (e.g. "Detection", "Sidechain", "Output").
#[component]
pub fn ControlGroup(label: &'static str, children: Element) -> Element {
    rsx! {
        div {
            style: "display:flex; flex-direction:column; align-items:center; gap:6px;",
            div {
                style: format!(
                    "font-size:9px; color:{TEXT_DIM}; text-transform:uppercase; \
                     letter-spacing:0.6px; font-weight:600;",
                ),
                "{label}"
            }
            div {
                style: "display:flex; gap:14px; align-items:flex-end;",
                {children}
            }
        }
    }
}

/// Vertical divider line between control groups or UI sections.
#[component]
pub fn Divider() -> Element {
    rsx! {
        div {
            style: format!(
                "width:1px; background:{}; align-self:stretch;",
                BORDER,
            ),
        }
    }
}

/// Tiny uppercase section label for annotating UI regions.
///
/// Smaller and lighter than `Section` — used inline above knob rows
/// or visualization areas.
#[component]
pub fn SectionLabel(text: &'static str) -> Element {
    rsx! {
        div {
            style: format!(
                "font-size:10px; color:{TEXT_DIM}; text-transform:uppercase; \
                 letter-spacing:0.4px;",
            ),
            "{text}"
        }
    }
}
