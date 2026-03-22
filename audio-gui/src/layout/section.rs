//! Layout components — section cards, action buttons, control groups.
//!
//! Raised panels with depth shadows and consistent spacing tokens.

use crate::theme;
use nih_plug_dioxus::prelude::*;

/// Card-style section wrapper with uppercase title.
///
/// Raised panel with subtle top-edge highlight and drop shadow.
#[component]
pub fn Section(title: &'static str, children: Element) -> Element {
    rsx! {
        div {
            style: format!(
                "{CARD} padding:{PAD}; margin-bottom:8px;",
                CARD = theme::STYLE_CARD,
                PAD = theme::SPACING_CARD,
            ),
            div {
                style: format!(
                    "{LABEL} margin-bottom:6px;",
                    LABEL = theme::STYLE_LABEL,
                ),
                "{title}"
            }
            {children}
        }
    }
}

/// Full-width action button.
///
/// Raised with accent background and subtle glow.
#[component]
pub fn ActionButton(label: &'static str, on_click: EventHandler<()>) -> Element {
    rsx! {
        div {
            style: format!(
                "padding:8px 0; cursor:pointer; text-align:center; \
                 background:{ACCENT}; color:#fff; border-radius:{RADIUS}; \
                 font-size:13px; font-weight:600; margin-top:4px; \
                 box-shadow:{SHADOW}, 0 0 8px {GLOW}; \
                 transition:{TRANS};",
                ACCENT = theme::ACCENT,
                RADIUS = theme::RADIUS_CARD,
                SHADOW = theme::SHADOW_SUBTLE,
                GLOW = theme::ACCENT_GLOW,
                TRANS = theme::TRANSITION_FAST,
            ),
            onclick: move |_| on_click.call(()),
            "{label}"
        }
    }
}

/// A labeled group of controls with a sub-heading.
///
/// Renders a column with a tiny uppercase label and a horizontal row
/// of child controls.
#[component]
pub fn ControlGroup(label: &'static str, children: Element) -> Element {
    rsx! {
        div {
            style: "display:flex; flex-direction:column; align-items:center; gap:6px;",
            div {
                style: format!("{LABEL}", LABEL = theme::STYLE_LABEL),
                "{label}"
            }
            div {
                style: format!(
                    "display:flex; gap:{GAP}; align-items:flex-end;",
                    GAP = theme::SPACING_CONTROL,
                ),
                {children}
            }
        }
    }
}

/// Vertical divider line between control groups or UI sections.
///
/// Subtle border with slight depth.
#[component]
pub fn Divider() -> Element {
    rsx! {
        div {
            style: format!(
                "width:1px; background:{BORDER}; align-self:stretch; \
                 box-shadow:1px 0 0 rgba(255,255,255,0.03);",
                BORDER = theme::BORDER,
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
            style: format!("{LABEL}", LABEL = theme::STYLE_LABEL),
            "{text}"
        }
    }
}
