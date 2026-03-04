//! Divider — horizontal or vertical separator line.

use dioxus::prelude::*;

/// Orientation of the divider.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum DividerOrientation {
    #[default]
    Horizontal,
    Vertical,
}

#[derive(Props, Clone, PartialEq)]
pub struct DividerProps {
    /// Orientation.
    #[props(default)]
    pub orientation: DividerOrientation,

    /// Extra CSS classes.
    #[props(default)]
    pub class: String,
}

#[component]
pub fn Divider(props: DividerProps) -> Element {
    let orientation_class = match props.orientation {
        DividerOrientation::Horizontal => "w-full border-t border-border",
        DividerOrientation::Vertical => "h-full border-l border-border",
    };

    rsx! {
        div {
            class: format!("shrink-0 {orientation_class} {}", props.class),
            role: "separator",
            aria_orientation: match props.orientation {
                DividerOrientation::Horizontal => "horizontal",
                DividerOrientation::Vertical => "vertical",
            },
        }
    }
}
