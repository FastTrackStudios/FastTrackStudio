//! Divider — shadcn v4 maia separator style.

use dioxus::prelude::*;
use dioxus_primitives::separator::Separator;

/// Orientation of the divider.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum DividerOrientation {
    #[default]
    Horizontal,
    Vertical,
}

#[derive(Props, Clone, PartialEq)]
pub struct DividerProps {
    #[props(default)]
    pub orientation: DividerOrientation,
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: cn-separator
#[component]
pub fn Divider(props: DividerProps) -> Element {
    let orientation_class = match props.orientation {
        DividerOrientation::Horizontal => "h-px w-full",
        DividerOrientation::Vertical => "h-full w-px",
    };
    let horizontal = matches!(props.orientation, DividerOrientation::Horizontal);

    rsx! {
        Separator {
            horizontal,
            class: crate::cn::merge_slice(&["shrink-0 bg-border", orientation_class, props.class.as_str()]),
        }
    }
}
