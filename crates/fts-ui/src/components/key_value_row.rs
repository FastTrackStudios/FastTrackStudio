//! Key-value row — label/value pair for detail breakdowns.
//!
//! Used in info popovers, latency panels, and settings displays.

use dioxus::prelude::*;

/// A horizontal row with a label on the left and a value on the right.
///
/// ```rust,ignore
/// KeyValueRow { label: "Input", value: "3.2ms" }
/// ```
#[derive(Props, Clone, PartialEq)]
pub struct KeyValueRowProps {
    /// Left-aligned label.
    pub label: String,

    /// Right-aligned value.
    pub value: String,

    /// Whether to render the value in monospace (for numeric data).
    #[props(default = true)]
    pub mono: bool,

    /// Whether to render with bold/semibold weight (for totals/summaries).
    #[props(default = false)]
    pub bold: bool,

    /// Extra CSS classes on the row container.
    #[props(default)]
    pub class: String,
}

#[component]
pub fn KeyValueRow(props: KeyValueRowProps) -> Element {
    let weight = if props.bold { "font-semibold" } else { "" };
    let font = if props.mono {
        "font-mono tabular-nums"
    } else {
        ""
    };

    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex justify-between items-center", weight, props.class.as_str()]),

            span { class: "text-muted-foreground", "{props.label}" }
            span { class: "{font}", "{props.value}" }
        }
    }
}
