//! Label — shadcn v4 maia style, standalone (no lumen-blocks).

use dioxus::prelude::*;
use dioxus_primitives::label::Label as PrimitiveLabel;

#[derive(Props, Clone, PartialEq)]
pub struct LabelProps {
    #[props(default)]
    pub html_for: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-label
#[component]
pub fn Label(props: LabelProps) -> Element {
    // shadcn v4 maia: cn-label
    let base = "text-sm leading-none font-medium select-none peer-disabled:cursor-not-allowed peer-disabled:opacity-70";
    let html_for = props.html_for.unwrap_or_default();

    rsx! {
        PrimitiveLabel {
            html_for,
            class: crate::cn::merge_slice(&[base, props.class.as_str()]),
            {props.children}
        }
    }
}
