//! VStack and HStack — flex containers with configurable gap.

use dioxus::prelude::*;

/// Vertical flex container (column direction).
#[derive(Props, Clone, PartialEq)]
pub struct VStackProps {
    /// Tailwind gap class (e.g. "2", "4"). Defaults to "2".
    #[props(default = "2".to_string())]
    pub gap: String,

    /// Extra CSS classes.
    #[props(default)]
    pub class: String,

    pub children: Element,
}

#[component]
pub fn VStack(props: VStackProps) -> Element {
    rsx! {
        div {
            class: format!("flex flex-col gap-{} {}", props.gap, props.class),
            {props.children}
        }
    }
}

/// Horizontal flex container (row direction).
#[derive(Props, Clone, PartialEq)]
pub struct HStackProps {
    /// Tailwind gap class (e.g. "2", "4"). Defaults to "2".
    #[props(default = "2".to_string())]
    pub gap: String,

    /// Extra CSS classes.
    #[props(default)]
    pub class: String,

    pub children: Element,
}

#[component]
pub fn HStack(props: HStackProps) -> Element {
    rsx! {
        div {
            class: format!("flex flex-row items-center gap-{} {}", props.gap, props.class),
            {props.children}
        }
    }
}
