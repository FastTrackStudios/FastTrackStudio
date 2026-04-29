//! Skeleton — loading placeholder, shadcn v4 maia style.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct SkeletonProps {
    #[props(default)]
    pub class: String,
}

/// Generic skeleton block.
#[component]
pub fn Skeleton(props: SkeletonProps) -> Element {
    let base = "bg-muted rounded-xl animate-pulse";

    rsx! {
        div { class: crate::cn::merge_slice(&[base, props.class.as_str()]) }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SkeletonTextProps {
    #[props(default)]
    pub class: String,
}

/// Single-line text skeleton.
#[component]
pub fn SkeletonText(props: SkeletonTextProps) -> Element {
    let base = "bg-muted rounded-xl animate-pulse h-4";

    rsx! {
        div { class: crate::cn::merge_slice(&[base, props.class.as_str()]) }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SkeletonCircleProps {
    #[props(default)]
    pub class: String,
}

/// Circular skeleton (e.g. avatar placeholder).
#[component]
pub fn SkeletonCircle(props: SkeletonCircleProps) -> Element {
    let base = "bg-muted rounded-full animate-pulse";

    rsx! {
        div { class: crate::cn::merge_slice(&[base, props.class.as_str()]) }
    }
}
