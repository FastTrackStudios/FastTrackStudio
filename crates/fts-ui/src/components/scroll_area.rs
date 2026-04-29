//! ScrollArea — shadcn v4 maia style scroll container.
//!
//! CSS-only approach using Tailwind scrollbar utilities where available,
//! falling back to native OS scrollbars otherwise.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct ScrollAreaProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-scroll-area
#[component]
pub fn ScrollArea(props: ScrollAreaProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["relative overflow-hidden", props.class.as_str()]),
            div {
                class: "h-full w-full overflow-auto scrollbar-thin scrollbar-thumb-rounded-full scrollbar-thumb-border scrollbar-track-transparent",
                {props.children}
            }
        }
    }
}
