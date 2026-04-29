//! Kbd — keyboard shortcut badge, shadcn v4 maia style.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct KbdProps {
    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: cn-kbd
#[component]
pub fn Kbd(props: KbdProps) -> Element {
    let base = "inline-flex items-center justify-center bg-muted text-muted-foreground h-5 w-fit min-w-5 gap-1 rounded-sm px-1 font-sans text-xs font-medium [&_svg:not([class*='size-'])]:size-3";

    rsx! {
        kbd {
            class: "{base} {props.class}",
            {props.children}
        }
    }
}
