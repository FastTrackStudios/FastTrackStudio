//! Kbd — keyboard shortcut badge, shadcn v4 maia style.

use dioxus::prelude::*;
use fts_story_runtime::story;

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
            class: crate::cn::merge_slice(&[base, props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct KbdGroupProps {
    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// Groups `Kbd` keys into one cluster — a chord like `Ctrl B` or `⌘ K`.
/// shadcn v4 maia: cn-kbd-group (an `inline-flex` container; the keys keep
/// their own `Kbd` chrome).
#[component]
pub fn KbdGroup(props: KbdGroupProps) -> Element {
    let base = "inline-flex items-center gap-1";

    rsx! {
        kbd {
            class: crate::cn::merge_slice(&[base, props.class.as_str()]),
            {props.children}
        }
    }
}

#[story(category = "Kbd", name = "default")]
pub fn kbd_default() -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground flex items-center gap-2",
            Kbd { "Ctrl" }
            span { class: "text-muted-foreground", "+" }
            Kbd { "K" }
            span { class: "text-muted-foreground ml-3", "or" }
            Kbd { "Enter" }
            Kbd { "Esc" }
        }
    }
}

#[story(category = "Kbd", name = "group")]
pub fn kbd_group() -> Element {
    rsx! {
        div { class: "p-6 bg-background text-foreground flex items-center gap-3",
            KbdGroup {
                Kbd { "Ctrl" }
                Kbd { "B" }
            }
            KbdGroup {
                Kbd { "⌘" }
                Kbd { "K" }
            }
        }
    }
}
