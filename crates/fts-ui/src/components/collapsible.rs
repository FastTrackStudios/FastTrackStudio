//! Collapsible — simple open/close container with context-driven state.

use dioxus::prelude::*;

#[derive(Clone)]
struct CollapsibleContext {
    open: Signal<bool>,
}

// ---------------------------------------------------------------------------
// Collapsible (root)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CollapsibleProps {
    #[props(default)]
    pub default_open: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Root container that manages open/closed state via context.
#[component]
pub fn Collapsible(props: CollapsibleProps) -> Element {
    let open = use_signal(|| props.default_open);

    rsx! {
        div {
            class: props.class,
            {use_context_provider(|| CollapsibleContext { open });}
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// CollapsibleTrigger
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CollapsibleTriggerProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Button that toggles the collapsible open/closed.
#[component]
pub fn CollapsibleTrigger(props: CollapsibleTriggerProps) -> Element {
    let mut ctx = use_context::<CollapsibleContext>();

    rsx! {
        button {
            r#type: "button",
            class: crate::cn::merge_slice(&["cursor-pointer", props.class.as_str()]),
            onclick: move |_| {
                let current = *ctx.open.read();
                ctx.open.set(!current);
            },
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// CollapsibleContent
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CollapsibleContentProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Body content that only renders when the collapsible is open.
#[component]
pub fn CollapsibleContent(props: CollapsibleContentProps) -> Element {
    let ctx = use_context::<CollapsibleContext>();

    if !*ctx.open.read() {
        return rsx! {};
    }

    rsx! {
        div {
            class: props.class,
            {props.children}
        }
    }
}
