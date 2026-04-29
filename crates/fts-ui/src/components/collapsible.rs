//! Collapsible — accessible primitive-backed open/close container.

use dioxus::prelude::*;
use dioxus_primitives::collapsible::{
    Collapsible as PrimitiveCollapsible, CollapsibleContent as PrimitiveCollapsibleContent,
    CollapsibleTrigger as PrimitiveCollapsibleTrigger,
};

// ---------------------------------------------------------------------------
// Collapsible (root)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct CollapsibleProps {
    #[props(default)]
    pub default_open: bool,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default = false)]
    pub keep_mounted: bool,
    #[props(default)]
    pub open: Option<bool>,
    #[props(default)]
    pub on_open_change: Option<Callback<bool>>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Root container that manages open/closed state via the Dioxus primitive.
#[component]
pub fn Collapsible(props: CollapsibleProps) -> Element {
    rsx! {
        PrimitiveCollapsible {
            default_open: props.default_open,
            disabled: props.disabled,
            keep_mounted: props.keep_mounted,
            open: props.open,
            on_open_change: move |open| {
                if let Some(callback) = &props.on_open_change {
                    callback.call(open);
                }
            },
            class: props.class,
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
    rsx! {
        PrimitiveCollapsibleTrigger {
            class: crate::cn::merge_slice(&["cursor-pointer", props.class.as_str()]),
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
    pub id: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Body content that only renders when the collapsible is open.
#[component]
pub fn CollapsibleContent(props: CollapsibleContentProps) -> Element {
    rsx! {
        PrimitiveCollapsibleContent {
            id: props.id,
            class: props.class,
            {props.children}
        }
    }
}
