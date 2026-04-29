//! Accordion — shadcn v4 maia style collapsible sections (single-open mode).

use dioxus::prelude::*;
use dioxus_primitives::accordion::{
    Accordion as PrimitiveAccordion, AccordionContent as PrimitiveAccordionContent,
    AccordionItem as PrimitiveAccordionItem, AccordionTrigger as PrimitiveAccordionTrigger,
};

// ---------------------------------------------------------------------------
// Accordion (container)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct AccordionProps {
    #[props(default)]
    pub id: Option<String>,
    #[props(default = false)]
    pub allow_multiple_open: bool,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default = true)]
    pub collapsible: bool,
    #[props(default = false)]
    pub horizontal: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Root accordion container. Provides single-open context to children.
#[component]
pub fn Accordion(props: AccordionProps) -> Element {
    rsx! {
        PrimitiveAccordion {
            id: props.id,
            allow_multiple_open: props.allow_multiple_open,
            disabled: props.disabled,
            collapsible: props.collapsible,
            horizontal: props.horizontal,
            class: crate::cn::merge_slice(&["overflow-hidden rounded-lg border", props.class.as_str()]),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// AccordionItem
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct AccordionItemProps {
    pub index: usize,
    #[props(default = false)]
    pub default_open: bool,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub on_change: Option<Callback<bool>>,
    #[props(default)]
    pub on_trigger_click: Option<Callback>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Individual collapsible section inside an `Accordion`.
#[component]
pub fn AccordionItem(props: AccordionItemProps) -> Element {
    rsx! {
        PrimitiveAccordionItem {
            index: props.index,
            default_open: props.default_open,
            disabled: props.disabled,
            on_change: move |open| {
                if let Some(callback) = &props.on_change {
                    callback.call(open);
                }
            },
            on_trigger_click: move |_| {
                if let Some(callback) = &props.on_trigger_click {
                    callback.call(());
                }
            },
            class: crate::cn::merge_slice(&["not-last:border-b data-[open=true]:bg-muted/50", props.class.as_str()]),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// AccordionTrigger
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct AccordionTriggerProps {
    #[props(default)]
    pub id: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Clickable header that toggles its parent `AccordionItem`.
#[component]
pub fn AccordionTrigger(props: AccordionTriggerProps) -> Element {
    rsx! {
        PrimitiveAccordionTrigger {
            id: props.id,
            class: crate::cn::merge(format!(
                "group flex w-full items-center justify-between gap-6 p-4 text-left text-sm font-medium hover:underline cursor-pointer {}",
                props.class
            )),
            {props.children}
            svg {
                class: "size-4 text-muted-foreground transition-transform group-aria-expanded:rotate-180",
                xmlns: "http://www.w3.org/2000/svg",
                width: "24",
                height: "24",
                view_box: "0 0 24 24",
                fill: "none",
                stroke: "currentColor",
                stroke_width: "2",
                stroke_linecap: "round",
                stroke_linejoin: "round",
                path { d: "m6 9 6 6 6-6" }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// AccordionContent
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct AccordionContentProps {
    #[props(default)]
    pub id: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Collapsible body of an `AccordionItem`. Only renders when its item is open.
#[component]
pub fn AccordionContent(props: AccordionContentProps) -> Element {
    rsx! {
        PrimitiveAccordionContent {
            id: props.id,
            class: crate::cn::merge_slice(&["px-4 text-sm", props.class.as_str()]),
            div {
                class: "pt-0 pb-4",
                {props.children}
            }
        }
    }
}
