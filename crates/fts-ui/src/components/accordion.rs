//! Accordion — shadcn v4 maia style collapsible sections (single-open mode).

use dioxus::prelude::*;

#[derive(Clone)]
struct AccordionContext {
    open_item: Signal<Option<String>>,
}

#[derive(Clone)]
struct ItemContext {
    value: String,
}

// ---------------------------------------------------------------------------
// Accordion (container)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct AccordionProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Root accordion container. Provides single-open context to children.
#[component]
pub fn Accordion(props: AccordionProps) -> Element {
    let open_item = use_signal(|| None::<String>);

    rsx! {
        div {
            class: crate::cn::merge_slice(&["overflow-hidden rounded-lg border", props.class.as_str()]),
            {use_context_provider(|| AccordionContext { open_item });}
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// AccordionItem
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct AccordionItemProps {
    pub value: String,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Individual collapsible section inside an `Accordion`.
#[component]
pub fn AccordionItem(props: AccordionItemProps) -> Element {
    let ctx = use_context::<AccordionContext>();
    let is_open = ctx.open_item.read().as_deref() == Some(props.value.as_str());

    let bg = if is_open { " bg-muted/50" } else { "" };
    let state = if is_open { "open" } else { "closed" };

    rsx! {
        div {
            class: crate::cn::merge(format!("not-last:border-b{} {}", bg, props.class)),
            "data-state": state,
            {use_context_provider(|| ItemContext { value: props.value.clone() });}
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
    pub class: String,
    pub children: Element,
}

/// Clickable header that toggles its parent `AccordionItem`.
#[component]
pub fn AccordionTrigger(props: AccordionTriggerProps) -> Element {
    let mut ctx = use_context::<AccordionContext>();
    let item = use_context::<ItemContext>();
    let is_open = ctx.open_item.read().as_deref() == Some(item.value.as_str());

    let chevron_rotate = if is_open { " rotate-180" } else { "" };

    let value = item.value.clone();
    rsx! {
        button {
            r#type: "button",
            class: crate::cn::merge(format!(
                "flex w-full items-center justify-between gap-6 p-4 text-left text-sm font-medium hover:underline cursor-pointer {}",
                props.class
            )),
            onclick: move |_| {
                let current = ctx.open_item.read().clone();
                if current.as_deref() == Some(value.as_str()) {
                    ctx.open_item.set(None);
                } else {
                    ctx.open_item.set(Some(value.clone()));
                }
            },
            {props.children}
            // Chevron-down SVG
            svg {
                class: format!("size-4 text-muted-foreground transition-transform{}", chevron_rotate),
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
    pub class: String,
    pub children: Element,
}

/// Collapsible body of an `AccordionItem`. Only renders when its item is open.
#[component]
pub fn AccordionContent(props: AccordionContentProps) -> Element {
    let ctx = use_context::<AccordionContext>();
    let item = use_context::<ItemContext>();
    let is_open = ctx.open_item.read().as_deref() == Some(item.value.as_str());

    if !is_open {
        return rsx! {};
    }

    rsx! {
        div {
            class: crate::cn::merge_slice(&["px-4 text-sm", props.class.as_str()]),
            div {
                class: "pt-0 pb-4",
                {props.children}
            }
        }
    }
}
