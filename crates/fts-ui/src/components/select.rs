//! Select — shadcn v4 maia style single-value select dropdown.

use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// Context shared between Select sub-components
// ---------------------------------------------------------------------------

#[derive(Clone, Copy)]
struct SelectContext {
    value: Signal<String>,
    on_change: Option<Callback<String>>,
    open: Signal<bool>,
    placeholder: Signal<String>,
}

// ---------------------------------------------------------------------------
// Select (root)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct SelectProps {
    /// The currently selected value (two-way bound).
    pub value: Signal<String>,

    /// Called when the user picks a new value.
    #[props(default)]
    pub on_change: Option<Callback<String>>,

    /// Text shown when no value is selected.
    #[props(default = "Select...".to_string())]
    pub placeholder: String,

    /// Whether the select is disabled.
    #[props(default = false)]
    pub disabled: bool,

    /// Extra CSS classes on the outer wrapper.
    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: select root
#[component]
pub fn Select(props: SelectProps) -> Element {
    let mut open = use_signal(|| false);
    let placeholder = use_signal(|| props.placeholder.clone());

    use_context_provider(|| SelectContext {
        value: props.value,
        on_change: props.on_change,
        open,
        placeholder,
    });

    rsx! {
        div {
            class: crate::cn::merge_slice(&["relative inline-block w-full", props.class.as_str()]),

            // Click-outside overlay when open
            if *open.read() {
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| open.set(false),
                }
            }

            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// SelectTrigger
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct SelectTriggerProps {
    #[props(default = false)]
    pub disabled: bool,

    #[props(default)]
    pub class: String,

    #[props(default)]
    pub children: Element,
}

/// shadcn v4 maia: select trigger button
#[component]
pub fn SelectTrigger(props: SelectTriggerProps) -> Element {
    let mut ctx: SelectContext = use_context();
    let current = ctx.value.read().clone();
    let placeholder = ctx.placeholder.read().clone();
    let display = if current.is_empty() { placeholder } else { current };
    let is_placeholder = ctx.value.read().is_empty();

    rsx! {
        button {
            r#type: "button",
            disabled: if props.disabled { Some(true) } else { None },
            class: crate::cn::merge(format!(
                "inline-flex items-center justify-between gap-2 h-9 w-full px-3 text-sm rounded-lg border border-input bg-transparent shadow-xs hover:bg-accent hover:text-accent-foreground transition-colors cursor-pointer select-none disabled:cursor-not-allowed disabled:opacity-50 {} {}",
                if is_placeholder { "text-muted-foreground" } else { "text-foreground" },
                props.class
            )),
            onclick: move |_| {
                if !props.disabled {
                    let was_open = *ctx.open.read();
                    ctx.open.set(!was_open);
                }
            },
            span { class: "truncate", "{display}" }
            // ChevronsUpDown icon (shadcn v4 style)
            svg {
                class: "size-4 text-muted-foreground shrink-0 opacity-50",
                xmlns: "http://www.w3.org/2000/svg",
                width: "24",
                height: "24",
                view_box: "0 0 24 24",
                fill: "none",
                stroke: "currentColor",
                stroke_width: "2",
                stroke_linecap: "round",
                stroke_linejoin: "round",
                path { d: "m7 15 5 5 5-5" }
                path { d: "m7 9 5-5 5 5" }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// SelectContent
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct SelectContentProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: select dropdown content panel
#[component]
pub fn SelectContent(props: SelectContentProps) -> Element {
    let ctx: SelectContext = use_context();

    if !*ctx.open.read() {
        return rsx! {};
    }

    rsx! {
        div {
            class: crate::cn::merge(format!(
                "absolute z-50 mt-1 min-w-48 w-full rounded-lg bg-popover text-popover-foreground border border-border shadow-md p-1 overflow-hidden {}",
                props.class
            )),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// SelectItem
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct SelectItemProps {
    /// The value this item represents.
    pub value: String,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: select item
#[component]
pub fn SelectItem(props: SelectItemProps) -> Element {
    let mut ctx: SelectContext = use_context();
    let is_selected = *ctx.value.read() == props.value;
    let item_value = props.value.clone();

    rsx! {
        div {
            class: crate::cn::merge(format!(
                "relative flex cursor-pointer select-none items-center rounded-xl px-3 py-2 text-sm hover:bg-accent hover:text-accent-foreground transition-colors {}",
                props.class
            )),
            "data-state": if is_selected { "checked" } else { "unchecked" },
            onclick: move |_| {
                let val = item_value.clone();
                ctx.value.set(val.clone());
                ctx.open.set(false);
                if let Some(cb) = &ctx.on_change {
                    cb.call(val);
                }
            },
            span { class: "flex-1", {props.children} }
            // Check icon when selected
            if is_selected {
                svg {
                    class: "size-4 text-current shrink-0",
                    xmlns: "http://www.w3.org/2000/svg",
                    width: "24",
                    height: "24",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    path { d: "M20 6 9 17l-5-5" }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// SelectSeparator
// ---------------------------------------------------------------------------

/// shadcn v4 maia: select separator
#[component]
pub fn SelectSeparator() -> Element {
    rsx! {
        div { class: "bg-border/50 -mx-1 my-1 h-px" }
    }
}

// ---------------------------------------------------------------------------
// SelectLabel
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct SelectLabelProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: select group label
#[component]
pub fn SelectLabel(props: SelectLabelProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["text-muted-foreground px-3 py-2.5 text-xs", props.class.as_str()]),
            {props.children}
        }
    }
}
