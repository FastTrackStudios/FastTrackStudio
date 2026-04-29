//! Combobox — shadcn v4 maia style searchable select.

use dioxus::prelude::*;

// ── Context ──────────────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
struct ComboboxContext {
    value: Signal<String>,
    on_change: Option<Callback<String>>,
    open: Signal<bool>,
    search_query: Signal<String>,
}

// ── Combobox ────────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxProps {
    /// The currently selected value (two-way bound).
    pub value: Signal<String>,

    /// Called when the user picks a new value.
    #[props(default)]
    pub on_change: Option<Callback<String>>,

    /// Text shown when no value is selected.
    #[props(default = "Select...".to_string())]
    pub placeholder: String,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: combobox root
#[component]
pub fn Combobox(props: ComboboxProps) -> Element {
    let mut open = use_signal(|| false);
    let mut search_query = use_signal(String::new);

    use_context_provider(|| ComboboxContext {
        value: props.value,
        on_change: props.on_change,
        open,
        search_query,
    });

    rsx! {
        div {
            class: format!("relative inline-block w-full {}", props.class),

            // Click-outside overlay when open
            if *open.read() {
                div {
                    class: "fixed inset-0 z-40",
                    onclick: move |_| {
                        open.set(false);
                        search_query.set(String::new());
                    },
                }
            }

            {props.children}
        }
    }
}

// ── ComboboxTrigger ─────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxTriggerProps {
    /// Placeholder text when no value selected.
    #[props(default = "Select...".to_string())]
    pub placeholder: String,

    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: combobox trigger button
#[component]
pub fn ComboboxTrigger(props: ComboboxTriggerProps) -> Element {
    let mut ctx: ComboboxContext = use_context();
    let current = ctx.value.read().clone();
    let display = if current.is_empty() {
        props.placeholder.clone()
    } else {
        current
    };
    let is_placeholder = ctx.value.read().is_empty();

    rsx! {
        button {
            r#type: "button",
            class: format!(
                "inline-flex items-center justify-between gap-1.5 h-9 w-full px-3 text-sm rounded-lg border border-input bg-input/30 hover:bg-input/50 transition-colors cursor-pointer select-none {} {}",
                if is_placeholder { "text-muted-foreground" } else { "" },
                props.class
            ),
            onclick: move |_| {
                let was_open = *ctx.open.read();
                ctx.open.set(!was_open);
                if was_open {
                    ctx.search_query.set(String::new());
                }
            },
            span { class: "truncate", "{display}" }
            // Chevron-down icon
            svg {
                class: "size-4 text-muted-foreground shrink-0",
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

// ── ComboboxContent ─────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxContentProps {
    /// Placeholder for the search input.
    #[props(default = "Search...".to_string())]
    pub search_placeholder: String,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: combobox dropdown panel with search input
#[component]
pub fn ComboboxContent(props: ComboboxContentProps) -> Element {
    let mut ctx: ComboboxContext = use_context();

    if !*ctx.open.read() {
        return rsx! {};
    }

    rsx! {
        div {
            class: format!(
                "absolute z-50 mt-1 w-full max-h-72 overflow-hidden rounded-lg bg-popover text-popover-foreground border border-border shadow-md {}",
                props.class
            ),
            onclick: move |e| e.stop_propagation(),
            // Search input
            div {
                class: "flex items-center border-b border-border px-3",
                svg {
                    class: "size-4 opacity-50 shrink-0",
                    xmlns: "http://www.w3.org/2000/svg",
                    width: "24",
                    height: "24",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    path { d: "m21 21-4.3-4.3" }
                    circle { cx: "11", cy: "11", r: "8" }
                }
                input {
                    class: "flex-1 h-9 bg-transparent text-sm placeholder:text-muted-foreground focus:outline-none ml-2",
                    r#type: "text",
                    placeholder: "{props.search_placeholder}",
                    value: "{ctx.search_query.read()}",
                    oninput: move |e| ctx.search_query.set(e.value()),
                }
            }
            // Items container
            div {
                class: "max-h-56 overflow-y-auto p-1",
                {props.children}
            }
        }
    }
}

// ── ComboboxItem ────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxItemProps {
    /// The value this item represents.
    pub value: String,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: combobox item
#[component]
pub fn ComboboxItem(props: ComboboxItemProps) -> Element {
    let mut ctx: ComboboxContext = use_context();
    let is_selected = *ctx.value.read() == props.value;
    let item_value = props.value.clone();

    rsx! {
        div {
            class: format!(
                "relative flex cursor-pointer select-none items-center rounded-xl px-3 py-2 text-sm hover:bg-accent hover:text-accent-foreground transition-colors gap-2.5 {}",
                props.class
            ),
            onclick: move |_| {
                let val = item_value.clone();
                ctx.value.set(val.clone());
                ctx.open.set(false);
                ctx.search_query.set(String::new());
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

// ── ComboboxEmpty ───────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct ComboboxEmptyProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: shown when no combobox items match
#[component]
pub fn ComboboxEmpty(props: ComboboxEmptyProps) -> Element {
    rsx! {
        div {
            class: format!("py-6 text-center text-sm text-muted-foreground {}", props.class),
            {props.children}
        }
    }
}
