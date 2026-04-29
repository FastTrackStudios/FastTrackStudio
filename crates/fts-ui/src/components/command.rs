//! Command palette — shadcn v4 maia style (Cmd+K dialog).

use dioxus::prelude::*;

// ── Context ──────────────────────────────────────────────────────────────────

#[derive(Clone, Copy)]
struct CommandContext {
    on_close: Option<Callback<()>>,
}

// ── CommandDialog ───────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CommandDialogProps {
    /// Whether the dialog is visible.
    #[props(default = false)]
    pub open: bool,

    /// Called when the user dismisses the dialog.
    #[props(default)]
    pub on_close: Option<Callback<()>>,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: command palette dialog with overlay
#[component]
pub fn CommandDialog(props: CommandDialogProps) -> Element {
    use_context_provider(|| CommandContext {
        on_close: props.on_close,
    });

    if !props.open {
        return rsx! {};
    }

    let on_close = props.on_close;

    rsx! {
        // Overlay
        div {
            class: "fixed inset-0 z-50 bg-black/80 supports-[backdrop-filter]:backdrop-blur-xs",
            onclick: move |_| {
                if let Some(cb) = &on_close {
                    cb.call(());
                }
            },
        }
        // Content
        div {
            class: crate::cn::merge(format!(
                "fixed left-1/2 top-1/2 -translate-x-1/2 -translate-y-1/2 z-50 w-full max-w-md rounded-xl bg-popover text-popover-foreground border border-border shadow-md p-0 overflow-hidden {}",
                props.class
            )),
            onclick: move |e| e.stop_propagation(),
            {props.children}
        }
    }
}

// ── CommandInput ────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CommandInputProps {
    /// Two-way bound search text.
    pub value: Signal<String>,

    #[props(default = "Search...".to_string())]
    pub placeholder: String,

    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: search input at top of command palette
#[component]
pub fn CommandInput(props: CommandInputProps) -> Element {
    let mut value = props.value;

    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex items-center border-b border-border px-3", props.class.as_str()]),
            // Search icon
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
                class: "flex-1 h-11 bg-transparent text-sm placeholder:text-muted-foreground focus:outline-none ml-2",
                r#type: "text",
                placeholder: "{props.placeholder}",
                value: "{value.read()}",
                oninput: move |e| value.set(e.value()),
            }
        }
    }
}

// ── CommandList ──────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CommandListProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: scrollable results container
#[component]
pub fn CommandList(props: CommandListProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["max-h-72 overflow-y-auto p-1", props.class.as_str()]),
            {props.children}
        }
    }
}

// ── CommandGroup ────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CommandGroupProps {
    /// Group heading text.
    pub heading: String,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: command group with heading
#[component]
pub fn CommandGroup(props: CommandGroupProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["overflow-hidden p-1", props.class.as_str()]),
            div {
                class: "text-muted-foreground px-3 py-2 text-xs font-medium",
                "{props.heading}"
            }
            {props.children}
        }
    }
}

// ── CommandItem ──────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CommandItemProps {
    #[props(default)]
    pub on_select: Option<Callback<()>>,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: single command item
#[component]
pub fn CommandItem(props: CommandItemProps) -> Element {
    let ctx: CommandContext = use_context();

    rsx! {
        div {
            class: crate::cn::merge(format!(
                "relative flex cursor-pointer items-center gap-2 rounded-xl px-3 py-2 text-sm hover:bg-muted hover:text-foreground transition-colors select-none [&_svg:not([class*='size-'])]:size-4 {}",
                props.class
            )),
            onclick: move |_| {
                if let Some(cb) = &props.on_select {
                    cb.call(());
                }
                if let Some(close) = &ctx.on_close {
                    close.call(());
                }
            },
            {props.children}
        }
    }
}

// ── CommandEmpty ─────────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CommandEmptyProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: shown when command list is empty
#[component]
pub fn CommandEmpty(props: CommandEmptyProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["py-6 text-center text-sm text-muted-foreground", props.class.as_str()]),
            {props.children}
        }
    }
}

// ── CommandSeparator ────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CommandSeparatorProps {
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: command separator
#[component]
pub fn CommandSeparator(props: CommandSeparatorProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["bg-border/50 my-1 h-px", props.class.as_str()]),
            role: "separator",
        }
    }
}

// ── CommandShortcut ─────────────────────────────────────────────────────────

#[derive(Props, Clone, PartialEq)]
pub struct CommandShortcutProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: keyboard shortcut hint in command item
#[component]
pub fn CommandShortcut(props: CommandShortcutProps) -> Element {
    rsx! {
        span {
            class: crate::cn::merge_slice(&["ml-auto text-xs tracking-widest text-muted-foreground", props.class.as_str()]),
            {props.children}
        }
    }
}
