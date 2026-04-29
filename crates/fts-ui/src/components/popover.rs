//! Popover — shadcn v4 maia style positioned floating panel.

use dioxus::prelude::*;

#[derive(Clone, Copy)]
struct PopoverContext {
    open: Signal<bool>,
    on_close: Option<Callback<()>>,
}

#[derive(Props, Clone, PartialEq)]
pub struct PopoverProps {
    #[props(default)]
    pub open: bool,
    #[props(default)]
    pub on_close: Option<Callback<()>>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-popover
#[component]
pub fn Popover(props: PopoverProps) -> Element {
    let open = use_signal(|| props.open);
    use_context_provider(|| PopoverContext {
        open,
        on_close: props.on_close,
    });

    rsx! {
        div {
            class: format!("relative inline-block {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct PopoverTriggerProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-popover-trigger
#[component]
pub fn PopoverTrigger(props: PopoverTriggerProps) -> Element {
    let mut ctx: PopoverContext = use_context();

    rsx! {
        div {
            class: format!("cursor-pointer {}", props.class),
            onclick: move |_| {
                let current = *ctx.open.read();
                ctx.open.set(!current);
            },
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct PopoverContentProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-popover-content
#[component]
pub fn PopoverContent(props: PopoverContentProps) -> Element {
    let mut ctx: PopoverContext = use_context();
    let is_open = *ctx.open.read();

    if !is_open {
        return rsx! {};
    }

    rsx! {
        // Invisible backdrop to capture outside clicks
        div {
            class: "fixed inset-0 z-40",
            onclick: move |_| {
                ctx.open.set(false);
                if let Some(cb) = &ctx.on_close {
                    cb.call(());
                }
            },
        }

        // Floating content panel
        div {
            class: format!(
                "bg-popover text-popover-foreground border border-border flex flex-col gap-4 rounded-lg p-4 text-sm shadow-md absolute z-50 mt-2 {}",
                props.class
            ),
            onclick: move |evt: MouseEvent| {
                evt.stop_propagation();
            },
            {props.children}
        }
    }
}
