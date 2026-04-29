//! Dialog — shadcn v4 maia style modal overlay.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct DialogProps {
    pub open: bool,
    #[props(default)]
    pub on_close: Option<Callback<()>>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-dialog-overlay + cn-dialog-content
#[component]
pub fn Dialog(props: DialogProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    rsx! {
        // Overlay: cn-dialog-overlay
        div {
            class: "fixed inset-0 z-50 bg-black/80 animate-fade-in supports-[backdrop-filter]:backdrop-blur-xs",
            "data-state": "open",
            onclick: move |_| {
                if let Some(cb) = &props.on_close {
                    cb.call(());
                }
            },
        }

        // Content: cn-dialog-content
        div {
            class: crate::cn::merge(format!(
                "fixed z-50 grid w-full max-w-[calc(100%-2rem)] sm:max-w-md gap-6 rounded-xl bg-popover text-popover-foreground border border-border shadow-lg p-6 text-sm animate-scale-in {}",
                props.class
            )),
            style: "left: 50%; top: 50%; transform: translate(-50%, -50%);",
            role: "dialog",
            aria_modal: "true",
            onclick: move |evt: MouseEvent| {
                evt.stop_propagation();
            },
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DialogHeaderProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-dialog-header
#[component]
pub fn DialogHeader(props: DialogHeaderProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex flex-col gap-2", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DialogTitleProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-dialog-title
#[component]
pub fn DialogTitle(props: DialogTitleProps) -> Element {
    rsx! {
        h2 {
            class: crate::cn::merge_slice(&["text-base leading-none font-medium", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DialogDescriptionProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-dialog-description
#[component]
pub fn DialogDescription(props: DialogDescriptionProps) -> Element {
    rsx! {
        p {
            class: crate::cn::merge_slice(&["text-sm text-muted-foreground", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DialogFooterProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-dialog-footer
#[component]
pub fn DialogFooter(props: DialogFooterProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex flex-col-reverse gap-2 sm:flex-row sm:justify-end", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DialogCloseProps {
    pub on_click: Callback<()>,
    #[props(default)]
    pub class: String,
}

/// shadcn v4 maia: cn-dialog-close
#[component]
pub fn DialogClose(props: DialogCloseProps) -> Element {
    rsx! {
        button {
            class: crate::cn::merge(format!(
                "absolute top-4 right-4 rounded-sm opacity-70 transition-opacity hover:opacity-100 focus:outline-none focus:ring-2 focus:ring-ring {}",
                props.class
            )),
            r#type: "button",
            onclick: move |_| props.on_click.call(()),
            span { class: "size-4 text-lg leading-none", aria_hidden: "true", "\u{2715}" }
            span { class: "sr-only", "Close" }
        }
    }
}
