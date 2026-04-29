//! AlertDialog — shadcn v4 maia style confirmation dialog for destructive actions.
//!
//! Unlike `Dialog`, clicking the overlay does NOT close the dialog —
//! the user must explicitly choose an action (confirm or cancel).

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct AlertDialogProps {
    pub open: bool,
    #[props(default)]
    pub on_close: Option<Callback<()>>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-alert-dialog-overlay + cn-alert-dialog-content
#[component]
pub fn AlertDialog(props: AlertDialogProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    rsx! {
        // Overlay — does NOT close on click
        div {
            class: "fixed inset-0 z-50 bg-black/80 animate-fade-in supports-[backdrop-filter]:backdrop-blur-xs",
            "data-state": "open",
        }

        // Content
        div {
            class: crate::cn::merge(format!(
                "fixed z-50 grid w-full max-w-[calc(100%-2rem)] sm:max-w-md gap-6 rounded-xl bg-popover text-popover-foreground border border-border shadow-lg p-6 text-sm animate-scale-in {}",
                props.class
            )),
            style: "left: 50%; top: 50%; transform: translate(-50%, -50%);",
            role: "alertdialog",
            aria_modal: "true",
            onclick: move |evt: MouseEvent| {
                evt.stop_propagation();
            },
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct AlertDialogHeaderProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-alert-dialog-header
#[component]
pub fn AlertDialogHeader(props: AlertDialogHeaderProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex flex-col gap-1.5 text-center sm:text-left", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct AlertDialogTitleProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-alert-dialog-title
#[component]
pub fn AlertDialogTitle(props: AlertDialogTitleProps) -> Element {
    rsx! {
        h2 {
            class: crate::cn::merge_slice(&["text-lg font-medium", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct AlertDialogDescriptionProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-alert-dialog-description
#[component]
pub fn AlertDialogDescription(props: AlertDialogDescriptionProps) -> Element {
    rsx! {
        p {
            class: crate::cn::merge_slice(&["text-muted-foreground text-sm", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct AlertDialogFooterProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-alert-dialog-footer
#[component]
pub fn AlertDialogFooter(props: AlertDialogFooterProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex flex-col-reverse gap-2 sm:flex-row sm:justify-end", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct AlertDialogActionProps {
    pub children: Element,
}

/// Slot for the primary destructive action button.
#[component]
pub fn AlertDialogAction(props: AlertDialogActionProps) -> Element {
    rsx! { {props.children} }
}

#[derive(Props, Clone, PartialEq)]
pub struct AlertDialogCancelProps {
    pub children: Element,
}

/// Slot for the cancel / secondary button.
#[component]
pub fn AlertDialogCancel(props: AlertDialogCancelProps) -> Element {
    rsx! { {props.children} }
}
