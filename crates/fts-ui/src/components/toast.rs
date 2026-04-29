//! Toast — primitive-backed notifications with FTS styling defaults.

use dioxus::dioxus_core::DynamicNode;
use dioxus::prelude::*;
use dioxus_primitives::toast::{
    Toast as PrimitiveToast, ToastProps as PrimitiveToastProps,
    ToastPropsWithOwner as PrimitiveToastPropsWithOwner, ToastProvider as PrimitiveToastProvider,
};
use std::time::Duration;

pub use dioxus_primitives::toast::{consume_toast, use_toast, ToastOptions, ToastType, Toasts};

#[component]
pub fn ToastProvider(
    #[props(default = Some(Duration::from_secs(5)))] default_duration: Option<Duration>,
    #[props(default = 10)] max_toasts: usize,
    children: Element,
) -> Element {
    rsx! {
        PrimitiveToastProvider {
            default_duration,
            max_toasts,
            render_toast: Callback::new(|props: PrimitiveToastPropsWithOwner| {
                rsx! { {DynamicNode::Component(props.into_vcomponent(Toast))} }
            }),
            {children}
        }
    }
}

#[component]
pub fn Toast(props: PrimitiveToastProps) -> Element {
    let variant_class = match props.toast_type {
        ToastType::Success => "border-l-4 border-l-green-500",
        ToastType::Error => "border-l-4 border-l-destructive",
        ToastType::Warning => "border-l-4 border-l-yellow-500",
        ToastType::Info => "",
    };

    rsx! {
        PrimitiveToast {
            id: props.id,
            index: props.index,
            title: props.title,
            description: props.description,
            toast_type: props.toast_type,
            on_close: props.on_close,
            permanent: props.permanent,
            duration: props.duration,
            class: crate::cn::merge_slice(&[
                "pointer-events-auto relative grid w-full min-w-80 max-w-sm gap-1 rounded-lg border border-border bg-popover p-4 pr-10 text-sm text-popover-foreground shadow-md outline-none",
                "[&_.toast-content]:grid [&_.toast-content]:gap-1 [&_.toast-title]:font-medium [&_.toast-description]:text-muted-foreground [&_.toast-close]:absolute [&_.toast-close]:right-3 [&_.toast-close]:top-3 [&_.toast-close]:rounded-md [&_.toast-close]:opacity-70 [&_.toast-close]:transition-opacity [&_.toast-close:hover]:opacity-100 [&_.toast-close:focus-visible]:outline-none [&_.toast-close:focus-visible]:ring-2 [&_.toast-close:focus-visible]:ring-ring",
                "data-[type=success]:border-l-green-500 data-[type=error]:border-l-destructive data-[type=warning]:border-l-yellow-500",
                variant_class,
            ]),
        }
    }
}

pub fn styled_toast_options() -> ToastOptions {
    ToastOptions::new()
}
