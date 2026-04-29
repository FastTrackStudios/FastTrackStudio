//! Popover — shadcn v4 maia style positioned floating panel.

use dioxus::prelude::*;
use dioxus_primitives::{
    popover::{
        PopoverContent as PrimitivePopoverContent, PopoverRoot as PrimitivePopover,
        PopoverTrigger as PrimitivePopoverTrigger,
    },
    ContentAlign, ContentSide,
};

#[derive(Props, Clone, PartialEq)]
pub struct PopoverProps {
    #[props(default)]
    pub open: Option<bool>,
    #[props(default = false)]
    pub default_open: bool,
    #[props(default = true)]
    pub is_modal: bool,
    #[props(default)]
    pub on_open_change: Option<Callback<bool>>,
    #[props(default)]
    pub on_close: Option<Callback<()>>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-popover
#[component]
pub fn Popover(props: PopoverProps) -> Element {
    rsx! {
        PrimitivePopover {
            open: props.open,
            default_open: props.default_open,
            is_modal: props.is_modal,
            on_open_change: move |open| {
                if let Some(callback) = &props.on_open_change {
                    callback.call(open);
                }
                if !open {
                    if let Some(callback) = &props.on_close {
                        callback.call(());
                    }
                }
            },
            class: crate::cn::merge_slice(&["relative inline-block", props.class.as_str()]),
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
    rsx! {
        PrimitivePopoverTrigger {
            class: crate::cn::merge_slice(&["cursor-pointer", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct PopoverContentProps {
    #[props(default)]
    pub id: Option<String>,
    #[props(default = ContentSide::Bottom)]
    pub side: ContentSide,
    #[props(default = ContentAlign::Center)]
    pub align: ContentAlign,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-popover-content
#[component]
pub fn PopoverContent(props: PopoverContentProps) -> Element {
    rsx! {
        PrimitivePopoverContent {
            id: props.id,
            side: props.side,
            align: props.align,
            class: crate::cn::merge(format!(
                "bg-popover text-popover-foreground border border-border flex flex-col gap-4 rounded-lg p-4 text-sm shadow-md absolute z-50 mt-2 {}",
                props.class
            )),
            {props.children}
        }
    }
}
