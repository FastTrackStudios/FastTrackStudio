//! Popover — shadcn v4 maia style positioned floating panel.

use dioxus::prelude::*;
use dioxus_primitives::{
    ContentAlign, ContentSide,
    popover::{
        PopoverContent as PrimitivePopoverContent, PopoverRoot as PrimitivePopover,
        PopoverTrigger as PrimitivePopoverTrigger,
    },
};
use fts_story_runtime::story;

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

/// Popover forced open with simple content.
#[story(category = "Popover", name = "popover default")]
pub fn popover_default() -> Element {
    let mut open = use_signal(|| true);
    rsx! {
        div { class: "p-6 bg-background text-foreground flex items-center justify-center min-h-[18rem]",
            Popover {
                open: open(),
                is_modal: false,
                on_open_change: move |o| open.set(o),
                PopoverTrigger {
                    button { class: "h-9 px-3 rounded-lg border border-border text-sm", "Open popover" }
                }
                PopoverContent {
                    div { class: "flex flex-col gap-1",
                        span { class: "text-sm font-medium", "Popover" }
                        span { class: "text-xs text-muted-foreground", "Anchored content next to the trigger." }
                    }
                }
            }
        }
    }
}
