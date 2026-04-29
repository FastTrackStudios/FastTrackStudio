//! Tooltip — shadcn v4 maia style CSS-only tooltip.

use dioxus::prelude::*;
use dioxus_primitives::{
    tooltip::{
        Tooltip as PrimitiveTooltip, TooltipContent as PrimitiveTooltipContent,
        TooltipTrigger as PrimitiveTooltipTrigger,
    },
    ContentAlign, ContentSide,
};

// ---------------------------------------------------------------------------
// TooltipSide
// ---------------------------------------------------------------------------

/// Which side the tooltip content appears on relative to the trigger.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum TooltipSide {
    #[default]
    Top,
    Bottom,
    Left,
    Right,
}

impl TooltipSide {
    fn primitive_side(self) -> ContentSide {
        match self {
            Self::Top => ContentSide::Top,
            Self::Bottom => ContentSide::Bottom,
            Self::Left => ContentSide::Left,
            Self::Right => ContentSide::Right,
        }
    }
}

// ---------------------------------------------------------------------------
// Tooltip (wrapper)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct TooltipProps {
    #[props(default)]
    pub open: Option<bool>,
    #[props(default = false)]
    pub default_open: bool,
    #[props(default)]
    pub on_open_change: Option<Callback<bool>>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: tooltip wrapper — uses CSS `group-hover` for show/hide.
#[component]
pub fn Tooltip(props: TooltipProps) -> Element {
    rsx! {
        PrimitiveTooltip {
            open: props.open,
            default_open: props.default_open,
            disabled: props.disabled,
            on_open_change: move |open| {
                if let Some(callback) = &props.on_open_change {
                    callback.call(open);
                }
            },
            class: crate::cn::merge_slice(&["relative inline-flex group", props.class.as_str()]),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// TooltipTrigger
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct TooltipTriggerProps {
    #[props(default)]
    pub id: Option<String>,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: tooltip trigger — renders children directly.
#[component]
pub fn TooltipTrigger(props: TooltipTriggerProps) -> Element {
    rsx! {
        PrimitiveTooltipTrigger {
            id: props.id,
            class: props.class,
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// TooltipContent
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct TooltipContentProps {
    /// Which side the tooltip appears on.
    #[props(default)]
    pub side: TooltipSide,
    #[props(default = ContentAlign::Center)]
    pub align: ContentAlign,
    #[props(default)]
    pub id: Option<String>,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: tooltip content popup
#[component]
pub fn TooltipContent(props: TooltipContentProps) -> Element {
    rsx! {
        PrimitiveTooltipContent {
            id: props.id,
            side: props.side.primitive_side(),
            align: props.align,
            class: crate::cn::merge(format!(
                "absolute z-50 inline-flex items-center gap-1.5 rounded-lg bg-primary text-primary-foreground px-3 py-1.5 text-xs shadow-md whitespace-nowrap pointer-events-none data-[side=top]:bottom-full data-[side=top]:left-1/2 data-[side=top]:-translate-x-1/2 data-[side=top]:mb-2 data-[side=bottom]:top-full data-[side=bottom]:left-1/2 data-[side=bottom]:-translate-x-1/2 data-[side=bottom]:mt-2 data-[side=left]:right-full data-[side=left]:top-1/2 data-[side=left]:-translate-y-1/2 data-[side=left]:mr-2 data-[side=right]:left-full data-[side=right]:top-1/2 data-[side=right]:-translate-y-1/2 data-[side=right]:ml-2 {}",
                props.class
            )),
            {props.children}
        }
    }
}
