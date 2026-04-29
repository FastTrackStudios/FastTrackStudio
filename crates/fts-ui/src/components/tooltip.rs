//! Tooltip — shadcn v4 maia style CSS-only tooltip.

use dioxus::prelude::*;

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
    fn position_classes(self) -> &'static str {
        match self {
            Self::Top => "bottom-full left-1/2 -translate-x-1/2 mb-2",
            Self::Bottom => "top-full left-1/2 -translate-x-1/2 mt-2",
            Self::Left => "right-full top-1/2 -translate-y-1/2 mr-2",
            Self::Right => "left-full top-1/2 -translate-y-1/2 ml-2",
        }
    }
}

// ---------------------------------------------------------------------------
// Tooltip (wrapper)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct TooltipProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: tooltip wrapper — uses CSS `group-hover` for show/hide.
#[component]
pub fn Tooltip(props: TooltipProps) -> Element {
    rsx! {
        div {
            class: format!("relative inline-flex group {}", props.class),
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
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: tooltip trigger — renders children directly.
#[component]
pub fn TooltipTrigger(props: TooltipTriggerProps) -> Element {
    rsx! { {props.children} }
}

// ---------------------------------------------------------------------------
// TooltipContent
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct TooltipContentProps {
    /// Which side the tooltip appears on.
    #[props(default)]
    pub side: TooltipSide,

    #[props(default)]
    pub class: String,

    pub children: Element,
}

/// shadcn v4 maia: tooltip content popup
#[component]
pub fn TooltipContent(props: TooltipContentProps) -> Element {
    rsx! {
        div {
            role: "tooltip",
            class: format!(
                "invisible group-hover:visible opacity-0 group-hover:opacity-100 transition-all duration-150 absolute z-50 inline-flex items-center gap-1.5 rounded-lg bg-primary text-primary-foreground px-3 py-1.5 text-xs shadow-md whitespace-nowrap pointer-events-none {} {}",
                props.side.position_classes(),
                props.class
            ),
            {props.children}
        }
    }
}
