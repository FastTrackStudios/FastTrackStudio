//! HoverCard — CSS-only hover card (no JS positioning).

use dioxus::prelude::*;

/// Which side the hover card appears on.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum HoverCardSide {
    Top,
    #[default]
    Bottom,
}

// ---------------------------------------------------------------------------
// HoverCard (wrapper)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct HoverCardProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Wrapper that establishes the `group` and `relative` positioning context.
#[component]
pub fn HoverCard(props: HoverCardProps) -> Element {
    rsx! {
        div {
            class: format!("relative inline-flex group {}", props.class),
            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// HoverCardTrigger
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct HoverCardTriggerProps {
    pub children: Element,
}

/// The element the user hovers over.
#[component]
pub fn HoverCardTrigger(props: HoverCardTriggerProps) -> Element {
    rsx! { {props.children} }
}

// ---------------------------------------------------------------------------
// HoverCardContent
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct HoverCardContentProps {
    #[props(default)]
    pub side: HoverCardSide,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// The popup card shown on hover.
#[component]
pub fn HoverCardContent(props: HoverCardContentProps) -> Element {
    let position = match props.side {
        HoverCardSide::Bottom => "top-full left-1/2 -translate-x-1/2 mt-2",
        HoverCardSide::Top => "bottom-full left-1/2 -translate-x-1/2 mb-2",
    };

    rsx! {
        div {
            class: format!(
                "invisible group-hover:visible opacity-0 group-hover:opacity-100 transition-all duration-200 delay-200 absolute z-50 w-64 rounded-lg bg-popover text-popover-foreground border border-border shadow-md p-4 text-sm pointer-events-none group-hover:pointer-events-auto {} {}",
                position, props.class
            ),
            {props.children}
        }
    }
}
