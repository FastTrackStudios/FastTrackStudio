//! Sheet (Side Sheet) — standalone, shadcn v4 maia style.

use dioxus::prelude::*;

// ---------------------------------------------------------------------------
// Types
// ---------------------------------------------------------------------------

/// Which edge the sheet slides in from.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum SheetSide {
    Left,
    #[default]
    Right,
}

// ---------------------------------------------------------------------------
// Sheet (root)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct SheetProps {
    pub open: bool,
    #[props(default)]
    pub on_close: Option<Callback<()>>,
    #[props(default)]
    pub side: SheetSide,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: Sheet overlay + content panel.
#[component]
pub fn Sheet(props: SheetProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    let on_close = props.on_close;

    let position_class = match props.side {
        SheetSide::Right => "fixed inset-y-0 right-0 z-50 w-3/4 sm:max-w-sm",
        SheetSide::Left => "fixed inset-y-0 left-0 z-50 w-3/4 sm:max-w-sm",
    };

    let border_class = match props.side {
        SheetSide::Right => "border-l border-border",
        SheetSide::Left => "border-r border-border",
    };

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
            class: format!(
                "{position_class} flex flex-col h-full bg-popover text-popover-foreground {border_class} p-6 gap-4 {}",
                props.class
            ),
            onclick: move |evt: MouseEvent| { evt.stop_propagation(); },

            // Close button
            SheetClose { on_close: on_close }

            {props.children}
        }
    }
}

// ---------------------------------------------------------------------------
// SheetClose (X button)
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
struct SheetCloseProps {
    on_close: Option<Callback<()>>,
}

#[component]
fn SheetClose(props: SheetCloseProps) -> Element {
    let on_close = props.on_close;

    rsx! {
        button {
            class: "absolute top-4 right-4 opacity-70 hover:opacity-100 cursor-pointer",
            onclick: move |_| {
                if let Some(cb) = &on_close {
                    cb.call(());
                }
            },
            svg {
                xmlns: "http://www.w3.org/2000/svg",
                width: "16",
                height: "16",
                view_box: "0 0 24 24",
                fill: "none",
                stroke: "currentColor",
                stroke_width: "2",
                stroke_linecap: "round",
                stroke_linejoin: "round",
                line { x1: "18", y1: "6", x2: "6", y2: "18" }
                line { x1: "6", y1: "6", x2: "18", y2: "18" }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Sub-components
// ---------------------------------------------------------------------------

#[derive(Props, Clone, PartialEq)]
pub struct SheetHeaderProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-sheet-header
#[component]
pub fn SheetHeader(props: SheetHeaderProps) -> Element {
    rsx! {
        div {
            class: format!("flex flex-col gap-2 {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SheetTitleProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-sheet-title
#[component]
pub fn SheetTitle(props: SheetTitleProps) -> Element {
    rsx! {
        h2 {
            class: format!("text-base font-medium {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SheetDescriptionProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-sheet-description
#[component]
pub fn SheetDescription(props: SheetDescriptionProps) -> Element {
    rsx! {
        p {
            class: format!("text-muted-foreground text-sm {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct SheetFooterProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-sheet-footer
#[component]
pub fn SheetFooter(props: SheetFooterProps) -> Element {
    rsx! {
        div {
            class: format!("flex flex-col-reverse gap-2 sm:flex-row sm:justify-end mt-auto {}", props.class),
            {props.children}
        }
    }
}
