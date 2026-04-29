//! Drawer — shadcn v4 maia style bottom/side drawer (mobile-friendly sheet variant).

use dioxus::prelude::*;

/// Which edge the drawer slides in from.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum DrawerSide {
    #[default]
    Bottom,
    Left,
    Right,
}

#[derive(Props, Clone, PartialEq)]
pub struct DrawerProps {
    pub open: bool,
    #[props(default)]
    pub on_close: Option<Callback<()>>,
    #[props(default)]
    pub side: DrawerSide,
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-drawer-overlay + cn-drawer-content
#[component]
pub fn Drawer(props: DrawerProps) -> Element {
    if !props.open {
        return rsx! {};
    }

    let position_classes = match props.side {
        DrawerSide::Bottom => "fixed inset-x-0 bottom-0 z-50 mt-24 max-h-[80vh] rounded-t-xl",
        DrawerSide::Left => "fixed inset-y-0 left-0 z-50 w-3/4 sm:max-w-sm rounded-r-xl",
        DrawerSide::Right => "fixed inset-y-0 right-0 z-50 w-3/4 sm:max-w-sm rounded-l-xl",
    };

    rsx! {
        // Overlay — closes on click
        div {
            class: "fixed inset-0 z-50 bg-black/80 animate-fade-in supports-[backdrop-filter]:backdrop-blur-xs",
            "data-state": "open",
            onclick: move |_| {
                if let Some(cb) = &props.on_close {
                    cb.call(());
                }
            },
        }

        // Content
        div {
            class: crate::cn::merge(format!(
                "{position_classes} flex flex-col bg-popover text-popover-foreground border border-border shadow-lg p-4 text-sm {}",
                props.class
            )),
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
pub struct DrawerHeaderProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-drawer-header
#[component]
pub fn DrawerHeader(props: DrawerHeaderProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex flex-col gap-0.5 p-4", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DrawerTitleProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-drawer-title
#[component]
pub fn DrawerTitle(props: DrawerTitleProps) -> Element {
    rsx! {
        h2 {
            class: crate::cn::merge_slice(&["text-foreground text-base font-medium", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DrawerDescriptionProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-drawer-description
#[component]
pub fn DrawerDescription(props: DrawerDescriptionProps) -> Element {
    rsx! {
        p {
            class: crate::cn::merge_slice(&["text-muted-foreground text-sm", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DrawerFooterProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-drawer-footer
#[component]
pub fn DrawerFooter(props: DrawerFooterProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge_slice(&["flex flex-col gap-2 p-4", props.class.as_str()]),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct DrawerHandleProps {
    #[props(default)]
    pub class: String,
}

/// Drag handle indicator for bottom drawers.
#[component]
pub fn DrawerHandle(props: DrawerHandleProps) -> Element {
    rsx! {
        div {
            class: crate::cn::merge(format!(
                "mx-auto mt-4 h-1.5 w-[100px] shrink-0 rounded-full bg-muted {}",
                props.class
            )),
        }
    }
}
