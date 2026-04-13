//! Pagination — shadcn v4 maia style.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct PaginationProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Nav wrapper for pagination controls.
#[component]
pub fn Pagination(props: PaginationProps) -> Element {
    rsx! {
        nav {
            class: "flex justify-center {props.class}",
            role: "navigation",
            aria_label: "pagination",
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct PaginationContentProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// The list container for pagination items.
#[component]
pub fn PaginationContent(props: PaginationContentProps) -> Element {
    rsx! {
        div {
            class: "flex items-center gap-1 {props.class}",
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct PaginationItemProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// Wraps each pagination button.
#[component]
pub fn PaginationItem(props: PaginationItemProps) -> Element {
    rsx! {
        div {
            class: "{props.class}",
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct PaginationLinkProps {
    pub page: u32,
    #[props(default = false)]
    pub is_active: bool,
    pub on_click: Callback<u32>,
    #[props(default)]
    pub class: String,
}

/// A page number button.
#[component]
pub fn PaginationLink(props: PaginationLinkProps) -> Element {
    let base = "inline-flex items-center justify-center h-9 min-w-9 rounded-lg border text-sm font-medium transition-colors";

    let state_class = if props.is_active {
        "border-input bg-input/30 text-foreground"
    } else {
        "border-transparent hover:bg-muted text-muted-foreground hover:text-foreground"
    };

    let page = props.page;

    rsx! {
        button {
            r#type: "button",
            class: "{base} {state_class} {props.class}",
            aria_current: if props.is_active { Some("page") } else { None },
            onclick: move |_| {
                props.on_click.call(page);
            },
            "{page}"
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct PaginationPreviousProps {
    pub on_click: Callback<()>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
}

/// Previous page button with left chevron.
#[component]
pub fn PaginationPrevious(props: PaginationPreviousProps) -> Element {
    let base = "inline-flex items-center justify-center h-9 min-w-9 rounded-lg border border-transparent text-sm font-medium transition-colors gap-1 pl-2";

    let disabled_class = if props.disabled {
        "pointer-events-none opacity-50"
    } else {
        "hover:bg-muted text-muted-foreground hover:text-foreground"
    };

    rsx! {
        button {
            r#type: "button",
            class: "{base} {disabled_class} {props.class}",
            disabled: if props.disabled { Some(true) } else { None },
            onclick: move |_| {
                props.on_click.call(());
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
                path { d: "m15 18-6-6 6-6" }
            }
            "Previous"
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct PaginationNextProps {
    pub on_click: Callback<()>,
    #[props(default = false)]
    pub disabled: bool,
    #[props(default)]
    pub class: String,
}

/// Next page button with right chevron.
#[component]
pub fn PaginationNext(props: PaginationNextProps) -> Element {
    let base = "inline-flex items-center justify-center h-9 min-w-9 rounded-lg border border-transparent text-sm font-medium transition-colors gap-1 pr-2";

    let disabled_class = if props.disabled {
        "pointer-events-none opacity-50"
    } else {
        "hover:bg-muted text-muted-foreground hover:text-foreground"
    };

    rsx! {
        button {
            r#type: "button",
            class: "{base} {disabled_class} {props.class}",
            disabled: if props.disabled { Some(true) } else { None },
            onclick: move |_| {
                props.on_click.call(());
            },
            "Next"
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
                path { d: "m9 18 6-6-6-6" }
            }
        }
    }
}

/// Ellipsis indicator for skipped pages.
#[component]
pub fn PaginationEllipsis() -> Element {
    rsx! {
        span {
            class: "flex size-9 items-center justify-center text-muted-foreground",
            "..."
        }
    }
}
