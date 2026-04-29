//! Breadcrumb — shadcn v4 maia style breadcrumb navigation.

use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-breadcrumb
#[component]
pub fn Breadcrumb(props: BreadcrumbProps) -> Element {
    rsx! {
        nav {
            class: props.class,
            aria_label: "breadcrumb",
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbListProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-breadcrumb-list
#[component]
pub fn BreadcrumbList(props: BreadcrumbListProps) -> Element {
    rsx! {
        ol {
            class: format!(
                "flex items-center flex-wrap text-muted-foreground gap-1.5 text-sm sm:gap-2.5 {}",
                props.class
            ),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbItemProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-breadcrumb-item
#[component]
pub fn BreadcrumbItem(props: BreadcrumbItemProps) -> Element {
    rsx! {
        li {
            class: format!("inline-flex items-center gap-1.5 {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbLinkProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-breadcrumb-link
#[component]
pub fn BreadcrumbLink(props: BreadcrumbLinkProps) -> Element {
    rsx! {
        a {
            class: format!("hover:text-foreground transition-colors {}", props.class),
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbPageProps {
    #[props(default)]
    pub class: String,
    pub children: Element,
}

/// shadcn v4 maia: cn-breadcrumb-page — current page, not a link.
#[component]
pub fn BreadcrumbPage(props: BreadcrumbPageProps) -> Element {
    rsx! {
        span {
            class: format!("text-foreground font-normal {}", props.class),
            role: "link",
            aria_disabled: "true",
            aria_current: "page",
            {props.children}
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct BreadcrumbSeparatorProps {
    #[props(default)]
    pub class: String,
    /// Optional custom separator element. Defaults to a chevron-right SVG.
    #[props(default)]
    pub children: Element,
}

/// shadcn v4 maia: cn-breadcrumb-separator — chevron-right icon by default.
#[component]
pub fn BreadcrumbSeparator(props: BreadcrumbSeparatorProps) -> Element {
    rsx! {
        li {
            class: format!("[&>svg]:size-3.5 {}", props.class),
            role: "presentation",
            aria_hidden: "true",
            if props.children == VNode::empty() {
                // Default chevron-right icon
                svg {
                    class: "size-3.5",
                    xmlns: "http://www.w3.org/2000/svg",
                    width: "24",
                    height: "24",
                    view_box: "0 0 24 24",
                    fill: "none",
                    stroke: "currentColor",
                    stroke_width: "2",
                    stroke_linecap: "round",
                    stroke_linejoin: "round",
                    path { d: "m9 18 6-6-6-6" }
                }
            } else {
                {props.children}
            }
        }
    }
}
