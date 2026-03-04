//! Badge — small status indicator with text label.

use dioxus::prelude::*;

/// Visual variant for the badge.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum BadgeVariant {
    #[default]
    Default,
    Secondary,
    Destructive,
    Outline,
}

impl BadgeVariant {
    fn classes(self) -> &'static str {
        match self {
            Self::Default => "border-transparent bg-primary text-primary-foreground shadow-sm hover:bg-primary/80",
            Self::Secondary => "border-transparent bg-secondary text-secondary-foreground hover:bg-secondary/80",
            Self::Destructive => "border-transparent bg-destructive text-destructive-foreground shadow-sm hover:bg-destructive/80",
            Self::Outline => "text-foreground",
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct BadgeProps {
    /// Visual variant.
    #[props(default)]
    pub variant: BadgeVariant,

    /// Extra CSS classes.
    #[props(default)]
    pub class: String,

    pub children: Element,
}

#[component]
pub fn Badge(props: BadgeProps) -> Element {
    rsx! {
        span {
            class: format!(
                "inline-flex items-center rounded-md border px-2.5 py-0.5 text-xs font-semibold transition-colors focus:outline-none focus:ring-2 focus:ring-ring focus:ring-offset-2 {} {}",
                props.variant.classes(),
                props.class
            ),
            {props.children}
        }
    }
}
