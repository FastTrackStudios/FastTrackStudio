//! Section header — uppercase label with optional trailing action.
//!
//! Used to label groups of content in sidebars, panels, and lists.

use dioxus::prelude::*;

/// Size variants for the section header.
#[derive(Clone, Copy, PartialEq, Default)]
pub enum SectionHeaderSize {
    /// Tiny — `text-[8px]` with wide tracking. Used inside dropdowns/pickers.
    Small,
    /// Standard — `text-sm`. Used for dashboard/panel sections.
    #[default]
    Medium,
}

impl SectionHeaderSize {
    fn classes(self) -> &'static str {
        match self {
            Self::Small => "text-[8px] font-semibold uppercase tracking-[0.2em]",
            Self::Medium => "text-sm font-medium uppercase tracking-wider",
        }
    }
}

/// An uppercase section label with an optional trailing element (button, count, etc.).
///
/// ```rust,ignore
/// SectionHeader {
///     label: "Instances",
///     trailing: rsx! { Button { "Import" } },
/// }
/// ```
#[derive(Props, Clone, PartialEq)]
pub struct SectionHeaderProps {
    /// Section label text (rendered uppercase by CSS).
    pub label: String,

    /// Size variant.
    #[props(default)]
    pub size: SectionHeaderSize,

    /// Optional trailing element (action button, badge, count).
    #[props(default)]
    pub trailing: Option<Element>,

    /// Extra CSS classes.
    #[props(default)]
    pub class: String,
}

#[component]
pub fn SectionHeader(props: SectionHeaderProps) -> Element {
    rsx! {
        div {
            class: format!("flex items-center justify-between {}", props.class),

            span {
                class: format!("text-muted-foreground {}", props.size.classes()),
                "{props.label}"
            }

            if let Some(trailing) = &props.trailing {
                {trailing}
            }
        }
    }
}
