use dioxus::prelude::*;
use fts_ui::prelude::*;
use task_core::Priority;

#[derive(Props, Clone, PartialEq)]
pub struct PriorityBadgeProps {
    pub priority: Priority,
}

#[component]
pub fn PriorityBadge(props: PriorityBadgeProps) -> Element {
    let (color, label) = match &props.priority {
        Priority::None => return rsx! {},
        Priority::Low => (StatusDotColor::Neutral, "Low"),
        Priority::Normal => (StatusDotColor::Custom("var(--primary)".into()), "Normal"),
        Priority::High => (StatusDotColor::Warning, "High"),
        Priority::Urgent => (StatusDotColor::Danger, "Urgent"),
    };

    rsx! {
        div { class: "flex items-center gap-1.5",
            StatusDot { color, size: StatusDotSize::Small }
            span { class: "text-xs text-muted-foreground", "{label}" }
        }
    }
}

/// Priority as a colored dot only (no text label).
/// Used for dense inline task rows.
#[derive(Props, Clone, PartialEq)]
pub struct PriorityDotProps {
    pub priority: Priority,
}

#[component]
pub fn PriorityDot(props: PriorityDotProps) -> Element {
    let color = match &props.priority {
        Priority::None => return rsx! {},
        Priority::Low => StatusDotColor::Neutral,
        Priority::Normal => StatusDotColor::Custom("var(--primary)".into()),
        Priority::High => StatusDotColor::Warning,
        Priority::Urgent => StatusDotColor::Danger,
    };

    rsx! {
        StatusDot { color, size: StatusDotSize::Small }
    }
}
