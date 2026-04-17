use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct TaskProgressProps {
    pub percent: f32,
    #[props(default)]
    pub label: Option<String>,
}

#[component]
pub fn TaskProgress(props: TaskProgressProps) -> Element {
    let pct = props.percent.clamp(0.0, 100.0);
    let color = if pct >= 100.0 {
        "var(--signal-safe)"
    } else {
        "var(--primary)"
    };

    rsx! {
        div { class: "flex items-center gap-2",
            div { class: "flex-1 h-1 rounded-full bg-secondary overflow-hidden",
                div {
                    class: "h-full rounded-full transition-all duration-500",
                    style: "width: {pct:.0}%; background-color: {color};",
                }
            }
            if let Some(label) = &props.label {
                span { class: "text-xs text-muted-foreground tabular-nums w-8 text-right", "{label}" }
            }
        }
    }
}
