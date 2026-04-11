use dioxus::prelude::*;
use fts_ui::prelude::*;
use vault_core::Task;

use super::PriorityDot;

#[derive(Props, Clone, PartialEq)]
pub struct TaskCardProps {
    pub task: Task,
    pub on_complete: EventHandler<()>,
    #[props(default)]
    pub on_tap: Option<EventHandler<()>>,
}

#[component]
pub fn TaskCard(props: TaskCardProps) -> Element {
    let task = &props.task;
    let overdue = task.is_overdue();
    let done = task.is_complete();

    let on_complete = props.on_complete.clone();
    let on_tap = props.on_tap.clone();

    // Detail line: project · @context · ~estimate
    let mut detail_parts: Vec<String> = Vec::new();
    for proj in &task.projects {
        detail_parts.push(proj.0.clone());
    }
    for ctx in &task.contexts {
        detail_parts.push(format!("@{ctx}"));
    }
    if let Some(est) = task.time_estimate {
        detail_parts.push(format!("~{est}m"));
    }
    if task.is_blocked() {
        detail_parts.push("Blocked".into());
    }
    let detail_str = detail_parts.join(" \u{b7} ");

    // Due date display
    let due_display: Option<(String, bool)> = task.due.map(|d| {
        let today = chrono::Local::now().date_naive();
        let days = (d - today).num_days();
        let label = match days {
            0 => "Today".to_string(),
            1 => "Tomorrow".to_string(),
            -1 => "Yesterday".to_string(),
            _ => d.format("%b %d").to_string(),
        };
        (label, overdue)
    });

    // Row opacity
    let row_class = if done {
        "group flex items-start gap-3 px-2 py-2 rounded-lg hover:bg-accent/50 transition-colors cursor-default opacity-40"
    } else {
        "group flex items-start gap-3 px-2 py-2 rounded-lg hover:bg-accent/50 transition-colors cursor-default"
    };

    // Checkbox style
    let check_class = if done {
        "flex size-4 items-center justify-center rounded-[6px] border border-primary bg-primary text-primary-foreground cursor-pointer transition-shadow shrink-0 mt-0.5"
    } else if overdue {
        "flex size-4 items-center justify-center rounded-[6px] border border-destructive cursor-pointer hover:bg-destructive/10 transition-shadow shrink-0 mt-0.5"
    } else {
        "flex size-4 items-center justify-center rounded-[6px] border border-input dark:bg-input/30 cursor-pointer hover:border-ring transition-shadow shrink-0 mt-0.5"
    };

    let title_class = if done {
        "text-sm font-medium line-through text-muted-foreground"
    } else {
        "text-sm font-medium text-foreground"
    };

    rsx! {
        div {
            class: row_class,
            onclick: move |_| {
                if let Some(ref h) = on_tap {
                    h.call(());
                }
            },

            // Checkbox
            button {
                r#type: "button",
                role: "checkbox",
                class: check_class,
                "aria-checked": "{done}",
                onclick: move |e| {
                    e.stop_propagation();
                    if !done {
                        on_complete.call(());
                    }
                },
                if done {
                    svg {
                        class: "size-3",
                        xmlns: "http://www.w3.org/2000/svg",
                        view_box: "0 0 24 24",
                        fill: "none",
                        stroke: "currentColor",
                        stroke_width: "3",
                        stroke_linecap: "round",
                        stroke_linejoin: "round",
                        path { d: "M20 6 9 17l-5-5" }
                    }
                }
            }

            // Title + detail
            div { class: "flex-1 min-w-0",
                span { class: title_class, "{task.title}" }
                if !detail_str.is_empty() {
                    p { class: "text-xs text-muted-foreground truncate mt-0.5", "{detail_str}" }
                }
            }

            // Right side: tags + due + priority
            div { class: "flex items-center gap-2 shrink-0",
                for tag in &task.tags {
                    span { class: "inline-flex items-center h-5 rounded-full bg-secondary text-secondary-foreground px-2 text-xs font-medium",
                        "#{tag}"
                    }
                }

                match &due_display {
                    Some((label, is_overdue)) if *is_overdue => rsx! {
                        span { class: "inline-flex items-center h-5 rounded-full bg-destructive/10 text-destructive px-2 text-xs font-medium",
                            "{label}"
                        }
                    },
                    Some((label, _)) => rsx! {
                        span { class: "text-xs text-muted-foreground whitespace-nowrap", "{label}" }
                    },
                    None => rsx! {},
                }

                PriorityDot { priority: task.priority.clone() }
            }
        }
    }
}
