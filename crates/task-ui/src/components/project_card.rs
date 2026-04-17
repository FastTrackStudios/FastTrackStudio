use dioxus::prelude::*;
use fts_ui::prelude::*;
use task_core::{Project, Task};

use super::{PriorityDot, TaskProgress};

#[derive(Props, Clone, PartialEq)]
pub struct ProjectCardProps {
    pub project: Project,
    pub next_task: Option<Task>,
    pub completion_percent: Option<f32>,
    pub completed_count: u32,
    pub total_count: u32,
    pub on_tap: EventHandler<()>,
    pub on_complete_next: EventHandler<()>,
}

#[component]
pub fn ProjectCard(props: ProjectCardProps) -> Element {
    let overdue = props.project.is_overdue();
    let pct = props.completion_percent.unwrap_or(0.0);

    rsx! {
        div {
            class: "rounded-2xl ring-1 ring-foreground/10 bg-card text-card-foreground overflow-hidden cursor-pointer hover:ring-foreground/20 transition-all",
            onclick: move |_| props.on_tap.call(()),

            // Header + progress
            div { class: "px-5 pt-5 pb-4",
                // Title row
                div { class: "flex items-center justify-between mb-1",
                    div { class: "flex items-center gap-2 min-w-0",
                        h3 { class: "text-sm font-semibold truncate", "{props.project.title}" }
                        if overdue {
                            span { class: "inline-flex items-center h-5 rounded-full bg-destructive/10 text-destructive px-2 text-xs font-medium",
                                "Overdue"
                            }
                        }
                    }
                    span { class: "text-xs text-muted-foreground tabular-nums shrink-0",
                        "{props.completed_count}/{props.total_count}"
                    }
                }

                // Area tag
                if let Some(ref area) = props.project.area {
                    span { class: "text-xs text-muted-foreground", "{area}" }
                }

                // Due date
                if let Some(due) = props.project.due {
                    span { class: "text-xs text-muted-foreground ml-2", "Due {due}" }
                }

                // Progress bar
                div { class: "mt-3",
                    TaskProgress {
                        percent: pct,
                        label: format!("{pct:.0}%"),
                    }
                }
            }

            // Next task
            div { class: "border-t border-border px-5 py-3",
                match &props.next_task {
                    Some(task) => {
                        let on_complete = props.on_complete_next.clone();
                        let priority = task.priority.clone();
                        rsx! {
                            div { class: "flex items-center gap-3",
                                // Checkbox for next task
                                button {
                                    r#type: "button",
                                    role: "checkbox",
                                    class: "flex size-4 items-center justify-center rounded-[6px] border border-input dark:bg-input/30 cursor-pointer hover:border-ring transition-shadow shrink-0",
                                    "aria-checked": "false",
                                    onclick: move |e| {
                                        e.stop_propagation();
                                        on_complete.call(());
                                    },
                                }
                                div { class: "flex-1 min-w-0",
                                    span { class: "text-xs text-muted-foreground", "Up next" }
                                    p { class: "text-sm truncate", "{task.title}" }
                                }
                                PriorityDot { priority }
                            }
                        }
                    },
                    None => rsx! {
                        div { class: "flex items-center gap-2",
                            span { class: "inline-flex items-center justify-center size-4 rounded-full bg-primary text-primary-foreground shrink-0",
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
                            span { class: "text-sm text-muted-foreground", "All tasks complete" }
                        }
                    },
                }
            }
        }
    }
}
