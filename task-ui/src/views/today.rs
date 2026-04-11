use std::cmp::Reverse;

use dioxus::prelude::*;
use fts_ui::prelude::*;
use vault_core::{Status, Task};

use crate::components::TaskCard;

#[derive(Props, Clone, PartialEq)]
pub struct TodayViewProps {
    pub tasks: Vec<Task>,
    pub on_complete: EventHandler<String>,
    pub on_tap: EventHandler<String>,
}

#[component]
pub fn TodayView(props: TodayViewProps) -> Element {
    let today = chrono::Local::now().date_naive();

    let mut all: Vec<Task> = props
        .tasks
        .iter()
        .filter(|t| {
            t.has_started()
                && !t.is_complete()
                && t.status != Status::Cancelled
                && t.status != Status::Archived
                && (t.due.map(|d| d == today).unwrap_or(false)
                    || t.scheduled.map(|d| d == today).unwrap_or(false)
                    || t.is_overdue()
                    || t.status == Status::InProgress)
        })
        .cloned()
        .collect();

    all.sort_by_key(|t| Reverse(t.urgency_score()));

    let overdue: Vec<Task> = all.iter().filter(|t| t.is_overdue()).cloned().collect();
    let due_today: Vec<Task> = all
        .iter()
        .filter(|t| !t.is_overdue() && t.due.map(|d| d == today).unwrap_or(false))
        .cloned()
        .collect();
    let in_progress: Vec<Task> = all
        .iter()
        .filter(|t| {
            !t.is_overdue()
                && !t.due.map(|d| d == today).unwrap_or(false)
                && t.status == Status::InProgress
        })
        .cloned()
        .collect();

    let total = all.len();

    rsx! {
        VStack { gap: "4".to_string(),
            SectionHeader {
                label: "Today".to_string(),
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{total}" }
                },
            }

            if all.is_empty() {
                EmptyState { message: "You're all caught up.".to_string() }
            } else {
                VStack { gap: "6".to_string(),
                    if !overdue.is_empty() {
                        VStack { gap: "1".to_string(),
                            div { class: "flex items-center gap-2 px-3 py-1",
                                span { class: "text-xs font-semibold uppercase tracking-wider text-destructive", "Overdue" }
                                Badge { variant: BadgeVariant::Secondary, "{overdue.len()}" }
                            }
                            for task in &overdue {
                                {
                                    let tc = task.title.clone();
                                    let tt = task.title.clone();
                                    rsx! {
                                        TaskCard {
                                            key: "{task.title}",
                                            task: task.clone(),
                                            on_complete: move |_| props.on_complete.call(tc.clone()),
                                            on_tap: move |_| props.on_tap.call(tt.clone()),
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if !due_today.is_empty() {
                        VStack { gap: "1".to_string(),
                            div { class: "flex items-center gap-2 px-3 py-1",
                                span { class: "text-xs font-semibold uppercase tracking-wider", "Due Today" }
                                Badge { variant: BadgeVariant::Secondary, "{due_today.len()}" }
                            }
                            for task in &due_today {
                                {
                                    let tc = task.title.clone();
                                    let tt = task.title.clone();
                                    rsx! {
                                        TaskCard {
                                            key: "{task.title}",
                                            task: task.clone(),
                                            on_complete: move |_| props.on_complete.call(tc.clone()),
                                            on_tap: move |_| props.on_tap.call(tt.clone()),
                                        }
                                    }
                                }
                            }
                        }
                    }

                    if !in_progress.is_empty() {
                        VStack { gap: "1".to_string(),
                            div { class: "flex items-center gap-2 px-3 py-1",
                                span { class: "text-xs font-semibold uppercase tracking-wider", "In Progress" }
                                Badge { variant: BadgeVariant::Secondary, "{in_progress.len()}" }
                            }
                            for task in &in_progress {
                                {
                                    let tc = task.title.clone();
                                    let tt = task.title.clone();
                                    rsx! {
                                        TaskCard {
                                            key: "{task.title}",
                                            task: task.clone(),
                                            on_complete: move |_| props.on_complete.call(tc.clone()),
                                            on_tap: move |_| props.on_tap.call(tt.clone()),
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
