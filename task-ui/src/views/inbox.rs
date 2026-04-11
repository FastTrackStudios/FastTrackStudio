use dioxus::prelude::*;
use fts_ui::prelude::*;
use vault_core::{Status, Task};

use crate::components::TaskCard;

#[derive(Props, Clone, PartialEq)]
pub struct InboxViewProps {
    pub tasks: Vec<Task>,
    pub on_complete: EventHandler<String>,
    pub on_tap: EventHandler<String>,
}

#[component]
pub fn InboxView(props: InboxViewProps) -> Element {
    let mut visible: Vec<Task> = props
        .tasks
        .iter()
        .filter(|t| {
            t.has_started()
                && t.projects.is_empty()
                && t.due.is_none()
                && t.scheduled.is_none()
                && matches!(t.status, Status::None | Status::Open | Status::InProgress)
        })
        .cloned()
        .collect();

    visible.sort_by(|a, b| b.date_created.cmp(&a.date_created));

    let count = visible.len();

    rsx! {
        VStack { gap: "4".to_string(),
            SectionHeader {
                label: "Inbox".to_string(),
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{count}" }
                },
            }

            if visible.is_empty() {
                EmptyState { message: "Inbox zero.".to_string() }
            } else {
                VStack { gap: "1".to_string(),
                    for task in visible {
                        {
                            let title = task.title.clone();
                            let title_tap = task.title.clone();
                            rsx! {
                                TaskCard {
                                    key: "{task.title}",
                                    task: task.clone(),
                                    on_complete: move |_| props.on_complete.call(title.clone()),
                                    on_tap: move |_| props.on_tap.call(title_tap.clone()),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
