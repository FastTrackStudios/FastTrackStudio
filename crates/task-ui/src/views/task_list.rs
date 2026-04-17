use dioxus::prelude::*;
use fts_ui::prelude::*;
use task_core::Task;

use crate::components::TaskCard;

#[derive(Props, Clone, PartialEq)]
pub struct TaskListProps {
    pub tasks: Vec<Task>,
    pub on_complete: EventHandler<String>,
    #[props(default)]
    pub on_tap: Option<EventHandler<String>>,
}

#[component]
pub fn TaskList(props: TaskListProps) -> Element {
    if props.tasks.is_empty() {
        return rsx! {
            EmptyState { message: "No tasks to show".to_string() }
        };
    }

    rsx! {
        VStack { gap: "1".to_string(),
            for task in &props.tasks {
                {
                    let tc = task.title.clone();
                    let tt = task.title.clone();
                    let on_tap = props.on_tap.clone();
                    rsx! {
                        TaskCard {
                            key: "{task.title}",
                            task: task.clone(),
                            on_complete: move |_| props.on_complete.call(tc.clone()),
                            on_tap: move |_| {
                                if let Some(ref h) = on_tap {
                                    h.call(tt.clone());
                                }
                            },
                        }
                    }
                }
            }
        }
    }
}
