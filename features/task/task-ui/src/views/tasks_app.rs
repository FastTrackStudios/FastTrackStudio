//! Top-level Tasks app. Owns the view-mode toggle + the
//! currently-open detail panel; everything else is dumb.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use uuid::Uuid;

use crate::TaskInfo;
use crate::TaskMutation;
use crate::model::Status;

use super::detail::TaskDetail;
use super::kanban::KanbanBoard;
use super::list::TaskList;
use super::quick_add::QuickAdd;

#[derive(Clone, Copy, PartialEq, Eq, Default)]
pub enum ViewMode {
    #[default]
    List,
    Kanban,
}

impl ViewMode {
    fn label(self) -> &'static str {
        match self {
            Self::List => "List",
            Self::Kanban => "Kanban",
        }
    }
}

#[derive(Props, Clone, PartialEq)]
pub struct TasksAppProps {
    pub tasks: Vec<TaskInfo>,
    pub on_event: EventHandler<TaskMutation>,
    #[props(default)]
    pub initial_view: Option<ViewMode>,
}

#[component]
pub fn TasksApp(props: TasksAppProps) -> Element {
    let mut view = use_signal(|| props.initial_view.unwrap_or_default());
    let mut open_id: Signal<Option<Uuid>> = use_signal(|| None);

    let selected: Option<TaskInfo> = open_id
        .read()
        .and_then(|id| props.tasks.iter().find(|t| t.id == id).cloned());

    let total = props.tasks.len();
    let done = props
        .tasks
        .iter()
        .filter(|t| t.status_enum() == Status::Done)
        .count();
    let open_count = total - done;

    rsx! {
        div { class: "relative mx-auto flex max-w-5xl flex-col gap-4 p-4 sm:p-6 lg:p-10 h-full",
            div { class: "flex flex-wrap items-center gap-x-3 gap-y-2",
                Heading { level: HeadingLevel::H1, "Tasks" }
                span { class: "text-xs text-muted-foreground", "{open_count} open · {done} done" }
                div { class: "ml-auto inline-flex items-center gap-0.5 rounded-lg bg-muted/40 p-0.5 text-xs",
                    for v in [ViewMode::List, ViewMode::Kanban] {
                        {
                            let active = v == view();
                            let cls = if active {
                                "bg-background text-foreground shadow-sm font-medium px-3 py-1 rounded-md transition-colors"
                            } else {
                                "text-muted-foreground hover:text-foreground px-3 py-1 rounded-md transition-colors"
                            };
                            rsx! {
                                button {
                                    key: "{v.label()}",
                                    r#type: "button",
                                    class: "{cls}",
                                    onclick: move |_| view.set(v),
                                    "{v.label()}"
                                }
                            }
                        }
                    }
                }
            }
            QuickAdd {
                on_create: move |task: TaskInfo| props.on_event.call(TaskMutation::Create { task }),
            }
            div { class: "flex-1 min-h-0 overflow-y-auto",
                match view() {
                    ViewMode::List => rsx! {
                        TaskList {
                            tasks: props.tasks.clone(),
                            on_toggle: move |id: Uuid| {
                                let next = props.tasks.iter().find(|t| t.id == id).map(|t| {
                                    if t.is_done() { Status::Open } else { Status::Done }
                                });
                                if let Some(s) = next {
                                    props.on_event.call(TaskMutation::SetStatus { id, status: s.as_str().to_string() });
                                }
                            },
                            on_open: move |id: Uuid| open_id.set(Some(id)),
                        }
                    },
                    ViewMode::Kanban => rsx! {
                        KanbanBoard {
                            tasks: props.tasks.clone(),
                            on_open: move |id: Uuid| open_id.set(Some(id)),
                            on_set_status: move |(id, status): (Uuid, String)| {
                                props.on_event.call(TaskMutation::SetStatus { id, status });
                            },
                        }
                    },
                }
            }
            if let Some(t) = selected {
                TaskDetail {
                    task: t,
                    on_event: props.on_event,
                    on_close: move |()| open_id.set(None),
                }
            }
        }
    }
}
