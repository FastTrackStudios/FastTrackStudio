//! Right-side detail sheet — edit title, status, priority,
//! due date. Save emits an `Update` mutation; per-field
//! checkbox / status flips emit the cheaper `SetStatus` /
//! `SetPriority` paths.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Trash2, X};
use fts_ui::prelude::*;

use crate::TaskInfo;
use crate::TaskMutation;
use crate::model::{Priority, Status};

#[derive(Props, Clone, PartialEq)]
pub struct TaskDetailProps {
    pub task: TaskInfo,
    pub on_event: EventHandler<TaskMutation>,
    pub on_close: EventHandler<()>,
}

#[component]
pub fn TaskDetail(props: TaskDetailProps) -> Element {
    let initial = props.task.clone();
    let mut title = use_signal(|| initial.title.clone());
    let mut due = use_signal(|| initial.due.clone().unwrap_or_default());
    let mut details = use_signal(|| initial.details.clone());
    let id = initial.id;
    let current_status = initial.status_enum();
    let current_priority = initial.priority_enum();

    rsx! {
        // Tap-to-close backdrop — essential on mobile where the sheet
        // is full-width; a subtle scrim on desktop.
        button {
            r#type: "button",
            class: "fixed inset-0 z-30 bg-foreground/20 backdrop-blur-[1px]",
            "aria-label": "Close task detail",
            onclick: move |_| props.on_close.call(()),
        }
        // Right sheet: full-width on phones (capped at 28rem), a fixed
        // 28rem panel on larger screens. Scrolls when fields overflow.
        aside { class: "fixed inset-y-0 right-0 z-40 flex h-screen w-full max-w-[28rem] flex-col gap-3 overflow-y-auto border-l border-border bg-background p-4 shadow-2xl sm:w-[28rem]",
            div { class: "flex items-center justify-between",
                span { class: "text-xs uppercase tracking-wider text-muted-foreground", "Task" }
                div { class: "flex items-center gap-1",
                    button {
                        r#type: "button",
                        class: "inline-flex h-7 w-7 items-center justify-center rounded-md text-muted-foreground hover:bg-accent hover:text-foreground",
                        title: "Delete",
                        onclick: move |_| {
                            props.on_event.call(TaskMutation::Delete { id });
                            props.on_close.call(());
                        },
                        Trash2 { size: 14 }
                    }
                    button {
                        r#type: "button",
                        class: "inline-flex h-7 w-7 items-center justify-center rounded-md text-muted-foreground hover:bg-accent hover:text-foreground",
                        onclick: move |_| props.on_close.call(()),
                        X { size: 14 }
                    }
                }
            }
            input {
                r#type: "text",
                class: "w-full bg-transparent text-lg font-medium text-foreground outline-none border-b border-border focus:border-primary py-1",
                value: "{title}",
                oninput: move |e| title.set(e.value()),
            }
            div { class: "flex flex-col gap-2",
                FieldLabel { label: "Status" }
                div { class: "flex flex-wrap gap-1",
                    for s in [Status::Open, Status::InProgress, Status::Waiting, Status::Done, Status::Cancelled] {
                        {
                            let active = s == current_status;
                            let cls = if active {
                                "rounded-md border border-primary bg-primary/15 text-foreground px-2.5 py-1 text-xs font-medium"
                            } else {
                                "rounded-md border border-border text-muted-foreground hover:text-foreground hover:bg-accent px-2.5 py-1 text-xs"
                            };
                            rsx! {
                                button {
                                    key: "{s.as_str()}",
                                    r#type: "button",
                                    class: "{cls}",
                                    onclick: move |_| props.on_event.call(TaskMutation::SetStatus { id, status: s.as_str().to_string() }),
                                    "{s.label()}"
                                }
                            }
                        }
                    }
                }
            }
            div { class: "flex flex-col gap-2",
                FieldLabel { label: "Priority" }
                div { class: "flex flex-wrap gap-1",
                    for p in [Priority::None, Priority::Low, Priority::Normal, Priority::High, Priority::Critical] {
                        {
                            let active = p == current_priority;
                            let cls = if active {
                                "rounded-md border border-primary bg-primary/15 text-foreground px-2.5 py-1 text-xs font-medium"
                            } else {
                                "rounded-md border border-border text-muted-foreground hover:text-foreground hover:bg-accent px-2.5 py-1 text-xs"
                            };
                            rsx! {
                                button {
                                    key: "{p.as_str()}",
                                    r#type: "button",
                                    class: "{cls}",
                                    onclick: move |_| props.on_event.call(TaskMutation::SetPriority { id, priority: p.as_str().to_string() }),
                                    "{p.label()}"
                                }
                            }
                        }
                    }
                }
            }
            div { class: "flex flex-col gap-2",
                FieldLabel { label: "Due date" }
                input {
                    r#type: "date",
                    class: "rounded-md border border-border bg-card px-2 py-1.5 text-sm text-foreground outline-none focus:border-primary",
                    value: "{due}",
                    oninput: move |e| due.set(e.value()),
                }
            }
            if !initial.contexts.is_empty() || !initial.projects.is_empty() {
                div { class: "flex flex-col gap-2",
                    FieldLabel { label: "Tags" }
                    div { class: "flex flex-wrap gap-1",
                        for c in initial.contexts.iter() {
                            span {
                                key: "{c}",
                                class: "rounded-full bg-muted/50 px-2 py-0.5 text-[11px] text-muted-foreground",
                                "@{c}"
                            }
                        }
                        for p in initial.projects.iter() {
                            span {
                                key: "{p}",
                                class: "rounded-full bg-violet-900/30 px-2 py-0.5 text-[11px] text-violet-200",
                                "{p.trim_start_matches(\"[[\").trim_end_matches(\"]]\")}"
                            }
                        }
                    }
                }
            }
            div { class: "flex flex-col gap-2 flex-1 min-h-0",
                FieldLabel { label: "Notes" }
                textarea {
                    class: "flex-1 min-h-0 resize-none rounded-md border border-border bg-card px-2 py-1.5 text-sm text-foreground outline-none focus:border-primary",
                    placeholder: "Notes…",
                    value: "{details}",
                    oninput: move |e| details.set(e.value()),
                }
            }
            div { class: "flex items-center justify-end gap-2 pt-2 border-t border-border",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| props.on_close.call(()),
                    "Cancel"
                }
                Button {
                    size: ButtonSize::Small,
                    on_click: move |_| {
                        let mut next = initial.clone();
                        next.title = title.read().clone();
                        let d = due.read().clone();
                        next.due = if d.is_empty() { None } else { Some(d) };
                        next.details = details.read().clone();
                        props.on_event.call(TaskMutation::Update { task: next });
                        props.on_close.call(());
                    },
                    "Save"
                }
            }
        }
    }
}

#[component]
fn FieldLabel(label: &'static str) -> Element {
    rsx! {
        span { class: "text-[10px] uppercase tracking-wider text-muted-foreground", "{label}" }
    }
}
