//! One row in the list view. Checkbox · title · due pill ·
//! priority badge · context / project chips.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Check, Hash};
use uuid::Uuid;

use crate::TaskInfo;
use crate::model::{Priority, Status};

use super::palette::{priority_pill, status_pill};

#[derive(Props, Clone, PartialEq)]
pub struct TaskRowProps {
    pub task: TaskInfo,
    pub on_toggle: EventHandler<Uuid>,
    pub on_open: EventHandler<Uuid>,
}

#[component]
pub fn TaskRow(props: TaskRowProps) -> Element {
    let t = props.task.clone();
    let id = t.id;
    let done = t.is_done();
    let status = t.status_enum();
    let priority = t.priority_enum();

    let title_cls = if done {
        "text-sm text-muted-foreground line-through truncate"
    } else {
        "text-sm text-foreground truncate"
    };

    rsx! {
        div {
            class: "group flex min-h-[44px] items-center gap-3 rounded-md border border-transparent px-2 py-2.5 hover:border-border hover:bg-accent/30 cursor-pointer sm:min-h-0 sm:py-2",
            onclick: move |_| props.on_open.call(id),
            CheckboxButton {
                done,
                priority,
                on_click: move |()| props.on_toggle.call(id),
            }
            div { class: "flex-1 min-w-0 flex flex-col gap-0.5",
                div { class: "flex items-center gap-2 min-w-0",
                    span { class: "{title_cls}", "{t.title}" }
                }
                {
                    let has_meta = t.due.is_some()
                        || !t.contexts.is_empty()
                        || !t.projects.is_empty()
                        || priority != Priority::Normal
                        || (status != Status::Open && status != Status::Done);
                    if has_meta {
                        rsx! {
                            div { class: "flex items-center gap-1.5 flex-wrap",
                                if let Some(d) = t.due_date() {
                                    {
                                        let (label, cls) = due_pill(d, done);
                                        rsx! {
                                            span { class: "{cls}", "{label}" }
                                        }
                                    }
                                }
                                if priority != Priority::Normal {
                                    span {
                                        class: "inline-flex items-center rounded-full px-1.5 py-0 text-[10px] uppercase tracking-wider {priority_pill(priority)}",
                                        "{priority.label()}"
                                    }
                                }
                                if status != Status::Open && status != Status::Done {
                                    span {
                                        class: "inline-flex items-center rounded-full px-1.5 py-0 text-[10px] uppercase tracking-wider {status_pill(status)}",
                                        "{status.label()}"
                                    }
                                }
                                for c in t.contexts.iter() {
                                    span {
                                        key: "{c}",
                                        class: "inline-flex items-center gap-0.5 rounded-full bg-muted/50 px-1.5 py-0 text-[10px] text-muted-foreground",
                                        "@{c}"
                                    }
                                }
                                for p in t.projects.iter() {
                                    span {
                                        key: "{p}",
                                        class: "inline-flex items-center gap-0.5 rounded-full bg-violet-900/30 px-1.5 py-0 text-[10px] text-violet-200",
                                        Hash { size: 9 }
                                        "{strip_wikilink(p)}"
                                    }
                                }
                            }
                        }
                    } else {
                        rsx! {}
                    }
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct CheckboxButtonProps {
    done: bool,
    priority: Priority,
    on_click: EventHandler<()>,
}

#[component]
fn CheckboxButton(props: CheckboxButtonProps) -> Element {
    let cls = if props.done {
        "flex h-6 w-6 sm:h-5 sm:w-5 items-center justify-center rounded-md border-2 bg-emerald-500 border-emerald-500 text-white shrink-0"
    } else {
        let edge = match props.priority {
            Priority::Critical => "border-rose-500",
            Priority::High => "border-amber-500",
            _ => "border-border",
        };
        let base = "flex h-6 w-6 sm:h-5 sm:w-5 items-center justify-center rounded-md border-2 bg-transparent hover:bg-accent/30 shrink-0";
        return rsx! {
            button {
                r#type: "button",
                class: "{base} {edge}",
                onclick: move |e: MouseEvent| {
                    e.stop_propagation();
                    props.on_click.call(());
                },
            }
        };
    };
    rsx! {
        button {
            r#type: "button",
            class: "{cls}",
            onclick: move |e: MouseEvent| {
                e.stop_propagation();
                props.on_click.call(());
            },
            Check { size: 12 }
        }
    }
}

/// Convert a `[[Wikilink]]` into the bare page name.
fn strip_wikilink(s: &str) -> String {
    s.trim_start_matches("[[")
        .trim_end_matches("]]")
        .to_string()
}

/// Coloring for the due-date pill — red when overdue, amber for
/// today, sky for upcoming, muted once done.
fn due_pill(d: chrono::NaiveDate, done: bool) -> (String, &'static str) {
    let today = chrono::Local::now().date_naive();
    let label = if d == today {
        "Today".to_string()
    } else if d == today + chrono::Duration::days(1) {
        "Tomorrow".to_string()
    } else if d == today - chrono::Duration::days(1) {
        "Yesterday".to_string()
    } else {
        d.format("%b %-d").to_string()
    };
    let cls = if done {
        "inline-flex items-center rounded-full px-1.5 py-0 text-[10px] text-muted-foreground border border-border"
    } else if d < today {
        "inline-flex items-center rounded-full px-1.5 py-0 text-[10px] text-rose-100 bg-rose-700/50 border border-rose-500"
    } else if d == today {
        "inline-flex items-center rounded-full px-1.5 py-0 text-[10px] text-amber-50 bg-amber-700/60 border border-amber-400 font-medium"
    } else if d <= today + chrono::Duration::days(7) {
        "inline-flex items-center rounded-full px-1.5 py-0 text-[10px] text-sky-100 bg-sky-700/40 border border-sky-500"
    } else {
        "inline-flex items-center rounded-full px-1.5 py-0 text-[10px] text-muted-foreground border border-border"
    };
    (label, cls)
}
