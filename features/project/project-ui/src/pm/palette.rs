//! `TaskCommandPalette` — Cmd-K dialog with actions + tasks.
//!
//! Pure dumb component: it owns its own internal `search` signal (the
//! `CommandInput` needs a `Signal<String>` to bind to), but the
//! `open` state and the action callbacks are owned by the caller.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    Flag, FolderKanban, LayoutList, LayoutTemplate, Plus, Search as SearchIcon, Target, Users,
};
use fts_ui::prelude::*;
use project_proto::Task;
use uuid::Uuid;

use super::common::task_id_chip;
use super::header::{ProjectTab, TaskView};

/// Side-effect to fire when the user picks an item.
#[derive(Clone, Debug, PartialEq)]
pub enum PaletteIntent {
    CreateTask,
    SwitchView(TaskView),
    OpenTask(Uuid),
    FilterByPriority(String),
    FilterByAssignee(String),
    SetActiveTab(ProjectTab),
}

#[component]
pub fn TaskCommandPalette(
    open: bool,
    tasks: Vec<Task>,
    on_open_change: EventHandler<bool>,
    on_run: EventHandler<PaletteIntent>,
) -> Element {
    let search = use_signal(String::new);

    // Pull unique assignees out of the live task list so the filter
    // suggestions track reality. Cap at 6 to keep the palette short.
    let mut assignees: Vec<String> = tasks.iter().filter_map(|t| t.assignee.clone()).collect();
    assignees.sort();
    assignees.dedup();
    assignees.truncate(6);

    rsx! {
        CommandDialog {
            open,
            on_close: move |_| on_open_change.call(false),
            div { class: "flex items-center gap-2 border-b border-border px-3 py-2",
                SearchIcon { size: 14 }
                CommandInput {
                    value: search,
                    placeholder: "Search tasks, run an action…".to_string(),
                    class: "h-9 flex-1 border-0 bg-transparent text-sm outline-none focus:ring-0".to_string(),
                }
            }

            CommandList {
                CommandEmpty { "No results." }

                CommandGroup { heading: "Actions".to_string(),
                    CommandItem {
                        value: "create task".to_string(),
                        keywords: vec!["new".into(), "add".into()],
                        on_select: move |_| on_run.call(PaletteIntent::CreateTask),
                        Plus { size: 14 }
                        "Create task"
                        CommandShortcut { "C" }
                    }
                    CommandItem {
                        value: "switch view list".to_string(),
                        keywords: vec!["view".into(), "table".into()],
                        on_select: move |_| on_run.call(PaletteIntent::SwitchView(TaskView::List)),
                        LayoutList { size: 14 }
                        "Switch to list view"
                    }
                    CommandItem {
                        value: "switch view board".to_string(),
                        keywords: vec!["view".into(), "kanban".into()],
                        on_select: move |_| on_run.call(PaletteIntent::SwitchView(TaskView::Board)),
                        LayoutTemplate { size: 14 }
                        "Switch to board view"
                    }
                    CommandItem {
                        value: "tab overview".to_string(),
                        keywords: vec!["jump".into()],
                        on_select: move |_| on_run.call(PaletteIntent::SetActiveTab(ProjectTab::Overview)),
                        FolderKanban { size: 14 }
                        "Open Overview"
                    }
                    CommandItem {
                        value: "tab tasks".to_string(),
                        keywords: vec!["jump".into()],
                        on_select: move |_| on_run.call(PaletteIntent::SetActiveTab(ProjectTab::Tasks)),
                        LayoutList { size: 14 }
                        "Open Tasks"
                    }
                    CommandItem {
                        value: "tab cycles".to_string(),
                        keywords: vec!["jump".into(), "sprint".into()],
                        on_select: move |_| on_run.call(PaletteIntent::SetActiveTab(ProjectTab::Cycles)),
                        Target { size: 14 }
                        "Open Cycles"
                    }
                    CommandItem {
                        value: "tab milestones".to_string(),
                        keywords: vec!["jump".into()],
                        on_select: move |_| on_run.call(PaletteIntent::SetActiveTab(ProjectTab::Milestones)),
                        Flag { size: 14 }
                        "Open Milestones"
                    }
                }

                CommandSeparator {}

                CommandGroup { heading: "Filter by priority".to_string(),
                    for p in ["urgent", "high", "medium", "low", "none"].iter() {
                        CommandItem {
                            key: "p-{p}",
                            value: format!("priority {p}"),
                            on_select: {
                                let p = (*p).to_string();
                                move |_| on_run.call(PaletteIntent::FilterByPriority(p.clone()))
                            },
                            Flag { size: 14 }
                            "Priority: {p}"
                        }
                    }
                }

                if !assignees.is_empty() {
                    CommandGroup { heading: "Filter by assignee".to_string(),
                        for a in assignees.into_iter() {
                            {
                                let a_for_select = a.clone();
                                rsx! {
                                    CommandItem {
                                        key: "a-{a}",
                                        value: format!("assignee {a}"),
                                        on_select: move |_| {
                                            on_run.call(PaletteIntent::FilterByAssignee(a_for_select.clone()));
                                        },
                                        Users { size: 14 }
                                        "Assignee: {a}"
                                    }
                                }
                            }
                        }
                    }
                }

                CommandSeparator {}

                CommandGroup { heading: "Tasks".to_string(),
                    for task in tasks.iter().take(50).cloned() {
                        {
                            let task_id = task.id;
                            let chip = task_id_chip(task_id);
                            let title = task.title.clone();
                            let status = task.status.clone();
                            let tags = task.tags.join(" ");
                            rsx! {
                                CommandItem {
                                    key: "{task.id}",
                                    value: format!("{} {} {} {}", chip, title, status, tags),
                                    keywords: vec![chip.clone(), status.clone()],
                                    on_select: move |_| on_run.call(PaletteIntent::OpenTask(task_id)),
                                    span { class: "font-mono text-[10px] text-muted-foreground", "{chip}" }
                                    span { class: "truncate", "{title}" }
                                    CommandShortcut { "{status}" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
