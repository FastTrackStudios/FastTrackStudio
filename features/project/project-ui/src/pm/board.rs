//! `TaskKanbanBoard` — horizontal-scrolling column board.
//!
//! Static columns from `column_order`; tasks fall into columns by
//! `group_by`. Quick-create row at the bottom of every column.
//! Right-click on a card opens a `ContextMenu`.
//!
//! FUTURE: drag-to-move between columns (LoroMovableList reorder +
//! status patch). v1 ships read+click only.

use std::collections::HashMap;

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Bot, Calendar as CalendarIcon, Plus, Trash2};
use fts_ui::prelude::*;
use project_proto::{Task, TaskUpdate};
use uuid::Uuid;

use super::common::{status_to_badge_variant, task_id_chip};
use super::list::TaskGroupBy;
use super::priority::PriorityBadge;

fn key_for(task: &Task, by: TaskGroupBy) -> String {
    match by {
        TaskGroupBy::Status => task.status.clone(),
        TaskGroupBy::Priority => {
            if task.priority.trim().is_empty() {
                "none".into()
            } else {
                task.priority.clone()
            }
        }
        TaskGroupBy::Assignee => task.assignee.clone().unwrap_or_else(|| "Unassigned".into()),
        TaskGroupBy::Cycle => task
            .cycle_id
            .map(|c| c.simple().to_string())
            .unwrap_or_else(|| "No cycle".into()),
    }
}

#[component]
fn TaskCard(
    task: Task,
    on_open: EventHandler<Uuid>,
    on_patch: EventHandler<(Uuid, TaskUpdate)>,
    on_delete: EventHandler<Uuid>,
    on_dispatch_agent: EventHandler<Uuid>,
) -> Element {
    let id = task.id;
    let id_chip = task_id_chip(id);
    let title = task.title.clone();
    let priority = task.priority.clone();
    let due = task.due_date;
    let tags = task.tags.clone();
    let assignee = task.assignee.clone();
    let mut menu_open = use_signal(|| false);

    rsx! {
        ContextMenu {
            open: Some(menu_open()),
            on_open_change: move |o: bool| menu_open.set(o),
            ContextMenuTrigger {
                class: "block",
                div {
                    class: "group rounded-lg border border-border bg-card p-3 text-sm shadow-sm hover:border-primary/40 hover:shadow-md transition-all cursor-pointer flex flex-col gap-2",
                    onclick: move |_| on_open.call(id),
                    div { class: "font-medium text-foreground line-clamp-2", "{title}" }
                    HStack { class: "items-center gap-2",
                        span { class: "font-mono text-[10px] text-muted-foreground", "{id_chip}" }
                        PriorityBadge {
                            priority: priority,
                            editable: false,
                            on_change: move |_p: String| {},
                        }
                    }
                    if !tags.is_empty() {
                        div { class: "flex items-center gap-1 flex-wrap",
                            for tag in tags.iter().take(3).cloned() {
                                Badge { key: "{tag}", variant: BadgeVariant::Secondary, "{tag}" }
                            }
                            if tags.len() > 3 {
                                span { class: "text-[10px] text-muted-foreground", "+{tags.len() - 3}" }
                            }
                        }
                    }
                    HStack { class: "items-center justify-between",
                        if let Some(d) = due {
                            {
                                let overdue = d < Utc::now();
                                let cls = if overdue { "text-red-500" } else { "text-muted-foreground" };
                                let label = d.format("%b %-d").to_string();
                                rsx! {
                                    span { class: "inline-flex items-center gap-1 text-[10px] {cls}",
                                        CalendarIcon { size: 10 }
                                        "{label}"
                                    }
                                }
                            }
                        } else {
                            span {}
                        }
                        if let Some(name) = assignee.as_deref() {
                            Avatar { size: AvatarSize::Small,
                                AvatarFallback {
                                    class: "bg-muted text-foreground text-[10px]",
                                    "{name.chars().next().unwrap_or('?').to_uppercase().to_string()}"
                                }
                            }
                        }
                    }
                }
            }
            ContextMenuContent {
                ContextMenuLabel { "Task" }
                ContextMenuItem {
                    value: "open",
                    index: 0usize,
                    on_select: move |_: String| on_open.call(id),
                    "Open"
                }
                ContextMenuSeparator {}
                ContextMenuLabel { "Set status" }
                for (i, s) in [
                    "todo",
                    "in-progress",
                    "in-review",
                    "blocked",
                    "done",
                ].iter().enumerate() {
                    ContextMenuItem {
                        key: "s-{s}",
                        value: s.to_string(),
                        index: 1 + i,
                        on_select: {
                            let s = (*s).to_string();
                            move |_: String| {
                                on_patch
                                    .call((
                                        id,
                                        TaskUpdate { status: Some(s.clone()), ..Default::default() },
                                    ));
                            }
                        },
                        "{s}"
                    }
                }
                ContextMenuSeparator {}
                ContextMenuLabel { "Set priority" }
                for (i, p) in ["urgent", "high", "medium", "low", "none"].iter().enumerate() {
                    ContextMenuItem {
                        key: "p-{p}",
                        value: p.to_string(),
                        index: 6 + i,
                        on_select: {
                            let p = (*p).to_string();
                            move |_: String| {
                                on_patch
                                    .call((
                                        id,
                                        TaskUpdate { priority: Some(p.clone()), ..Default::default() },
                                    ));
                            }
                        },
                        "{p}"
                    }
                }
                ContextMenuSeparator {}
                ContextMenuItem {
                    value: "run-agent",
                    index: 11usize,
                    on_select: move |_: String| on_dispatch_agent.call(id),
                    Bot { size: 14 }
                    "Run with agent"
                }
                ContextMenuSeparator {}
                ContextMenuItem {
                    value: "delete",
                    index: 12usize,
                    destructive: true,
                    on_select: move |_: String| on_delete.call(id),
                    Trash2 { size: 14 }
                    "Delete"
                }
            }
        }
    }
}

#[component]
fn QuickCreate(column_key: String, on_quick_create: EventHandler<(String, String)>) -> Element {
    let mut active = use_signal(|| false);
    let mut draft = use_signal(String::new);
    let key_for_create = column_key.clone();
    let key_for_keydown = column_key.clone();
    rsx! {
        if !*active.read() {
            button {
                r#type: "button",
                class: "w-full rounded-lg border border-dashed border-border/60 px-3 py-2 text-left text-xs text-muted-foreground hover:bg-accent/30 hover:text-foreground transition-colors flex items-center gap-1.5",
                onclick: move |_| {
                    active.set(true);
                },
                Plus { size: 12 }
                "New task"
            }
        } else {
            div { class: "flex flex-col gap-2 rounded-lg border border-primary/40 bg-card p-2",
                Input {
                    value: draft,
                    placeholder: "Task title",
                    size: InputSize::Small,
                    on_change: move |e: FormEvent| {
                        // Submit on Enter via key check on the next blur/Enter wiring is
                        // not available without raw onkeydown — but `oninput` is used
                        // to keep the buffer; commit is via the Add button below.
                        let _ = e;
                    },
                }
                HStack { class: "items-center justify-end gap-1",
                    Button {
                        variant: ButtonVariant::Ghost,
                        size: ButtonSize::Small,
                        on_click: move |_| {
                            active.set(false);
                            draft.set(String::new());
                        },
                        "Cancel"
                    }
                    Button {
                        variant: ButtonVariant::Primary,
                        size: ButtonSize::Small,
                        on_click: {
                            let k = key_for_create.clone();
                            move |_| {
                                let v = draft.read().trim().to_string();
                                if !v.is_empty() {
                                    on_quick_create.call((k.clone(), v));
                                    draft.set(String::new());
                                    active.set(false);
                                }
                            }
                        },
                        Plus { size: 12 }
                        " Add"
                    }
                }
                // Hidden — silences `column_key` unused via referencing in a tag-string.
                div { class: "hidden", "{key_for_keydown}" }
            }
        }
    }
}

#[component]
pub fn TaskKanbanBoard(
    tasks: Vec<Task>,
    group_by: TaskGroupBy,
    column_order: Vec<String>,
    on_card_open: EventHandler<Uuid>,
    on_card_patch: EventHandler<(Uuid, TaskUpdate)>,
    on_quick_create: EventHandler<(String, String)>,
    on_card_delete: EventHandler<Uuid>,
    #[props(default)] on_dispatch_agent: EventHandler<Uuid>,
) -> Element {
    // Bucket tasks by group key. Keys not present in `column_order`
    // get appended to the end so we don't lose data.
    let mut by_col: HashMap<String, Vec<Task>> = HashMap::new();
    for t in &tasks {
        by_col
            .entry(key_for(t, group_by))
            .or_default()
            .push(t.clone());
    }
    for items in by_col.values_mut() {
        items.sort_by(|a, b| {
            a.sort_index
                .cmp(&b.sort_index)
                .then_with(|| a.title.cmp(&b.title))
        });
    }
    let mut ordered: Vec<String> = column_order.clone();
    for k in by_col.keys() {
        if !ordered.iter().any(|x| x == k) {
            ordered.push(k.clone());
        }
    }

    rsx! {
        ScrollArea { class: "w-full",
            div { class: "flex items-start gap-3 px-1 py-2 min-h-[24rem]",
                for col_key in ordered.iter().cloned() {
                    {
                        let items = by_col.remove(&col_key).unwrap_or_default();
                        let count = items.len();
                        rsx! {
                            div { key: "col-{col_key}",
                                class: "group/col flex shrink-0 flex-col gap-2 w-72 rounded-xl bg-muted/30 p-2",
                                HStack { class: "items-center justify-between px-1",
                                    HStack { class: "items-center gap-2",
                                        StatusDot {
                                            color: match status_to_badge_variant(&col_key) {
                                                StatusBadgeVariant::Success => StatusDotColor::Success,
                                                StatusBadgeVariant::Warning => StatusDotColor::Warning,
                                                StatusBadgeVariant::Danger => StatusDotColor::Danger,
                                                StatusBadgeVariant::Neutral => StatusDotColor::Neutral,
                                            },
                                            size: StatusDotSize::Small,
                                        }
                                        span { class: "text-xs font-semibold uppercase tracking-widest text-foreground", "{col_key}" }
                                        Badge { variant: BadgeVariant::Secondary, "{count}" }
                                    }
                                    button {
                                        r#type: "button",
                                        class: "rounded-md p-1 text-muted-foreground hover:text-foreground opacity-0 group-hover/col:opacity-100 transition-opacity",
                                        onclick: {
                                            let k = col_key.clone();
                                            move |_| on_quick_create.call((k.clone(), String::new()))
                                        },
                                        Plus { size: 14 }
                                    }
                                }
                                div { class: "flex flex-col gap-2",
                                    for task in items.into_iter() {
                                        TaskCard {
                                            key: "{task.id}",
                                            task: task,
                                            on_open: move |id| on_card_open.call(id),
                                            on_patch: move |p| on_card_patch.call(p),
                                            on_delete: move |id| on_card_delete.call(id),
                                            on_dispatch_agent: move |id| on_dispatch_agent.call(id),
                                        }
                                    }
                                }
                                QuickCreate {
                                    column_key: col_key.clone(),
                                    on_quick_create: move |t| on_quick_create.call(t),
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
