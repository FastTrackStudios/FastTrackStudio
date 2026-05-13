//! `TaskListView` — Linear / GitHub-Projects-v2 style task list.
//!
//! Pure presentation: data + callbacks in. Multi-select state is owned
//! by the caller (so the bulk action bar can survive re-renders, and
//! external "select all" buttons can drive the same set).

use std::collections::{BTreeMap, HashSet};

use chrono::Utc;
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    Calendar as CalendarIcon, ChevronDown, ChevronRight, Circle, CircleCheck, FolderKanban,
    SquareDashed, Trash2,
};
use fts_ui::prelude::*;
use project_proto::{Task, TaskUpdate};
use uuid::Uuid;

use super::common::{status_to_badge_variant, task_id_chip};
use super::priority::PriorityBadge;

/// Grouping axis for the list / board.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TaskGroupBy {
    Status,
    Priority,
    Assignee,
    Cycle,
}

/// Side-effect a multi-select bar can trigger. Caller fans this out
/// to its repo / service.
#[derive(Clone, Debug, PartialEq)]
pub enum BulkAction {
    Delete,
    SetStatus(String),
    SetPriority(String),
    SetAssignee(Option<String>),
}

fn group_key(task: &Task, by: TaskGroupBy) -> String {
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
fn StatusIcon(status: String) -> Element {
    let cls = match status.as_str() {
        "done" | "completed" => "text-green-500",
        "in-progress" | "in-review" => "text-yellow-500",
        "blocked" | "cancelled" => "text-red-500",
        _ => "text-muted-foreground",
    };
    rsx! {
        span { class: "inline-flex items-center justify-center {cls}",
            match status.as_str() {
                "done" | "completed" => rsx! { CircleCheck { size: 14 } },
                _ => rsx! { Circle { size: 14 } },
            }
        }
    }
}

#[component]
fn DuePill(due: chrono::DateTime<chrono::Utc>) -> Element {
    let now = Utc::now();
    let overdue = due < now;
    let cls = if overdue {
        "text-red-500 bg-red-500/10"
    } else {
        "text-muted-foreground bg-muted"
    };
    let label = due.format("%b %-d").to_string();
    rsx! {
        span { class: "inline-flex items-center gap-1 rounded-full px-2 py-0.5 text-[10px] font-medium {cls}",
            CalendarIcon { size: 10 }
            "{label}"
        }
    }
}

/// Multi-task list view. Sorting is by `sort_index` ascending then
/// `title`; grouping is collapsible per-group via a local signal map.
#[component]
pub fn TaskListView(
    tasks: Vec<Task>,
    group_by: Option<TaskGroupBy>,
    selected: HashSet<Uuid>,
    on_select_change: EventHandler<HashSet<Uuid>>,
    on_open: EventHandler<Uuid>,
    on_inline_edit: EventHandler<(Uuid, TaskUpdate)>,
    on_bulk_action: EventHandler<BulkAction>,
) -> Element {
    let mut collapsed: Signal<HashSet<String>> = use_signal(HashSet::new);

    if tasks.is_empty() {
        return rsx! {
            EmptyState {
                message: "No tasks here yet.",
                icon: rsx! { FolderKanban { size: 32 } },
            }
        };
    }

    // Group + sort.
    let groups: Vec<(String, Vec<Task>)> = {
        let mut buckets: BTreeMap<String, Vec<Task>> = BTreeMap::new();
        let key_fn = group_by;
        for t in &tasks {
            let k = key_fn.map(|g| group_key(t, g)).unwrap_or_default();
            buckets.entry(k).or_default().push(t.clone());
        }
        let mut v: Vec<(String, Vec<Task>)> = buckets.into_iter().collect();
        for (_, list) in v.iter_mut() {
            list.sort_by(|a, b| {
                a.sort_index
                    .cmp(&b.sort_index)
                    .then_with(|| a.title.cmp(&b.title))
            });
        }
        v
    };

    let sel_count = selected.len();
    let selected_for_toggle_all = selected.clone();
    let all_ids: HashSet<Uuid> = tasks.iter().map(|t| t.id).collect();
    let all_selected = !all_ids.is_empty() && all_ids.iter().all(|id| selected.contains(id));

    rsx! {
        div { class: "flex flex-col gap-2",
            // Header row — column labels.
            div {
                class: "grid grid-cols-[24px_18px_64px_24px_1fr_auto_auto_auto_48px] items-center gap-2 px-2 py-1 text-[10px] uppercase tracking-widest text-muted-foreground border-b border-border",
                {
                    let sel = use_signal(|| all_selected);
                    let all_ids_inner = all_ids.clone();
                    rsx! {
                        Checkbox {
                            checked: sel,
                            on_change: move |v: bool| {
                                if v {
                                    on_select_change.call(all_ids_inner.clone());
                                } else {
                                    on_select_change.call(HashSet::new());
                                }
                            },
                        }
                    }
                }
                span {}
                span { "ID" }
                span {}
                span { "Title" }
                span { "Tags" }
                span { "Due" }
                span { "Assignee" }
                span { "Est." }
            }

            for (gkey, items) in groups.iter().cloned() {
                {
                    let gkey_clone = gkey.clone();
                    let is_collapsed = collapsed.read().contains(&gkey);
                    let count = items.len();
                    rsx! {
                        // Group header (only when grouping)
                        if group_by.is_some() {
                            button {
                                key: "g-{gkey}",
                                r#type: "button",
                                class: "flex items-center gap-2 w-full rounded-md px-2 py-1 text-left text-xs font-semibold text-foreground hover:bg-accent/40 transition-colors",
                                onclick: {
                                    let gkey_inner = gkey_clone.clone();
                                    move |_| {
                                        let mut next = collapsed.read().clone();
                                        if next.contains(&gkey_inner) {
                                            next.remove(&gkey_inner);
                                        } else {
                                            next.insert(gkey_inner.clone());
                                        }
                                        collapsed.set(next);
                                    }
                                },
                                if is_collapsed { ChevronRight { size: 12 } } else { ChevronDown { size: 12 } }
                                StatusBadge {
                                    variant: status_to_badge_variant(&gkey),
                                    label: gkey.clone(),
                                }
                                span { class: "text-muted-foreground", "{count}" }
                            }
                        }

                        if !is_collapsed {
                            for task in items.into_iter() {
                                {
                                    let id = task.id;
                                    let is_sel = selected.contains(&id);
                                    let row_check = use_signal(|| is_sel);
                                    let title = task.title.clone();
                                    let status_label = task.status.clone();
                                    let priority = task.priority.clone();
                                    let id_chip = task_id_chip(id);
                                    let assignee = task.assignee.clone();
                                    let estimate = task
                                        .estimate_minutes
                                        .map(|m| {
                                            if m >= 60 {
                                                format!("{:.1}h", m as f64 / 60.0)
                                            } else {
                                                format!("{}m", m)
                                            }
                                        })
                                        .unwrap_or_default();
                                    let due = task.due_date;
                                    let tags = task.tags.clone();
                                    let selected_for_row = selected_for_toggle_all.clone();
                                    rsx! {
                                        div {
                                            key: "{id}",
                                            class: "group grid grid-cols-[24px_18px_64px_24px_1fr_auto_auto_auto_48px] items-center gap-2 rounded-md px-2 py-2 min-h-[36px] hover:bg-accent/30 transition-colors cursor-pointer",
                                            onclick: move |_| on_open.call(id),
                                            div {
                                                onclick: move |e: MouseEvent| e.stop_propagation(),
                                                Checkbox {
                                                    checked: row_check,
                                                    on_change: {
                                                        let selected_for_row = selected_for_row.clone();
                                                        move |v: bool| {
                                                            let mut next = selected_for_row.clone();
                                                            if v {
                                                                next.insert(id);
                                                            } else {
                                                                next.remove(&id);
                                                            }
                                                            on_select_change.call(next);
                                                        }
                                                    },
                                                }
                                            }
                                            StatusIcon { status: status_label.clone() }
                                            span { class: "font-mono text-[10px] text-muted-foreground truncate", "{id_chip}" }
                                            div {
                                                onclick: move |e: MouseEvent| e.stop_propagation(),
                                                PriorityBadge {
                                                    priority: priority.clone(),
                                                    editable: false,
                                                    on_change: move |_p: String| {},
                                                }
                                            }
                                            div {
                                                class: "min-w-0",
                                                onclick: move |e: MouseEvent| e.stop_propagation(),
                                                InlineEdit {
                                                    value: title.clone(),
                                                    on_commit: {
                                                        let on_inline_edit = on_inline_edit;
                                                        move |new: String| {
                                                            on_inline_edit
                                                                .call((
                                                                    id,
                                                                    TaskUpdate {
                                                                        title: Some(new),
                                                                        ..Default::default()
                                                                    },
                                                                ));
                                                        }
                                                    },
                                                    class: "text-sm truncate".to_string(),
                                                }
                                            }
                                            div { class: "flex items-center gap-1 flex-wrap max-w-[10rem]",
                                                for tag in tags.iter().take(2).cloned() {
                                                    Badge {
                                                        key: "{tag}",
                                                        variant: BadgeVariant::Secondary,
                                                        "{tag}"
                                                    }
                                                }
                                                if tags.len() > 2 {
                                                    span { class: "text-[10px] text-muted-foreground", "+{tags.len() - 2}" }
                                                }
                                            }
                                            div {
                                                if let Some(d) = due {
                                                    DuePill { due: d }
                                                }
                                            }
                                            div {
                                                if let Some(name) = assignee.as_deref() {
                                                    Avatar {
                                                        size: AvatarSize::Small,
                                                        AvatarFallback {
                                                            class: "bg-muted text-foreground text-[10px]".to_string(),
                                                            "{name.chars().next().unwrap_or('?').to_uppercase().to_string()}"
                                                        }
                                                    }
                                                }
                                            }
                                            span { class: "text-[10px] text-muted-foreground text-right tabular-nums", "{estimate}" }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Bulk action bar
            if sel_count > 0 {
                div { class: "sticky bottom-3 z-10 mt-3 flex justify-center",
                    Card { class: "shadow-lg border-primary/30".to_string(),
                        CardContent { class: "flex items-center gap-3 py-2 px-3".to_string(),
                            SquareDashed { size: 14 }
                            span { class: "text-sm font-medium", "{sel_count} selected" }
                            Divider { orientation: DividerOrientation::Vertical }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| on_bulk_action.call(BulkAction::SetStatus("done".into())),
                                "Mark done"
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| on_bulk_action.call(BulkAction::SetPriority("high".into())),
                                "Set high"
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| on_bulk_action.call(BulkAction::SetAssignee(None)),
                                "Unassign"
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| on_bulk_action.call(BulkAction::Delete),
                                Trash2 { size: 14 }
                                " Delete"
                            }
                            Button {
                                variant: ButtonVariant::Ghost,
                                size: ButtonSize::Small,
                                on_click: move |_| on_select_change.call(HashSet::new()),
                                "Clear"
                            }
                        }
                    }
                }
            }
        }
    }
}
