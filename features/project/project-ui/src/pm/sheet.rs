//! `TaskDetailSheet` — right slide-over inspector / editor.
//!
//! Mounted by the page-level state. When `task` is `Some(_)` we render
//! the fts-ui `Sheet`; when `None`, nothing. All field edits emit a
//! `TaskUpdate` patch that only sets the field the user touched.

use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Copy as CopyIcon, Plus, Trash2, X};
use fts_ui::prelude::*;
use project_proto::{Cycle, Milestone, Task, TaskUpdate};
use uuid::Uuid;

use super::common::{status_to_badge_variant, task_id_chip};
use super::git::TaskGitPanel;
use super::priority::PriorityBadge;

const STATUSES: &[&str] = &[
    "todo",
    "in-progress",
    "in-review",
    "blocked",
    "done",
    "cancelled",
];

#[component]
pub fn TaskDetailSheet(
    task: Option<Task>,
    subtasks: Vec<Task>,
    available_cycles: Vec<Cycle>,
    available_milestones: Vec<Milestone>,
    available_assignees: Vec<String>,
    on_close: EventHandler<()>,
    on_patch: EventHandler<(Uuid, TaskUpdate)>,
    on_subtask_add: EventHandler<String>,
    on_delete: EventHandler<Uuid>,
    on_duplicate: EventHandler<Uuid>,
    // ── Git extension ─────────────────────────────────────────────
    #[props(default)] on_set_branch_name: EventHandler<String>,
    #[props(default)] on_clear_branch_name: EventHandler<()>,
) -> Element {
    let open = task.is_some();
    let mut confirm_open = use_signal(|| false);
    let mut active_tab = use_signal(|| "comments".to_string());
    let new_subtask = use_signal(String::new);

    // Field-local signals are reconstructed each render based on `task` —
    // this keeps the inputs in sync when the caller swaps to a different
    // task without the sheet remounting.
    let task_ref = task.clone();

    rsx! {
        Sheet {
            open,
            on_close: move |_| on_close.call(()),
            // Width override — spec asks ~560px.
            class: "sm:max-w-[36rem] w-[36rem] flex flex-col h-full",
            if let Some(t) = task_ref {
                {
                    let task_id = t.id;
                    let id_chip = task_id_chip(task_id);
                    let title_init = t.title.clone();
                    let desc_signal = use_signal(|| t.description.clone().unwrap_or_default());
                    let tags_signal = use_signal(|| t.tags.join(", "));
                    let estimate_signal = use_signal(|| t.estimate_minutes.map(|m| m.to_string()).unwrap_or_default());
                    let assignee_signal = use_signal(|| t.assignee.clone().unwrap_or_default());
                    let status_signal = use_signal(|| t.status.clone());
                    let priority_str = t.priority.clone();
                    let cycle_signal = use_signal(|| t.cycle_id.map(|c| c.simple().to_string()).unwrap_or_default());
                    let status_for_badge = t.status.clone();
                    rsx! {
                        SheetHeader {
                            HStack { class: "items-center gap-2 flex-wrap",
                                StatusBadge {
                                    variant: status_to_badge_variant(&status_for_badge),
                                    label: status_for_badge.clone(),
                                }
                                span { class: "font-mono text-[10px] text-muted-foreground", "{id_chip}" }
                            }
                            SheetTitle {
                                InlineEdit {
                                    value: title_init,
                                    on_commit: move |new: String| {
                                        on_patch
                                            .call((
                                                task_id,
                                                TaskUpdate { title: Some(new), ..Default::default() },
                                            ));
                                    },
                                    placeholder: "Task title",
                                    class: "text-base font-semibold",
                                }
                            }
                        }

                        // Body — scrollable.
                        div { class: "flex-1 overflow-y-auto px-1 py-4 flex flex-col gap-5",
                            // Two-column: description+tabs / properties
                            div { class: "grid grid-cols-1 md:grid-cols-[1fr_18rem] gap-4",
                                // Left column
                                div { class: "flex flex-col gap-3 min-w-0",
                                    div {
                                        Label { "Description" }
                                        {
                                            let desc_signal = desc_signal;
                                            rsx! {
                                                Textarea {
                                                    value: desc_signal,
                                                    placeholder: "Add a description…",
                                                    rows: 5u32,
                                                    on_change: move |_| {
                                                        let v = desc_signal.read().clone();
                                                        on_patch
                                                            .call((
                                                                task_id,
                                                                TaskUpdate {
                                                                    description: Some(if v.is_empty() { None } else { Some(v) }),
                                                                    ..Default::default()
                                                                },
                                                            ));
                                                    },
                                                }
                                            }
                                        }
                                    }

                                    Tabs {
                                        value: Some(active_tab.read().clone()),
                                        on_change: move |v: String| active_tab.set(v),
                                        TabList {
                                            TabTrigger { value: "comments", index: 0usize, "Comments" }
                                            TabTrigger { value: "activity", index: 2usize, "Activity" }
                                            TabTrigger { value: "subtasks", index: 3usize, "Subtasks" }
                                            TabTrigger { value: "git", index: 4usize, "Git" }
                                        }
                                        TabContent { value: "comments", index: 0usize,
                                            div { class: "py-4 text-xs text-muted-foreground", "Comments coming soon." }
                                        }
                                        TabContent { value: "activity", index: 2usize,
                                            div { class: "py-4 text-xs text-muted-foreground", "Activity log coming soon." }
                                        }
                                        TabContent { value: "git", index: 4usize,
                                            TaskGitPanel {
                                                task: t.clone(),
                                                on_set_branch_name: move |s| on_set_branch_name.call(s),
                                                on_clear_branch_name: move |_| on_clear_branch_name.call(()),
                                            }
                                        }
                                        TabContent { value: "subtasks", index: 3usize,
                                            div { class: "flex flex-col gap-2 py-3",
                                                for sub in subtasks.iter().cloned() {
                                                    {
                                                        let st_status = sub.status.clone();
                                                        let st_id = task_id_chip(sub.id);
                                                        rsx! {
                                                            div { key: "{sub.id}",
                                                                class: "flex items-center gap-2 rounded-md px-2 py-1 hover:bg-accent/30",
                                                                StatusBadge {
                                                                    variant: status_to_badge_variant(&st_status),
                                                                    label: st_status.clone(),
                                                                }
                                                                span { class: "font-mono text-[10px] text-muted-foreground", "{st_id}" }
                                                                span { class: "text-sm truncate flex-1", "{sub.title}" }
                                                            }
                                                        }
                                                    }
                                                }
                                                if subtasks.is_empty() {
                                                    div { class: "text-xs text-muted-foreground", "No subtasks yet." }
                                                }
                                                {
                                                    let mut new_sub_inner = new_subtask;
                                                    rsx! {
                                                        HStack { class: "items-center gap-2",
                                                            Input {
                                                                value: new_sub_inner,
                                                                placeholder: "Add a subtask and press +",
                                                                size: InputSize::Small,
                                                            }
                                                            Button {
                                                                variant: ButtonVariant::Outline,
                                                                size: ButtonSize::Small,
                                                                on_click: move |_| {
                                                                    let v = new_sub_inner.read().trim().to_string();
                                                                    if !v.is_empty() {
                                                                        on_subtask_add.call(v);
                                                                        new_sub_inner.set(String::new());
                                                                    }
                                                                },
                                                                Plus { size: 14 }
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }

                                // Right column — properties
                                div { class: "flex flex-col gap-3",
                                    SectionHeader { label: "Properties", size: SectionHeaderSize::Small }
                                    div { class: "flex flex-col gap-1",
                                        Label { "Status" }
                                        {
                                            let status_signal = status_signal;
                                            rsx! {
                                                NativeSelect {
                                                    value: status_signal,
                                                    on_change: move |_| {
                                                        let v = status_signal.read().clone();
                                                        on_patch
                                                            .call((
                                                                task_id,
                                                                TaskUpdate { status: Some(v), ..Default::default() },
                                                            ));
                                                    },
                                                    for s in STATUSES.iter() {
                                                        NativeSelectOption { key: "{s}", value: s.to_string(), "{s}" }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    div { class: "flex flex-col gap-1",
                                        Label { "Priority" }
                                        PriorityBadge {
                                            priority: priority_str,
                                            show_label: true,
                                            editable: true,
                                            on_change: move |p: String| {
                                                on_patch
                                                    .call((
                                                        task_id,
                                                        TaskUpdate { priority: Some(p), ..Default::default() },
                                                    ));
                                            },
                                        }
                                    }
                                    div { class: "flex flex-col gap-1",
                                        Label { "Assignee" }
                                        {
                                            let assignee_signal = assignee_signal;
                                            let assignees = available_assignees.clone();
                                            rsx! {
                                                NativeSelect {
                                                    value: assignee_signal,
                                                    on_change: move |_| {
                                                        let v = assignee_signal.read().clone();
                                                        on_patch
                                                            .call((
                                                                task_id,
                                                                TaskUpdate {
                                                                    assignee: Some(if v.is_empty() { None } else { Some(v) }),
                                                                    ..Default::default()
                                                                },
                                                            ));
                                                    },
                                                    NativeSelectOption { value: "", "Unassigned" }
                                                    for a in assignees.into_iter() {
                                                        NativeSelectOption { key: "{a}", value: a.clone(), "{a}" }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    div { class: "flex flex-col gap-1",
                                        Label { "Cycle" }
                                        {
                                            let cycle_signal = cycle_signal;
                                            let cycles = available_cycles.clone();
                                            rsx! {
                                                NativeSelect {
                                                    value: cycle_signal,
                                                    on_change: move |_| {
                                                        let v = cycle_signal.read().clone();
                                                        let parsed = if v.is_empty() {
                                                            None
                                                        } else {
                                                            Uuid::parse_str(&v).ok()
                                                        };
                                                        on_patch
                                                            .call((
                                                                task_id,
                                                                TaskUpdate { cycle_id: Some(parsed), ..Default::default() },
                                                            ));
                                                    },
                                                    NativeSelectOption { value: "", "No cycle" }
                                                    for c in cycles.into_iter() {
                                                        NativeSelectOption {
                                                            key: "{c.id}",
                                                            value: c.id.simple().to_string(),
                                                            "{c.name}"
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    if !available_milestones.is_empty() {
                                        div { class: "flex justify-between items-center text-xs",
                                            span { class: "text-muted-foreground", "Milestone" }
                                            span { "{available_milestones.len()} available" }
                                        }
                                    }
                                    {
                                        let due_label = t
                                            .due_date
                                            .map(|d| d.format("%b %-d, %Y").to_string())
                                            .unwrap_or_else(|| "—".to_string());
                                        rsx! {
                                            div { class: "flex justify-between items-center text-xs",
                                                span { class: "text-muted-foreground", "Due" }
                                                span { "{due_label}" }
                                            }
                                        }
                                    }
                                    div { class: "flex flex-col gap-1",
                                        Label { "Estimate (minutes)" }
                                        {
                                            let estimate_signal = estimate_signal;
                                            rsx! {
                                                Input {
                                                    value: estimate_signal,
                                                    placeholder: "0",
                                                    size: InputSize::Small,
                                                    on_change: move |_| {
                                                        let v = estimate_signal.read().clone();
                                                        let parsed: Option<i64> = v.parse().ok();
                                                        on_patch
                                                            .call((
                                                                task_id,
                                                                TaskUpdate {
                                                                    estimate_minutes: Some(parsed),
                                                                    ..Default::default()
                                                                },
                                                            ));
                                                    },
                                                }
                                            }
                                        }
                                    }
                                    div { class: "flex flex-col gap-1",
                                        Label { "Tags" }
                                        {
                                            let tags_signal = tags_signal;
                                            rsx! {
                                                Input {
                                                    value: tags_signal,
                                                    placeholder: "comma,separated",
                                                    size: InputSize::Small,
                                                    on_change: move |_| {
                                                        let v = tags_signal.read().clone();
                                                        let parsed: Vec<String> = v
                                                            .split(',')
                                                            .map(|s| s.trim().to_string())
                                                            .filter(|s| !s.is_empty())
                                                            .collect();
                                                        on_patch
                                                            .call((
                                                                task_id,
                                                                TaskUpdate { tags: Some(parsed), ..Default::default() },
                                                            ));
                                                    },
                                                }
                                            }
                                        }
                                    }
                                    {
                                        let updated = t.updated_at.format("%b %-d %H:%M").to_string();
                                        rsx! {
                                            div { class: "text-[10px] text-muted-foreground pt-1",
                                                "Updated {updated}"
                                            }
                                        }
                                    }
                                }
                            }
                        }

                        SheetFooter {
                            Button {
                                variant: ButtonVariant::Destructive,
                                size: ButtonSize::Small,
                                on_click: move |_| confirm_open.set(true),
                                Trash2 { size: 14 }
                                " Delete"
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| on_duplicate.call(task_id),
                                CopyIcon { size: 14 }
                                " Duplicate"
                            }
                            Button {
                                variant: ButtonVariant::Outline,
                                size: ButtonSize::Small,
                                on_click: move |_| on_close.call(()),
                                X { size: 14 }
                                " Close"
                            }
                        }

                        AlertDialog {
                            open: Some(confirm_open()),
                            on_open_change: move |o: bool| confirm_open.set(o),
                            AlertDialogHeader {
                                AlertDialogTitle { "Delete this task?" }
                                AlertDialogDescription {
                                    "This will permanently remove the task and any subtasks beneath it. This action cannot be undone."
                                }
                            }
                            AlertDialogFooter {
                                AlertDialogCancel {
                                    on_click: move |_| confirm_open.set(false),
                                    "Cancel"
                                }
                                AlertDialogAction {
                                    on_click: move |_| {
                                        confirm_open.set(false);
                                        on_delete.call(task_id);
                                    },
                                    "Delete"
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
