//! `TimeEntryTable` — Solidtime's Time tab, grouped by day with sticky day headers.

use std::collections::HashMap;

use chrono::{Local, NaiveDate, TimeZone, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Copy, DollarSign, Ellipsis, Trash2};
use fts_ui::prelude::*;
use invoice_proto::Client;
use project_proto::Project;
use timer_proto::{TimeEntry, TimeEntryUpdate};
use uuid::Uuid;

use super::common::{bucket_by_day, entry_seconds, format_duration_compact, format_money};

#[derive(Props, Clone, PartialEq)]
pub struct TimeEntryTableProps {
    pub entries: Vec<TimeEntry>,
    pub projects_by_id: HashMap<Uuid, Project>,
    pub clients_by_id: HashMap<Uuid, Client>,
    pub on_edit: EventHandler<(Uuid, TimeEntryUpdate)>,
    pub on_delete: EventHandler<Uuid>,
    pub on_duplicate: EventHandler<Uuid>,
}

#[component]
pub fn TimeEntryTable(props: TimeEntryTableProps) -> Element {
    if props.entries.is_empty() {
        return rsx! {
            EmptyState {
                message: "No time entries match the current filter.",
                icon: rsx! { div { class: "text-muted-foreground", "—" } },
            }
        };
    }

    let groups = bucket_by_day(&props.entries);
    let now = Utc::now();

    rsx! {
        div { class: "flex flex-col",
            for (date, entries) in groups.into_iter() {
                {
                    let total: i64 = entries.iter().map(|e| entry_seconds(e, now)).sum();
                    let billable_cents: i64 = entries.iter().filter(|e| e.billable).map(|e| {
                        let rate = e.billable_rate_cents.unwrap_or(0) as i64;
                        rate * entry_seconds(e, now) / 3600
                    }).sum();
                    let count = entries.len();
                    rsx! {
                        div { key: "{date}", class: "flex flex-col",
                            DayHeader {
                                date: date,
                                count: count,
                                total_seconds: total,
                                billable_cents: billable_cents,
                            }
                            for entry in entries {
                                EntryRow {
                                    key: "{entry.id}",
                                    entry: entry.clone(),
                                    projects_by_id: props.projects_by_id.clone(),
                                    clients_by_id: props.clients_by_id.clone(),
                                    on_edit: props.on_edit,
                                    on_delete: props.on_delete,
                                    on_duplicate: props.on_duplicate,
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

#[component]
fn DayHeader(date: NaiveDate, count: usize, total_seconds: i64, billable_cents: i64) -> Element {
    let header = date.format("%A, %B %-d").to_string();
    rsx! {
        div {
            class: "sticky top-0 z-10 flex items-center justify-between border-b bg-background/95 px-3 py-2 text-sm backdrop-blur",
            span { class: "font-medium", "{header}" }
            div { class: "flex items-center gap-4 text-xs text-muted-foreground",
                span { "{count} entries" }
                span { "{format_duration_compact(total_seconds)}" }
                span { "{format_money(billable_cents, \"USD\")}" }
            }
        }
    }
}

#[component]
fn EntryRow(
    entry: TimeEntry,
    projects_by_id: HashMap<Uuid, Project>,
    clients_by_id: HashMap<Uuid, Client>,
    on_edit: EventHandler<(Uuid, TimeEntryUpdate)>,
    on_delete: EventHandler<Uuid>,
    on_duplicate: EventHandler<Uuid>,
) -> Element {
    let id = entry.id;
    let now = Utc::now();
    let secs = entry_seconds(&entry, now);
    let start_local = Local.from_utc_datetime(&entry.start_time.naive_utc());
    let end_local = entry
        .end_time
        .map(|e| Local.from_utc_datetime(&e.naive_utc()));
    let time_range = match end_local {
        Some(end) => format!("{} – {}", start_local.format("%H:%M"), end.format("%H:%M")),
        None => format!("{} – ...", start_local.format("%H:%M")),
    };
    let description = entry
        .description
        .clone()
        .unwrap_or_else(|| "(no description)".into());

    let project = entry.project_id.and_then(|pid| projects_by_id.get(&pid));
    let project_name = project.map(|p| p.name.clone());
    let project_color = project
        .and_then(|p| p.color.clone())
        .unwrap_or_else(|| "#888".into());

    let _client = entry.client_id.and_then(|cid| clients_by_id.get(&cid));

    let billable = entry.billable;
    let tags = entry.tags.clone();

    rsx! {
        ContextMenu {
            ContextMenuTrigger {
                div { class: "group flex items-center gap-3 border-b px-3 py-2 text-sm hover:bg-accent/30",

                    // Time range — popover edit
                    div { class: "min-w-28 font-mono text-xs tabular-nums text-muted-foreground",
                        TimeRangePopover {
                            entry: entry.clone(),
                            on_edit: on_edit,
                            label: time_range,
                        }
                    }

                    // Description (inline edit)
                    div { class: "min-w-0 flex-1",
                        InlineEdit {
                            value: description.clone(),
                            on_commit: move |new_desc: String| {
                                on_edit.call((id, TimeEntryUpdate {
                                    description: Some(Some(new_desc)),
                                    ..Default::default()
                                }));
                            },
                            class: "text-sm",
                        }
                    }

                    // Project chip
                    div { class: "min-w-32",
                        if let Some(name) = project_name {
                            div { class: "flex items-center gap-2",
                                span {
                                    class: "inline-block size-2.5 rounded-full",
                                    style: format!("background: {}", project_color),
                                }
                                span { class: "text-xs", "{name}" }
                            }
                        } else {
                            Text { variant: TextVariant::Muted, "—" }
                        }
                    }

                    // Billable indicator
                    div { class: "w-6",
                        if billable {
                            DollarSign { size: 12 }
                        }
                    }

                    // Tag chips (first 2 + overflow)
                    div { class: "flex flex-wrap gap-1 max-w-40",
                        for tag in tags.iter().take(2) {
                            Badge { variant: BadgeVariant::Secondary, "{tag}" }
                        }
                        if tags.len() > 2 {
                            Badge { variant: BadgeVariant::Outline, "+{tags.len() - 2}" }
                        }
                    }

                    // Duration (inline-edit-ish via popover)
                    div { class: "min-w-16 text-right font-mono text-xs tabular-nums",
                        DurationEdit {
                            entry: entry.clone(),
                            on_edit: on_edit,
                            label: format_duration_compact(secs),
                        }
                    }

                    // Hover actions
                    div { class: "ml-2 hidden items-center gap-1 group-hover:flex",
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: move |_| on_duplicate.call(id),
                            Copy { size: 12 }
                        }
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: move |_| on_delete.call(id),
                            Trash2 { size: 12 }
                        }
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: move |_| {},
                            Ellipsis { size: 12 }
                        }
                    }
                }
            }
            ContextMenuContent {
                ContextMenuItem {
                    value: "duplicate",
                    index: 0,
                    on_select: move |_| on_duplicate.call(id),
                    "Duplicate"
                }
                ContextMenuItem {
                    value: "billable",
                    index: 1,
                    on_select: move |_| on_edit.call((id, TimeEntryUpdate {
                        billable: Some(true),
                        ..Default::default()
                    })),
                    "Mark billable"
                }
                ContextMenuItem {
                    value: "non-billable",
                    index: 2,
                    on_select: move |_| on_edit.call((id, TimeEntryUpdate {
                        billable: Some(false),
                        ..Default::default()
                    })),
                    "Mark non-billable"
                }
                ContextMenuSeparator {}
                ContextMenuItem {
                    value: "delete",
                    index: 3,
                    destructive: true,
                    on_select: move |_| on_delete.call(id),
                    "Delete"
                }
            }
        }
    }
}

#[component]
fn TimeRangePopover(
    entry: TimeEntry,
    on_edit: EventHandler<(Uuid, TimeEntryUpdate)>,
    label: String,
) -> Element {
    let id = entry.id;
    let start_local = Local.from_utc_datetime(&entry.start_time.naive_utc());
    let end_local = entry
        .end_time
        .map(|e| Local.from_utc_datetime(&e.naive_utc()));

    let mut start_str = use_signal(|| start_local.format("%H:%M").to_string());
    let mut end_str = use_signal(|| {
        end_local
            .map(|e| e.format("%H:%M").to_string())
            .unwrap_or_default()
    });
    let date = entry.start_time.date_naive();

    let commit = {
        move |_| {
            // Parse "HH:MM" relative to entry's date in local tz.
            let parse = |s: &str| -> Option<chrono::DateTime<Utc>> {
                let (h, m) = s.split_once(':')?;
                let h: u32 = h.trim().parse().ok()?;
                let m: u32 = m.trim().parse().ok()?;
                let naive = date.and_hms_opt(h, m, 0)?;
                let local = Local.from_local_datetime(&naive).single()?;
                Some(local.with_timezone(&Utc))
            };
            let s = start_str.read().clone();
            let e = end_str.read().clone();
            let new_start = parse(&s);
            let new_end = if e.trim().is_empty() {
                Some(None)
            } else {
                parse(&e).map(Some)
            };
            on_edit.call((
                id,
                TimeEntryUpdate {
                    start_time: new_start,
                    end_time: new_end,
                    ..Default::default()
                },
            ));
        }
    };

    rsx! {
        Popover {
            PopoverTrigger {
                button { class: "rounded px-1 py-0.5 hover:bg-accent", "{label}" }
            }
            PopoverContent { class: "p-3 min-w-48",
                div { class: "flex flex-col gap-2",
                    label { class: "text-xs text-muted-foreground", "Start" }
                    input {
                        r#type: "time",
                        class: "h-8 rounded border bg-background px-2 text-sm",
                        value: "{start_str}",
                        oninput: move |e| start_str.set(e.value()),
                    }
                    label { class: "text-xs text-muted-foreground", "End" }
                    input {
                        r#type: "time",
                        class: "h-8 rounded border bg-background px-2 text-sm",
                        value: "{end_str}",
                        oninput: move |e| end_str.set(e.value()),
                    }
                    Button {
                        size: ButtonSize::Small,
                        on_click: commit,
                        "Save"
                    }
                }
            }
        }
    }
}

#[component]
fn DurationEdit(
    entry: TimeEntry,
    on_edit: EventHandler<(Uuid, TimeEntryUpdate)>,
    label: String,
) -> Element {
    let id = entry.id;
    let start = entry.start_time;
    let mut draft = use_signal(|| label.clone());

    let commit = move |_| {
        let s = draft.read().clone();
        // Parse `1:30` or `90m` or `1.5h` (best effort).
        let secs: Option<i64> = if let Some((h, m)) = s.split_once(':') {
            let h: Option<i64> = h.trim().parse().ok();
            let m: Option<i64> = m.trim().parse().ok();
            h.and_then(|h| m.map(|m| h * 3600 + m * 60))
        } else if let Some(num) = s.trim().strip_suffix('m') {
            num.trim().parse::<i64>().ok().map(|m| m * 60)
        } else if let Some(num) = s.trim().strip_suffix('h') {
            num.trim().parse::<f64>().ok().map(|h| (h * 3600.0) as i64)
        } else {
            None
        };
        if let Some(secs) = secs {
            let new_end = start + chrono::Duration::seconds(secs);
            on_edit.call((
                id,
                TimeEntryUpdate {
                    end_time: Some(Some(new_end)),
                    ..Default::default()
                },
            ));
        }
    };

    rsx! {
        Popover {
            PopoverTrigger {
                button { class: "rounded px-1 py-0.5 hover:bg-accent", "{label}" }
            }
            PopoverContent { class: "p-3 min-w-44",
                div { class: "flex flex-col gap-2",
                    label { class: "text-xs text-muted-foreground", "Duration (1:30 or 90m)" }
                    input {
                        class: "h-8 rounded border bg-background px-2 text-sm",
                        value: "{draft}",
                        oninput: move |e| draft.set(e.value()),
                    }
                    Button {
                        size: ButtonSize::Small,
                        on_click: commit,
                        "Save"
                    }
                }
            }
        }
    }
}
