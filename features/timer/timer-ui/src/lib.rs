//! Timer feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! - [`TimeEntryList`]       — full collection view, dispatches `on_delete` / `on_stop`
//! - [`TimeEntryRow`]        — single-row presentation (composable into other lists)
//! - [`TimeEntryCreateForm`] — minimal "start a timer" form, emits the create payload

use chrono::{DateTime, Duration, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{Clock, DollarSign, Pause, Play, Plus, Timer, Trash2};
use fts_ui::prelude::*;
use timer_proto::{TimeEntry, TimeEntryCreate};
use uuid::Uuid;

pub mod solidtime;
pub use solidtime::*;

#[component]
pub fn TimeEntryList(
    items: Vec<TimeEntry>,
    on_delete: EventHandler<Uuid>,
    on_stop: EventHandler<Uuid>,
) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No time entries yet. Start a timer above.",
                icon: rsx! { Play { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for entry in items.iter().cloned() {
                TimeEntryRow {
                    key: "{entry.id}",
                    entry: entry.clone(),
                    on_delete: move |id| on_delete.call(id),
                    on_stop: move |id| on_stop.call(id),
                }
            }
        }
    }
}

#[component]
pub fn TimeEntryRow(
    entry: TimeEntry,
    on_delete: EventHandler<Uuid>,
    on_stop: EventHandler<Uuid>,
) -> Element {
    let id = entry.id;
    let running = entry.end_time.is_none();
    let description = entry
        .description
        .clone()
        .unwrap_or_else(|| "(no description)".into());
    let user = entry.user.clone().unwrap_or_else(|| "unknown".into());
    let duration = duration_string(entry.start_time, entry.end_time);
    let meta = format!("{user} · {duration}");
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{description}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                if entry.billable {
                    Badge { variant: BadgeVariant::Secondary, "Billable" }
                }
                if running {
                    StatusBadge { variant: StatusBadgeVariant::Warning, label: "Running" }
                    Button {
                        variant: ButtonVariant::Outline,
                        size: ButtonSize::Small,
                        on_click: move |_| on_stop.call(id),
                        Pause { size: 14 }
                        " Stop"
                    }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| on_delete.call(id),
                    Trash2 { size: 14 }
                }
            }
        }
    }
}

#[component]
pub fn TimeEntryCreateForm(on_submit: EventHandler<TimeEntryCreate>) -> Element {
    let mut description = use_signal(String::new);
    let mut user = use_signal(String::new);
    let mut billable = use_signal(|| false);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Start a timer" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: description,
                        placeholder: "Description",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: user,
                        placeholder: "User (e.g. cody)",
                        class: "flex-1 min-w-40",
                    }
                }
                div { class: "flex items-center gap-3",
                    Switch { checked: billable }
                    span { class: "text-sm text-muted-foreground", "Billable" }
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let payload = TimeEntryCreate {
                                task_id: None,
                                project_id: None,
                                client_id: None,
                                user: trim_to_option(user.read().clone()),
                                start_time: Utc::now(),
                                end_time: None,
                                description: trim_to_option(description.read().clone()),
                                billable: *billable.read(),
                                manual: false,
                                billable_rate_cents: None,
                                tags: Vec::new(),
                                invoiced_at: None,
                            };
                            on_submit.call(payload);
                            description.set(String::new());
                            user.set(String::new());
                            billable.set(false);
                        },
                        Plus { size: 14 }
                        " Start timer"
                    }
                }
            }
        }
    }
}

fn duration_string(start: DateTime<Utc>, end: Option<DateTime<Utc>>) -> String {
    let end = end.unwrap_or_else(Utc::now);
    let total = (end - start).num_seconds().max(0);
    if total < 60 {
        format!("{}s", total)
    } else {
        let m = total / 60;
        let s = total % 60;
        format!("{}m {}s", m, s)
    }
}

fn trim_to_option(s: String) -> Option<String> {
    let t = s.trim();
    if t.is_empty() {
        None
    } else {
        Some(t.to_string())
    }
}

fn format_hms(total_secs: i64) -> String {
    let s = total_secs.max(0);
    let h = s / 3600;
    let m = (s % 3600) / 60;
    let sec = s % 60;
    if h > 0 {
        format!("{}h {:02}m", h, m)
    } else if m > 0 {
        format!("{}m {:02}s", m, sec)
    } else {
        format!("{}s", sec)
    }
}

/// Purpose-built time-entry dashboard. Page header + running/today/billable
/// stats + billable filter + create form + list.
#[component]
pub fn TimeEntryDashboard(
    items: Vec<TimeEntry>,
    status: String,
    on_create: EventHandler<TimeEntryCreate>,
    on_delete: EventHandler<Uuid>,
    on_stop: EventHandler<Uuid>,
) -> Element {
    let mut filter = use_signal(|| "all".to_string());

    let now = Utc::now();
    let today_start = now - Duration::hours(24);

    let running = items.iter().filter(|e| e.end_time.is_none()).count();
    let billable_count = items.iter().filter(|e| e.billable).count();
    let non_billable_count = items.len() - billable_count;

    let today_secs: i64 = items
        .iter()
        .filter(|e| e.start_time >= today_start)
        .map(|e| {
            let end = e.end_time.unwrap_or(now);
            (end - e.start_time).num_seconds().max(0)
        })
        .sum();
    let billable_secs: i64 = items
        .iter()
        .filter(|e| e.billable)
        .map(|e| {
            let end = e.end_time.unwrap_or(now);
            (end - e.start_time).num_seconds().max(0)
        })
        .sum();

    let current = filter.read().clone();
    let filtered: Vec<TimeEntry> = match current.as_str() {
        "billable" => items.iter().filter(|e| e.billable).cloned().collect(),
        "non-billable" => items.iter().filter(|e| !e.billable).cloned().collect(),
        "running" => items
            .iter()
            .filter(|e| e.end_time.is_none())
            .cloned()
            .collect(),
        _ => items.clone(),
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Time tracking",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: StatusDotColor::Success,
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-amber-500/10 p-2 text-amber-500",
                    Timer { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Time entries dashboard" }
                    Text { variant: TextVariant::Muted,
                        "Live timers, billable hours, and what's been tracked in the last 24h."
                    }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-4 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Running" }
                            Play { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{running}" }
                        Text { variant: TextVariant::Muted, "active timers" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Last 24h" }
                            Clock { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{format_hms(today_secs)}" }
                        Text { variant: TextVariant::Muted, "tracked" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Billable" }
                            DollarSign { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{format_hms(billable_secs)}" }
                        Text { variant: TextVariant::Muted, "{billable_count} entries" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Internal" }
                            Timer { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{non_billable_count}" }
                        Text { variant: TextVariant::Muted, "non-billable" }
                    }
                }
            }

            HStack { class: "gap-2 flex-wrap items-center",
                Text { variant: TextVariant::Muted, "Filter:" }
                Button {
                    variant: if current == "all" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| filter.set("all".into()),
                    "All"
                }
                Button {
                    variant: if current == "running" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| filter.set("running".into()),
                    "Running"
                }
                Button {
                    variant: if current == "billable" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| filter.set("billable".into()),
                    "Billable"
                }
                Button {
                    variant: if current == "non-billable" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| filter.set("non-billable".into()),
                    "Internal"
                }
            }

            TimeEntryCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Entries",
                trailing: rsx! {
                    Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" }
                },
            }
            TimeEntryList {
                items: filtered,
                on_delete: move |id| on_delete.call(id),
                on_stop: move |id| on_stop.call(id),
            }
        }
    }
}
