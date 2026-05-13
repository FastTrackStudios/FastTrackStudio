//! Conference feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! - [`MeetingList`]       — full collection view, dispatches `on_delete`
//! - [`MeetingRow`]        — single-row presentation
//! - [`MeetingCreateForm`] — minimal new-meeting form, emits the create payload

use conference_proto::{Meeting, MeetingCreate};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{CalendarClock, Plus, Radio, Trash2, Users, Video};
use fts_ui::prelude::*;
use std::collections::BTreeSet;
use uuid::Uuid;

#[component]
pub fn MeetingList(items: Vec<Meeting>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No meetings yet. Add one above.",
                icon: rsx! { Video { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for meeting in items.iter().cloned() {
                MeetingRow {
                    key: "{meeting.id}",
                    meeting: meeting.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn MeetingRow(meeting: Meeting, on_delete: EventHandler<Uuid>) -> Element {
    let id = meeting.id;
    let scheduled = meeting.scheduled_at.format("%Y-%m-%d %H:%M").to_string();
    let status = meeting.status.clone();
    let status_variant = match status.as_str() {
        "live" | "in_progress" | "started" => StatusBadgeVariant::Success,
        "ended" | "completed" => StatusBadgeVariant::Neutral,
        "cancelled" | "canceled" => StatusBadgeVariant::Danger,
        _ => StatusBadgeVariant::Warning,
    };
    let meta = format!("scheduled {scheduled}");
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{meeting.name}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                StatusBadge { variant: status_variant, label: "{status}" }
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
pub fn MeetingDashboard(
    items: Vec<Meeting>,
    status: String,
    on_create: EventHandler<MeetingCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let total = items.len();
    let now = chrono::Utc::now();
    let live = items
        .iter()
        .filter(|m| {
            matches!(
                m.status.as_str(),
                "live" | "in_progress" | "in-progress" | "started"
            )
        })
        .count();
    let upcoming = items
        .iter()
        .filter(|m| m.scheduled_at > now && m.ended_at.is_none())
        .count();
    let completed = items
        .iter()
        .filter(|m| matches!(m.status.as_str(), "completed" | "ended"))
        .count();
    let participants: BTreeSet<String> = items
        .iter()
        .flat_map(|m| m.participants.iter().cloned())
        .collect();
    let mut tab = use_signal(|| "upcoming".to_string());

    let filtered: Vec<Meeting> = match tab.read().as_str() {
        "live" => items
            .iter()
            .filter(|m| {
                matches!(
                    m.status.as_str(),
                    "live" | "in_progress" | "in-progress" | "started"
                )
            })
            .cloned()
            .collect(),
        "upcoming" => items
            .iter()
            .filter(|m| m.scheduled_at > now)
            .cloned()
            .collect(),
        "past" => items
            .iter()
            .filter(|m| m.ended_at.is_some() || m.scheduled_at <= now)
            .cloned()
            .collect(),
        _ => items.clone(),
    };

    rsx! {
        div { class: "mx-auto flex max-w-5xl flex-col gap-6 p-6 lg:p-10",
            SectionHeader {
                label: "Conference".to_string(),
                trailing: rsx! {
                    StatusBadge { variant: StatusBadgeVariant::Neutral, label: status.clone() }
                },
            }
            HStack { class: "items-center gap-3",
                div { class: "rounded-md bg-blue-500/10 p-2 text-blue-500",
                    Video { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Meetings" }
                    Text { variant: TextVariant::Muted,
                        "Track live calls, upcoming sessions, and past recordings."
                    }
                }
            }
            div { class: "grid gap-3 sm:grid-cols-2 lg:grid-cols-4",
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Live now" }
                        Radio { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{live}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "in-progress meetings" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Upcoming" }
                        CalendarClock { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{upcoming}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "scheduled ahead" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Completed" }
                        Video { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{completed}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "of {total} total" }
                    }
                }
                Card {
                    CardHeader { class: "flex flex-row items-center justify-between space-y-0 pb-2",
                        CardTitle { class: "text-sm font-medium", "Attendees" }
                        Users { size: 16 }
                    }
                    CardContent {
                        div { class: "text-2xl font-bold", "{participants.len()}" }
                        Text { variant: TextVariant::Muted, class: "text-xs", "unique participants" }
                    }
                }
            }

            MeetingCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            HStack { class: "items-center justify-between",
                Heading { level: HeadingLevel::H3, "Calendar" }
                SegmentedControl {
                    value: tab.read().clone(),
                    on_change: move |v: String| tab.set(v),
                    options: vec![
                        ("upcoming".to_string(), "Upcoming".to_string()),
                        ("live".to_string(), "Live".to_string()),
                        ("past".to_string(), "Past".to_string()),
                        ("all".to_string(), "All".to_string()),
                    ],
                }
            }
            MeetingList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}

#[component]
pub fn MeetingCreateForm(on_submit: EventHandler<MeetingCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut host_user = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Schedule a meeting" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: host_user,
                        placeholder: "Host user",
                        class: "flex-1 min-w-40",
                    }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let n = name.read().clone();
                            if n.trim().is_empty() {
                                return;
                            }
                            let payload = MeetingCreate {
                                name: n,
                                host_user: trim_to_option(host_user.read().clone()),
                                scheduled_at: chrono::Utc::now(),
                                started_at: None,
                                ended_at: None,
                                status: "scheduled".into(),
                                recording_url: None,
                                notes: None,
                                participants: Vec::new(),
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            host_user.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add meeting"
                    }
                }
            }
        }
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
