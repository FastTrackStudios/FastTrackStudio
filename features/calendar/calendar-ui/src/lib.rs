//! Calendar feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! - [`CalendarEventList`]       — full collection view, dispatches `on_delete`
//! - [`CalendarEventRow`]        — single-row presentation (composable into other lists)
//! - [`CalendarEventCreateForm`] — minimal new-event form, emits the create payload

use calendar_proto::{CalendarEvent, CalendarEventCreate};
use chrono::{Duration, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{
    CalendarCheck, CalendarClock, CalendarDays, CalendarX, Clock, Plus, Trash2,
};
use fts_ui::prelude::*;
use uuid::Uuid;

pub mod interactive;
pub use interactive::{CalendarView, InteractiveCalendar};

/// Purpose-built dashboard for the calendar feature — header, stat
/// cards (today, this week, recurring, cancelled), filter tabs, create
/// form, list.
#[component]
pub fn CalendarEventDashboard(
    items: Vec<CalendarEvent>,
    status: String,
    on_create: EventHandler<CalendarEventCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut filter = use_signal(|| "upcoming".to_string());

    let now = Utc::now();
    let today_end = now + Duration::hours(24);
    let week_end = now + Duration::days(7);

    let total = items.len();
    let today_count = items
        .iter()
        .filter(|e| e.start_at >= now && e.start_at <= today_end)
        .count();
    let week_count = items
        .iter()
        .filter(|e| e.start_at >= now && e.start_at <= week_end)
        .count();
    let recurring = items.iter().filter(|e| e.rrule.is_some()).count();
    let cancelled = items.iter().filter(|e| e.status == "cancelled").count();

    let f = filter.read().clone();
    let filtered: Vec<CalendarEvent> = items
        .iter()
        .filter(|e| match f.as_str() {
            "upcoming" => e.start_at >= now && e.status != "cancelled",
            "today" => e.start_at >= now && e.start_at <= today_end,
            "past" => e.end_at < now,
            _ => true,
        })
        .cloned()
        .collect();

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Calendar".to_string(),
                trailing: rsx! {
                    StatusBadge {
                        variant: StatusBadgeVariant::Neutral,
                        label: status.clone(),
                    }
                },
            }
            HStack { class: "gap-2 items-center",
                CalendarDays { size: 22 }
                Heading { level: HeadingLevel::H2, "Events" }
            }
            Text { variant: TextVariant::Muted, "Meetings, sessions, and appointments across every connected calendar." }

            div { class: "grid grid-cols-2 md:grid-cols-4 gap-3",
                Card { class: "border-l-4 border-l-blue-500",
                    CardHeader {
                        CardDescription { "Today" }
                        CardTitle { class: "text-2xl", "{today_count}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-blue-500",
                        Clock { size: 16 }
                        Text { variant: TextVariant::Muted, "next 24h" }
                    }
                }
                Card { class: "border-l-4 border-l-teal-500",
                    CardHeader {
                        CardDescription { "This week" }
                        CardTitle { class: "text-2xl", "{week_count}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-teal-500",
                        CalendarCheck { size: 16 }
                        Text { variant: TextVariant::Muted, "next 7 days" }
                    }
                }
                Card { class: "border-l-4 border-l-purple-500",
                    CardHeader {
                        CardDescription { "Recurring" }
                        CardTitle { class: "text-2xl", "{recurring}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-purple-500",
                        CalendarClock { size: 16 }
                        Text { variant: TextVariant::Muted, "{total} total" }
                    }
                }
                Card { class: "border-l-4 border-l-rose-500",
                    CardHeader {
                        CardDescription { "Cancelled" }
                        CardTitle { class: "text-2xl", "{cancelled}" }
                    }
                    CardContent { class: "flex items-center gap-2 text-rose-500",
                        CalendarX { size: 16 }
                        Text { variant: TextVariant::Muted, "this view" }
                    }
                }
            }

            Divider {}

            CalendarEventCreateForm { on_submit: move |p| on_create.call(p) }

            HStack { class: "items-center justify-between",
                Heading { level: HeadingLevel::H3, "Schedule" }
                SegmentedControl {
                    value: filter.read().clone(),
                    on_change: move |v: String| filter.set(v),
                    options: vec![
                        ("upcoming".to_string(), "Upcoming".to_string()),
                        ("today".to_string(), "Today".to_string()),
                        ("past".to_string(), "Past".to_string()),
                        ("all".to_string(), "All".to_string()),
                    ],
                }
            }
            CalendarEventList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}

#[component]
pub fn CalendarEventList(items: Vec<CalendarEvent>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No calendar events yet. Add one above.",
                icon: rsx! { CalendarDays { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for event in items.iter().cloned() {
                CalendarEventRow {
                    key: "{event.id}",
                    event: event.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn CalendarEventRow(event: CalendarEvent, on_delete: EventHandler<Uuid>) -> Element {
    let id = event.id;
    let when = event.start_at.format("%Y-%m-%d %H:%M").to_string();
    let status = event.status.clone();
    let status_variant = match status.as_str() {
        "confirmed" => StatusBadgeVariant::Success,
        "cancelled" => StatusBadgeVariant::Danger,
        "tentative" => StatusBadgeVariant::Warning,
        _ => StatusBadgeVariant::Neutral,
    };
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{event.title}" }
                ItemDescription { "{when}" }
            }
            ItemActions { class: "gap-2",
                StatusBadge { variant: status_variant, label: status }
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
pub fn CalendarEventCreateForm(on_submit: EventHandler<CalendarEventCreate>) -> Element {
    let mut title = use_signal(String::new);
    let mut description = use_signal(String::new);
    rsx! {
        Card {
            CardHeader {
                CardTitle { "New event" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: title,
                        placeholder: "Title (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: description,
                        placeholder: "Description",
                        class: "flex-1 min-w-40",
                    }
                }
                div { class: "flex items-center gap-3",
                    div { class: "flex-1" }
                    Button {
                        on_click: move |_| {
                            let t = title.read().clone();
                            if t.trim().is_empty() {
                                return;
                            }
                            let start_at = Utc::now();
                            let end_at = start_at + Duration::hours(1);
                            let payload = CalendarEventCreate {
                                title: t,
                                description: trim_to_option(description.read().clone()),
                                start_at,
                                end_at,
                                all_day: false,
                                location_id: None,
                                location_text: None,
                                rrule: None,
                                organizer: None,
                                attendees: Vec::new(),
                                calendar_id: None,
                                status: "confirmed".into(),
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            title.set(String::new());
                            description.set(String::new());
                        },
                        Plus { size: 14 }
                        " Add event"
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
