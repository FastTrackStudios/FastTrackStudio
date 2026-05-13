//! Fitness feature UI. Dumb components — data + callbacks in, RSX out.
//!
//! v1 scope: `WorkoutSession` only. Exercise / Routine / SetLog / BodyMeasurement come later.

use chrono::{Duration, Utc};
use dioxus::prelude::*;
use fitness_proto::{WorkoutSession, WorkoutSessionCreate};
use fts_ui::lucide_dioxus::{Activity, Dumbbell, Flame, Plus, Timer, Trash2};
use fts_ui::prelude::*;
use uuid::Uuid;

#[component]
pub fn WorkoutSessionList(items: Vec<WorkoutSession>, on_delete: EventHandler<Uuid>) -> Element {
    if items.is_empty() {
        return rsx! {
            EmptyState {
                message: "No workout sessions yet. Add one above.",
                icon: rsx! { Dumbbell { size: 32 } },
            }
        };
    }
    rsx! {
        div { class: "flex flex-col gap-2",
            for session in items.iter().cloned() {
                WorkoutSessionRow {
                    key: "{session.id}",
                    session: session.clone(),
                    on_delete: move |id| on_delete.call(id),
                }
            }
        }
    }
}

#[component]
pub fn WorkoutSessionRow(session: WorkoutSession, on_delete: EventHandler<Uuid>) -> Element {
    let id = session.id;
    let started = session.started_at.format("%Y-%m-%d %H:%M").to_string();
    let mood = session.mood.clone().unwrap_or_else(|| "ok".into());
    let meta = format!("{} · {}", started, mood);
    let live = session.ended_at.is_none();
    let name = session.name.clone();
    rsx! {
        Item {
            ItemContent {
                ItemTitle { "{name}" }
                ItemDescription { "{meta}" }
            }
            ItemActions { class: "gap-2",
                if live {
                    StatusBadge { variant: StatusBadgeVariant::Warning, label: "Live" }
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
pub fn WorkoutSessionCreateForm(on_submit: EventHandler<WorkoutSessionCreate>) -> Element {
    let mut name = use_signal(String::new);
    let mut mood = use_signal(String::new);

    rsx! {
        Card {
            CardHeader {
                CardTitle { "Start a session" }
            }
            CardContent { class: "flex flex-col gap-3",
                div { class: "flex flex-wrap gap-2",
                    Input {
                        value: name,
                        placeholder: "Name (required)",
                        class: "flex-1 min-w-40",
                    }
                    Input {
                        value: mood,
                        placeholder: "Mood",
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
                            let payload = WorkoutSessionCreate {
                                routine_id: None,
                                name: n,
                                started_at: Utc::now(),
                                ended_at: None,
                                notes: None,
                                mood: trim_to_option(mood.read().clone()),
                                tags: Vec::new(),
                            };
                            on_submit.call(payload);
                            name.set(String::new());
                            mood.set(String::new());
                        },
                        Plus { size: 14 }
                        " Start session"
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

/// Purpose-built workout-session dashboard. Adds weekly volume, live-session
/// count, and a status filter tab strip on top of the existing list.
#[component]
pub fn WorkoutSessionDashboard(
    items: Vec<WorkoutSession>,
    status: String,
    on_create: EventHandler<WorkoutSessionCreate>,
    on_delete: EventHandler<Uuid>,
) -> Element {
    let mut tab = use_signal(|| "all".to_string());

    let now = Utc::now();
    let week_cutoff = now - Duration::days(7);
    let total = items.len();
    let live: Vec<&WorkoutSession> = items.iter().filter(|s| s.ended_at.is_none()).collect();
    let live_count = live.len();
    let this_week = items.iter().filter(|s| s.started_at >= week_cutoff).count();

    // Average session length (minutes) for ended sessions
    let mut total_minutes: i64 = 0;
    let mut ended_count: i64 = 0;
    for s in &items {
        if let Some(end) = s.ended_at {
            total_minutes += (end - s.started_at).num_minutes().max(0);
            ended_count += 1;
        }
    }
    let avg_minutes = if ended_count > 0 {
        total_minutes / ended_count
    } else {
        0
    };

    let current = tab.read().clone();
    let filtered: Vec<WorkoutSession> = match current.as_str() {
        "live" => items
            .iter()
            .filter(|s| s.ended_at.is_none())
            .cloned()
            .collect(),
        "ended" => items
            .iter()
            .filter(|s| s.ended_at.is_some())
            .cloned()
            .collect(),
        _ => items.clone(),
    };

    rsx! {
        VStack { class: "gap-6",
            SectionHeader {
                label: "Fitness",
                trailing: rsx! {
                    HStack { class: "gap-2 items-center",
                        StatusDot {
                            color: if live_count > 0 { StatusDotColor::Warning } else { StatusDotColor::Success },
                            size: StatusDotSize::Small,
                        }
                        Text { variant: TextVariant::Muted, "{status}" }
                    }
                },
            }

            HStack { class: "gap-3 items-start",
                div { class: "rounded-md bg-orange-500/10 p-2 text-orange-500",
                    Dumbbell { size: 24 }
                }
                VStack { class: "gap-1",
                    Heading { level: HeadingLevel::H1, "Workout sessions" }
                    Text { variant: TextVariant::Muted,
                        "Start a session, log the work, watch your weekly volume climb."
                    }
                }
            }

            div { class: "grid grid-cols-1 sm:grid-cols-3 gap-3",
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "This week" }
                            Flame { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{this_week}" }
                        Text { variant: TextVariant::Muted, "session(s) in last 7d" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Live now" }
                            Activity { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{live_count}" }
                        Text { variant: TextVariant::Muted, "running session(s)" }
                    }
                }
                Card {
                    CardHeader {
                        HStack { class: "items-center justify-between",
                            CardDescription { "Avg length" }
                            Timer { size: 16 }
                        }
                    }
                    CardContent {
                        Heading { level: HeadingLevel::H2, "{avg_minutes}m" }
                        Text { variant: TextVariant::Muted, "across {ended_count} ended" }
                    }
                }
            }

            HStack { class: "gap-2 items-center",
                Text { variant: TextVariant::Muted, "Show:" }
                Button {
                    variant: if current == "all" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("all".into()),
                    "All ({total})"
                }
                Button {
                    variant: if current == "live" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("live".into()),
                    "Live ({live_count})"
                }
                Button {
                    variant: if current == "ended" { ButtonVariant::Primary } else { ButtonVariant::Outline },
                    size: ButtonSize::Small,
                    on_click: move |_| tab.set("ended".into()),
                    "Ended"
                }
            }

            WorkoutSessionCreateForm { on_submit: move |p| on_create.call(p) }

            Divider {}

            SectionHeader {
                label: "Sessions",
                trailing: rsx! { Badge { variant: BadgeVariant::Secondary, "{filtered.len()}" } },
            }
            WorkoutSessionList { items: filtered, on_delete: move |id| on_delete.call(id) }
        }
    }
}
