//! Agenda view — flat list of events grouped by local date, sticky headers.

use calendar_proto::CalendarEvent;
use chrono::{DateTime, Local, NaiveDate, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::CalendarDays;
use fts_ui::prelude::*;
use std::collections::BTreeMap;

#[component]
pub fn AgendaList(
    /// Window center; agenda renders ± `window_days` around it.
    anchor: DateTime<Utc>,
    /// Days each side of anchor to include.
    #[props(default = 30)]
    window_days: i64,
    events: Vec<CalendarEvent>,
    on_pick_event: EventHandler<CalendarEvent>,
) -> Element {
    let anchor_local = anchor.with_timezone(&Local).date_naive();
    let from = anchor_local - chrono::Duration::days(window_days);
    let to = anchor_local + chrono::Duration::days(window_days);

    let mut buckets: BTreeMap<NaiveDate, Vec<CalendarEvent>> = BTreeMap::new();
    for e in events.iter().cloned() {
        let d = e.start_at.with_timezone(&Local).date_naive();
        if d >= from && d <= to {
            buckets.entry(d).or_default().push(e);
        }
    }
    for v in buckets.values_mut() {
        v.sort_by_key(|e| e.start_at);
    }

    if buckets.is_empty() {
        return rsx! {
            EmptyState {
                message: "No events in this window.",
                icon: rsx! { CalendarDays { size: 32 } },
            }
        };
    }

    rsx! {
        div { class: "flex flex-col gap-3",
            for (date, evs) in buckets.into_iter() {
                ItemGroup {
                    key: "{date}",
                    div { class: "sticky top-0 z-10 bg-background/95 backdrop-blur px-2 py-1 border-b border-border",
                        SectionHeader {
                            label: format!("{}", date.format("%A, %b %-d")),
                            size: SectionHeaderSize::Small,
                        }
                    }
                    div { class: "flex flex-col gap-1 py-1",
                        for ev in evs.into_iter() {
                            {
                                let ev2 = ev.clone();
                                let when = ev.start_at.with_timezone(&Local).format("%H:%M").to_string();
                                let status_v = match ev.status.as_str() {
                                    "confirmed" => StatusBadgeVariant::Success,
                                    "tentative" => StatusBadgeVariant::Warning,
                                    "cancelled" => StatusBadgeVariant::Danger,
                                    _ => StatusBadgeVariant::Neutral,
                                };
                                let title = ev.title.clone();
                                let loc = ev.location_text.clone().unwrap_or_default();
                                let cb = Callback::new(move |_: ()| on_pick_event.call(ev2.clone()));
                                rsx! {
                                    ListRow {
                                        key: "{ev.id}",
                                        label: title,
                                        detail: loc,
                                        leading: rsx! { span { class: "text-xs font-mono text-muted-foreground w-12 inline-block", "{when}" } },
                                        trailing: rsx! { StatusBadge { variant: status_v, label: ev.status.clone() } },
                                        on_click: cb,
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
