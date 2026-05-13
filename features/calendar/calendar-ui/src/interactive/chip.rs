//! Event chip rendered inside month cells, week/day grids, and agenda rows.

use calendar_proto::CalendarEvent;
use chrono::Local;
use dioxus::prelude::*;

/// Visual style derived from the event status.
fn chip_classes(status: &str) -> &'static str {
    match status {
        "cancelled" => {
            "bg-destructive/15 text-destructive border border-destructive/40 line-through"
        }
        "tentative" => {
            "bg-amber-500/15 text-amber-700 dark:text-amber-300 border border-dashed border-amber-500/60"
        }
        _ => "bg-primary/15 text-primary border border-primary/40",
    }
}

#[component]
pub fn EventChip(
    event: CalendarEvent,
    /// When false, hides the start-time prefix (e.g. inside time-grid where
    /// position already conveys the time).
    #[props(default = true)]
    show_time: bool,
    /// Compact mode (denser typography for week/day).
    #[props(default = false)]
    compact: bool,
    on_click: EventHandler<CalendarEvent>,
) -> Element {
    let classes = chip_classes(&event.status);
    let size_cls = if compact {
        "text-[10px] px-1 py-0 leading-tight"
    } else {
        "text-xs px-1.5 py-0.5"
    };
    let local_start = event.start_at.with_timezone(&Local);
    let time_label = if event.all_day {
        "All day".to_string()
    } else {
        local_start.format("%H:%M").to_string()
    };
    let title = event.title.clone();
    let ev = event.clone();
    rsx! {
        button {
            class: "w-full rounded {classes} {size_cls} text-left truncate cursor-pointer hover:opacity-90 focus:outline-none focus:ring-1 focus:ring-ring",
            onclick: move |e: Event<MouseData>| {
                e.stop_propagation();
                on_click.call(ev.clone());
            },
            if show_time {
                span { class: "font-medium mr-1 opacity-80", "{time_label}" }
            }
            span { class: "truncate", "{title}" }
        }
    }
}
