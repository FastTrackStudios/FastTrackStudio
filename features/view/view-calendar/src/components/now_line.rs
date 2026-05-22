//! Current-time marker for the week/day time grid.
//!
//! A thin red horizontal line at `now` on today's column, plus a
//! small dot at the left edge of that column. Renders only when
//! `today` is in the visible window. A background task ticks a
//! signal every 60 seconds so the line creeps without requiring
//! the user to navigate.

use chrono::{NaiveDate, Timelike};
use dioxus::prelude::*;

#[derive(Props, Clone, PartialEq)]
pub struct NowLineProps {
    /// Visible day window — used to figure out *which* column is
    /// today (if any).
    pub days: Vec<NaiveDate>,
    /// Pixels per hour, matching the time grid.
    pub px_per_hour: i64,
}

#[component]
pub fn NowLine(props: NowLineProps) -> Element {
    let mut tick = use_signal(chrono::Local::now);
    use_future(move || async move {
        loop {
            dioxus_sdk_time::sleep(std::time::Duration::from_secs(60)).await;
            tick.set(chrono::Local::now());
        }
    });

    let now = *tick.read();
    let today = now.date_naive();
    let Some(col) = props.days.iter().position(|d| *d == today) else {
        return rsx! {};
    };
    let total_minutes = i64::from(now.hour()) * 60 + i64::from(now.minute());
    let top_px = (total_minutes * props.px_per_hour) / 60;
    let col_one = col + 1;

    rsx! {
        // Single column wrapper — the parent time-grid is a CSS
        // grid with `grid-template-columns: 56px repeat(N, 1fr)`,
        // and our `grid-column` lines up with today's day column.
        div {
            class: "pointer-events-none relative",
            style: "grid-column: {col_one + 1};", // +1 because col 1 is the hour-axis rail
            // Red dot at left edge.
            div {
                class: "absolute w-2 h-2 rounded-full bg-rose-500 z-20",
                style: "top: {top_px - 4}px; left: -4px;",
            }
            // Red 1px line across the day column.
            div {
                class: "absolute left-0 right-0 h-px bg-rose-500 z-20",
                style: "top: {top_px}px;",
            }
        }
    }
}
