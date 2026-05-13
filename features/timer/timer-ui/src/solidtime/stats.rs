//! `TimerSummaryStats` — four KPI cards.

use chrono::{Datelike, Duration, NaiveDate, Utc};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use timer_proto::TimeEntry;

use super::common::{entry_seconds, format_duration_hms, format_money, week_start};

#[derive(Props, Clone, PartialEq)]
pub struct TimerSummaryStatsProps {
    pub entries: Vec<TimeEntry>,
    pub today: NaiveDate,
}

#[derive(Clone, Copy, Default)]
struct Bucket {
    total: i64,
    count: usize,
    cents: i64,
    billable_secs: i64,
}

fn bucket(entries: &[TimeEntry], start: NaiveDate, end_exclusive: NaiveDate) -> Bucket {
    let now = Utc::now();
    let mut b = Bucket::default();
    for e in entries {
        let d = e.start_time.date_naive();
        if d >= start && d < end_exclusive {
            let secs = entry_seconds(e, now);
            b.total += secs;
            b.count += 1;
            if e.billable {
                b.billable_secs += secs;
                let rate = e.billable_rate_cents.unwrap_or(0) as i64;
                b.cents += rate * secs / 3600;
            }
        }
    }
    b
}

#[component]
pub fn TimerSummaryStats(props: TimerSummaryStatsProps) -> Element {
    let today = props.today;
    let tomorrow = today + Duration::days(1);
    let week = week_start(today);
    let next_week = week + Duration::days(7);
    let month_start = NaiveDate::from_ymd_opt(today.year(), today.month(), 1).unwrap_or(today);
    let next_month = if today.month() == 12 {
        NaiveDate::from_ymd_opt(today.year() + 1, 1, 1).unwrap_or(month_start)
    } else {
        NaiveDate::from_ymd_opt(today.year(), today.month() + 1, 1).unwrap_or(month_start)
    };

    let today_b = bucket(&props.entries, today, tomorrow);
    let week_b = bucket(&props.entries, week, next_week);
    let month_b = bucket(&props.entries, month_start, next_month);

    // Billable % over the *month* — what Solidtime shows.
    let billable_pct = if month_b.total == 0 {
        0
    } else {
        (month_b.billable_secs * 100 / month_b.total) as i32
    };

    rsx! {
        div { class: "grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-3",
            StatCard {
                label: "Today".to_string(),
                value: format_duration_hms(today_b.total),
                sublabel: format!("{} entries · {}", today_b.count, format_money(today_b.cents, "USD")),
            }
            StatCard {
                label: "This week".to_string(),
                value: format_duration_hms(week_b.total),
                sublabel: format!("{} entries · {}", week_b.count, format_money(week_b.cents, "USD")),
            }
            StatCard {
                label: "This month".to_string(),
                value: format_duration_hms(month_b.total),
                sublabel: format!("{} entries · {}", month_b.count, format_money(month_b.cents, "USD")),
            }
            StatCard {
                label: "Billable".to_string(),
                value: format!("{}%", billable_pct),
                sublabel: format!("{} of {}", format_duration_hms(month_b.billable_secs), format_duration_hms(month_b.total)),
            }
        }
    }
}

#[component]
fn StatCard(label: String, value: String, sublabel: String) -> Element {
    rsx! {
        Card {
            CardHeader {
                CardDescription { "{label}" }
            }
            CardContent {
                Heading { level: HeadingLevel::H2, "{value}" }
                Text { variant: TextVariant::Muted, "{sublabel}" }
            }
        }
    }
}
