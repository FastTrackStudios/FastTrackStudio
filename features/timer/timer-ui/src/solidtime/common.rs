//! Pure helpers — formatting + bucketing. Zero Dioxus, zero state.

use chrono::{DateTime, Datelike, Duration, NaiveDate, Utc};
use timer_proto::TimeEntry;

/// `5m`, `42m`, `1:23`, `8:45` — compact human duration. Used in
/// table cells where vertical space is tight.
pub fn format_duration_compact(seconds: i64) -> String {
    let s = seconds.max(0);
    let h = s / 3600;
    let m = (s % 3600) / 60;
    if h == 0 {
        if m == 0 {
            format!("{}s", s)
        } else {
            format!("{}m", m)
        }
    } else {
        format!("{}:{:02}", h, m)
    }
}

/// `mm:ss` for under-an-hour, `hh:mm:ss` for ≥1h. Used by the
/// running timer's live ticker.
pub fn format_duration_hms(seconds: i64) -> String {
    let s = seconds.max(0);
    let h = s / 3600;
    let m = (s % 3600) / 60;
    let sec = s % 60;
    if h > 0 {
        format!("{:02}:{:02}:{:02}", h, m, sec)
    } else {
        format!("{:02}:{:02}", m, sec)
    }
}

/// `$1,234.56`, `€340.00`. v1: only USD/EUR/GBP symbols; everything
/// else falls back to a three-letter prefix.
pub fn format_money(cents: i64, currency: &str) -> String {
    let symbol = match currency.to_ascii_uppercase().as_str() {
        "USD" => "$",
        "EUR" => "€",
        "GBP" => "£",
        _ => "",
    };
    let neg = cents < 0;
    let abs = cents.unsigned_abs() as i128;
    let whole = abs / 100;
    let cents_part = abs % 100;
    // Thousand-separator the whole part.
    let mut w = whole.to_string();
    let bytes: Vec<char> = w.chars().collect();
    w.clear();
    for (i, c) in bytes.iter().rev().enumerate() {
        if i > 0 && i % 3 == 0 {
            w.insert(0, ',');
        }
        w.insert(0, *c);
    }
    let sign = if neg { "-" } else { "" };
    if symbol.is_empty() {
        format!("{sign}{} {}.{:02}", currency.to_uppercase(), w, cents_part)
    } else {
        format!("{sign}{symbol}{}.{:02}", w, cents_part)
    }
}

/// Seconds a single entry took (clamped non-negative). Running
/// entries are measured against `now`.
pub fn entry_seconds(entry: &TimeEntry, now: DateTime<Utc>) -> i64 {
    let end = entry.end_time.unwrap_or(now);
    (end - entry.start_time).num_seconds().max(0)
}

/// Group entries by their local-naive start date, sorted newest-day-first.
/// Within a day, entries themselves are sorted by start_time descending.
pub fn bucket_by_day(entries: &[TimeEntry]) -> Vec<(NaiveDate, Vec<TimeEntry>)> {
    use std::collections::BTreeMap;
    let mut by_day: BTreeMap<NaiveDate, Vec<TimeEntry>> = BTreeMap::new();
    for e in entries {
        let d = e.start_time.date_naive();
        by_day.entry(d).or_default().push(e.clone());
    }
    let mut out: Vec<(NaiveDate, Vec<TimeEntry>)> = by_day.into_iter().collect();
    out.sort_by(|a, b| b.0.cmp(&a.0));
    for (_, day) in &mut out {
        day.sort_by(|a, b| b.start_time.cmp(&a.start_time));
    }
    out
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
pub struct DayTotals {
    pub date: NaiveDate,
    pub total_seconds: i64,
    pub billable_seconds: i64,
    pub billable_cents: i64,
}

/// Per-day totals for a week starting on `week_start` (Mon by
/// convention but caller controls). Index 0 = `week_start`.
pub fn weekly_totals(entries: &[TimeEntry], week_start: NaiveDate) -> [DayTotals; 7] {
    let mut out: [DayTotals; 7] = std::array::from_fn(|i| DayTotals {
        date: week_start + Duration::days(i as i64),
        ..Default::default()
    });
    let now = Utc::now();
    for e in entries {
        let d = e.start_time.date_naive();
        let offset = (d - week_start).num_days();
        if (0..7).contains(&offset) {
            let idx = offset as usize;
            let secs = entry_seconds(e, now);
            out[idx].total_seconds += secs;
            if e.billable {
                out[idx].billable_seconds += secs;
                let rate = e.billable_rate_cents.unwrap_or(0) as i64;
                // rate is cents-per-hour
                out[idx].billable_cents += rate * secs / 3600;
            }
        }
    }
    out
}

/// Monday of the week containing `date`. Used by the week report
/// navigator and stats bucketing.
pub fn week_start(date: NaiveDate) -> NaiveDate {
    let dow = date.weekday().num_days_from_monday() as i64;
    date - Duration::days(dow)
}
