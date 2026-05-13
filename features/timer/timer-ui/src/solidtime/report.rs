//! `TimerWeekReport` — per-day-of-week stacked horizontal bars, segmented by project.

use std::collections::HashMap;

use chrono::{Datelike, Duration, NaiveDate, Utc};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronLeft, ChevronRight};
use fts_ui::prelude::*;
use project_proto::Project;
use timer_proto::TimeEntry;
use uuid::Uuid;

use super::common::{entry_seconds, format_duration_compact, format_money};

#[derive(Props, Clone, PartialEq)]
pub struct TimerWeekReportProps {
    pub week_start: NaiveDate,
    pub entries: Vec<TimeEntry>,
    pub projects_by_id: HashMap<Uuid, Project>,
    pub on_week_change: EventHandler<NaiveDate>,
}

#[derive(Clone)]
struct Seg {
    project_id: Option<Uuid>,
    seconds: i64,
}

#[component]
pub fn TimerWeekReport(props: TimerWeekReportProps) -> Element {
    let week_start = props.week_start;
    let week_end = week_start + Duration::days(7);
    let now = Utc::now();

    // Build per-day, per-project totals.
    let mut by_day: [HashMap<Option<Uuid>, i64>; 7] = Default::default();
    let mut total = 0i64;
    let mut billable_secs = 0i64;
    let mut billable_cents = 0i64;
    for e in props.entries.iter() {
        let d = e.start_time.date_naive();
        if d >= week_start && d < week_end {
            let idx = (d - week_start).num_days() as usize;
            let secs = entry_seconds(e, now);
            *by_day[idx].entry(e.project_id).or_insert(0) += secs;
            total += secs;
            if e.billable {
                billable_secs += secs;
                let rate = e.billable_rate_cents.unwrap_or(0) as i64;
                billable_cents += rate * secs / 3600;
            }
        }
    }

    let day_totals: [i64; 7] = std::array::from_fn(|i| by_day[i].values().sum());
    let max_day = day_totals.iter().copied().max().unwrap_or(0).max(1);

    let label = format!("Week of {}", week_start.format("%b %-d"));

    let on_prev = {
        let on_change = props.on_week_change;
        move |_| on_change.call(week_start - Duration::days(7))
    };
    let on_next = {
        let on_change = props.on_week_change;
        move |_| on_change.call(week_start + Duration::days(7))
    };
    let on_today = {
        let on_change = props.on_week_change;
        move |_| {
            let today = Utc::now().date_naive();
            let dow = today.weekday().num_days_from_monday() as i64;
            on_change.call(today - Duration::days(dow));
        }
    };

    let days = ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"];

    rsx! {
        Card {
            CardHeader {
                HStack { class: "items-center justify-between",
                    CardTitle { "Week report" }
                    HStack { class: "items-center gap-1",
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: on_prev,
                            ChevronLeft { size: 14 }
                        }
                        span { class: "text-sm", "{label}" }
                        Button {
                            variant: ButtonVariant::Ghost,
                            size: ButtonSize::Small,
                            on_click: on_next,
                            ChevronRight { size: 14 }
                        }
                        Button {
                            variant: ButtonVariant::Outline,
                            size: ButtonSize::Small,
                            on_click: on_today,
                            "Today"
                        }
                    }
                }
            }
            CardContent {
                div { class: "flex flex-col gap-2",
                    for (i, day) in days.iter().enumerate() {
                        {
                            let mut segs: Vec<Seg> = by_day[i]
                                .iter()
                                .map(|(pid, secs)| Seg { project_id: *pid, seconds: *secs })
                                .collect();
                            segs.sort_by(|a, b| b.seconds.cmp(&a.seconds));
                            let day_total = day_totals[i];
                            let pct = (day_total as f64 / max_day as f64 * 100.0).round() as i64;
                            let projects = props.projects_by_id.clone();
                            rsx! {
                                div { key: "{i}", class: "flex items-center gap-3",
                                    div { class: "w-10 text-xs text-muted-foreground", "{day}" }
                                    div { class: "flex-1 relative h-6 rounded bg-muted/30 overflow-hidden",
                                        div {
                                            class: "absolute inset-y-0 left-0 flex",
                                            style: format!("width: {}%", pct.max(0)),
                                            for seg in segs.iter() {
                                                {
                                                    let seg_pct = if day_total == 0 { 0.0 } else { seg.seconds as f64 / day_total as f64 * 100.0 };
                                                    let proj = seg.project_id.and_then(|pid| projects.get(&pid));
                                                    let color = proj.and_then(|p| p.color.clone()).unwrap_or_else(|| "#888".into());
                                                    let pname = proj.map(|p| p.name.clone()).unwrap_or_else(|| "No project".to_string());
                                                    let dur = format_duration_compact(seg.seconds);
                                                    rsx! {
                                                        Tooltip {
                                                            TooltipTrigger {
                                                                div {
                                                                    class: "h-full",
                                                                    style: format!("width: {}%; background: {}", seg_pct, color),
                                                                }
                                                            }
                                                            TooltipContent { side: TooltipSide::Top, "{pname} · {dur}" }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                    div { class: "w-16 text-right font-mono text-xs tabular-nums text-muted-foreground",
                                        "{format_duration_compact(day_total)}"
                                    }
                                }
                            }
                        }
                    }

                    Divider {}

                    div { class: "flex justify-between text-xs",
                        span { class: "text-muted-foreground", "Total" }
                        span { class: "font-mono", "{format_duration_compact(total)}" }
                    }
                    div { class: "flex justify-between text-xs",
                        span { class: "text-muted-foreground", "Billable" }
                        span { class: "font-mono", "{format_duration_compact(billable_secs)}" }
                    }
                    div { class: "flex justify-between text-xs",
                        span { class: "text-muted-foreground", "Amount" }
                        span { class: "font-mono", "{format_money(billable_cents, \"USD\")}" }
                    }
                }
            }
        }
    }
}
