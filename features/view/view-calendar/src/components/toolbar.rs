//! Top toolbar — prev/next/today buttons, current range label,
//! view-mode switch (Month/Week/Day), and a new-event button.

use chrono::{Datelike, NaiveDate, Weekday};
use cycle::{FirstWeekRule, cycle_for_date, generate_year};
use dioxus::prelude::*;
use fts_ui::lucide_dioxus::{ChevronLeft, ChevronRight, Plus};
use fts_ui::prelude::*;

use crate::time::{week_days, week_start};
use crate::types::ViewMode;

#[derive(Props, Clone, PartialEq)]
pub struct ToolbarProps {
    pub anchor: NaiveDate,
    pub view: ViewMode,
    pub on_prev: EventHandler<()>,
    pub on_next: EventHandler<()>,
    pub on_today: EventHandler<()>,
    pub on_view_change: EventHandler<ViewMode>,
    pub on_create: EventHandler<()>,
    #[props(default = false)]
    pub readonly: bool,
}

#[component]
pub fn Toolbar(props: ToolbarProps) -> Element {
    let label = range_label(props.anchor, props.view);
    let cycle_label = cycle_label(props.anchor);

    rsx! {
        div { class: "flex items-center gap-2 px-3 py-2 border-b border-border/40",
            Button {
                variant: ButtonVariant::Outline,
                size: ButtonSize::Small,
                on_click: move |_| props.on_today.call(()),
                "Today"
            }
            div { class: "flex items-center",
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| props.on_prev.call(()),
                    ChevronLeft { size: 16 }
                }
                Button {
                    variant: ButtonVariant::Ghost,
                    size: ButtonSize::Small,
                    on_click: move |_| props.on_next.call(()),
                    ChevronRight { size: 16 }
                }
            }
            div { class: "flex flex-col px-2 leading-tight",
                Heading { level: HeadingLevel::H2, class: "text-base font-medium", "{label}" }
                if let Some(cl) = cycle_label.clone() {
                    span {
                        class: "text-xs text-muted-foreground tabular-nums",
                        "{cl}"
                    }
                }
            }
            Spacer {}
            ViewSwitch {
                view: props.view,
                on_change: props.on_view_change,
            }
            if !props.readonly {
                Button {
                    size: ButtonSize::Small,
                    on_click: move |_| props.on_create.call(()),
                    Plus { size: 14 }
                    "New event"
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct ViewSwitchProps {
    view: ViewMode,
    on_change: EventHandler<ViewMode>,
}

#[component]
fn ViewSwitch(props: ViewSwitchProps) -> Element {
    let opts = [ViewMode::Day, ViewMode::Week, ViewMode::Month];
    rsx! {
        div { class: "inline-flex items-center gap-0.5 rounded-lg bg-muted/40 p-0.5 text-xs",
            for v in opts.iter() {
                {
                    let v = *v;
                    let active = v == props.view;
                    let cls = if active {
                        "bg-background text-foreground shadow-sm font-medium px-3 py-1 rounded-md transition-all"
                    } else {
                        "text-muted-foreground hover:text-foreground px-3 py-1 rounded-md transition-colors"
                    };
                    let key = v.label();
                    rsx! {
                        button {
                            key: "{key}",
                            r#type: "button",
                            class: "{cls}",
                            onclick: move |_| props.on_change.call(v),
                            "{v.label()}"
                        }
                    }
                }
            }
        }
    }
}

/// Cycle position for the anchor date — `"2026 Q2 C2 · W4"`
/// for mid-cycle, `"2026 Q2 reset week"` during a reset, or
/// `"2026 bonus week"` for the cyclic-leap bonus. `None` when
/// the date falls outside any generated calendar (shouldn't
/// happen in practice; defensive).
///
/// Hardcoded Monday-start + ≥4-day rule for now. When user
/// preferences land, route those in via props instead.
fn cycle_label(anchor: NaiveDate) -> Option<String> {
    let rule = FirstWeekRule::AtLeastFourDaysInYear;
    if let Some(c) = cycle_for_date(anchor, Weekday::Mon, rule) {
        let week_in_cycle = ((anchor - c.start_date).num_days() / 7) as u8 + 1;
        return Some(format!(
            "{} Q{} C{} · W{}/4",
            c.year, c.quarter, c.ordinal, week_in_cycle,
        ));
    }
    // Between cycles → reset or bonus week. Walk the year's
    // quarters to find which.
    for cyclic_year in [anchor.year() - 1, anchor.year(), anchor.year() + 1] {
        for q in generate_year(cyclic_year, Weekday::Mon, rule) {
            if anchor >= q.reset_week_start && anchor <= q.reset_week_end {
                return Some(format!("{} Q{} · reset week", q.year, q.ordinal));
            }
            if let (Some(s), Some(e)) = (q.bonus_week_start, q.bonus_week_end) {
                if anchor >= s && anchor <= e {
                    return Some(format!("{} bonus · week zero for {}", q.year, q.year + 1));
                }
            }
        }
    }
    None
}

/// Header label for the current range:
/// - Month → "March 2026"
/// - Week  → "Mar 2 – 8, 2026" (collapsed to "Feb 28 – Mar 6" across months)
/// - Day   → "Tue, Mar 3, 2026"
fn range_label(anchor: NaiveDate, view: ViewMode) -> String {
    match view {
        ViewMode::Month => anchor.format("%B %Y").to_string(),
        ViewMode::Week => {
            let days = week_days(week_start(anchor));
            let start = days[0];
            let end = days[6];
            if start.month() == end.month() {
                format!(
                    "{} {} – {}, {}",
                    start.format("%b"),
                    start.day(),
                    end.day(),
                    end.year()
                )
            } else {
                format!(
                    "{} – {}, {}",
                    start.format("%b %-d"),
                    end.format("%b %-d"),
                    end.year()
                )
            }
        }
        ViewMode::Day => anchor.format("%a, %b %-d, %Y").to_string(),
    }
}
