//! Top toolbar — prev/next/today buttons, current range label,
//! view-mode switch (Month/Week/Day), and a new-event button.

use chrono::{Datelike, NaiveDate};
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
            Heading { level: HeadingLevel::H2, class: "text-base font-medium px-2", "{label}" }
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
        div { class: "inline-flex rounded-md border border-border/60 overflow-hidden text-xs",
            for v in opts.iter() {
                {
                    let v = *v;
                    let active = v == props.view;
                    let cls = if active {
                        "bg-accent text-accent-foreground px-2.5 py-1"
                    } else {
                        "hover:bg-accent/50 text-muted-foreground px-2.5 py-1"
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
