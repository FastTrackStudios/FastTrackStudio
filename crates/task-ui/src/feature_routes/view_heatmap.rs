//! `/views/heatmap` — stub demo of the `view-heatmap` crate.

use chrono::{Days, Local, NaiveDate};
use dioxus::prelude::*;
use fts_ui::prelude::*;
use view::heatmap::{ColorTag, Heatmap, HeatmapStyle};

#[component]
pub fn HeatmapView() -> Element {
    let mut style = use_signal(|| HeatmapStyle::Grid);
    let today = Local::now().date_naive();
    let points = seed_points(today);
    let style_now = *style.read();

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] p-4 flex flex-col gap-3 overflow-auto",
            div { class: "flex items-center gap-2",
                Heading { level: HeadingLevel::H1, "Heatmap" }
                Spacer {}
                StyleSwitch { current: style_now, on_change: move |s: HeatmapStyle| style.set(s) }
            }
            Text { variant: TextVariant::Muted,
                match style_now {
                    HeatmapStyle::Grid => "53 weeks × 7 days. Color intensity scales by activity count per day — same vocabulary as GitHub's contribution graph.",
                    HeatmapStyle::Bars => "One bar per day for the visible week. Chevron-nav backward through history.",
                    HeatmapStyle::Cyclic => "Cyclic planning: 4 quarters × (3 cycles of 4 weeks + reset week). Same number of every weekday in every cycle — built for routine-building. Cyclic leap years get a bonus W0.",
                }
            }
            div { class: "p-3 border border-border/60 rounded-lg",
                Heatmap {
                    style: style_now,
                    points,
                    color: ColorTag::Success,
                    anchor: today,
                }
            }
        }
    }
}

#[derive(Props, Clone, PartialEq)]
struct StyleSwitchProps {
    current: HeatmapStyle,
    on_change: EventHandler<HeatmapStyle>,
}

#[component]
fn StyleSwitch(props: StyleSwitchProps) -> Element {
    let opts = [HeatmapStyle::Grid, HeatmapStyle::Cyclic, HeatmapStyle::Bars];
    rsx! {
        div { class: "inline-flex rounded-md border border-border/60 overflow-hidden text-xs",
            for s in opts.iter() {
                {
                    let s = *s;
                    let active = s == props.current;
                    let cls = if active {
                        "bg-accent text-accent-foreground px-2.5 py-1"
                    } else {
                        "hover:bg-accent/50 text-muted-foreground px-2.5 py-1"
                    };
                    let label = match s {
                        HeatmapStyle::Grid => "Year grid",
                        HeatmapStyle::Bars => "Weekly bars",
                        HeatmapStyle::Cyclic => "Cycles",
                    };
                    rsx! {
                        button {
                            key: "{label}",
                            r#type: "button",
                            class: "{cls}",
                            onclick: move |_| props.on_change.call(s),
                            "{label}"
                        }
                    }
                }
            }
        }
    }
}

fn seed_points(today: NaiveDate) -> Vec<(NaiveDate, u32)> {
    // Pseudo-random reproducible pattern — denser on weekdays,
    // sparser on weekends. Trails off the further back in time we
    // go so the grid has a visible "ramp" from less to more recent.
    use chrono::Datelike;
    let mut out = Vec::with_capacity(365);
    for i in 0..370i64 {
        let day = today - Days::new(i as u64);
        // Cheap deterministic hash from the date.
        let h = (day.num_days_from_ce() as u64).wrapping_mul(2_654_435_761);
        let weekday_bias = if matches!(day.weekday(), chrono::Weekday::Sat | chrono::Weekday::Sun) {
            1
        } else {
            3
        };
        let recency_bias = (370 - i) as u32 / 80; // 0..4 ramp
        let n = ((h % 5) as u32 + weekday_bias).saturating_add(recency_bias) % 9;
        if n > 0 {
            out.push((day, n));
        }
    }
    out
}
