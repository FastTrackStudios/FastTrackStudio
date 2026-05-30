//! `/schedule` — calendar with the daily-plan template overlaid as
//! faded "ghost" outlines.
//!
//! Loads the selected org's day-plan templates (`weekday` / `weekend`)
//! via [`crate::feeds::fetch_day_templates`], converts each
//! [`scheduling_proto::TimeBlock`] into a read-only
//! [`view_calendar::TemplateBlock`], and feeds them to the calendar as
//! a recurring background guide the user drops real events onto.
//!
//! Real events are still in-memory only (no persistence yet) — the
//! component is storage-agnostic, so wiring events to a service is a
//! follow-up. The overlay is the point of this page.

use chrono::Weekday;
use dioxus::prelude::*;
use fts_ui::prelude::*;
use scheduling_proto::{BlockCategory, DayTemplate};
use view_calendar::{Calendar, CalendarState, ColorTag, TemplateBlock, ViewMode, apply};

use crate::orgs::{OrgMeta, OrgSelection};

const WEEKDAYS: [Weekday; 5] = [
    Weekday::Mon,
    Weekday::Tue,
    Weekday::Wed,
    Weekday::Thu,
    Weekday::Fri,
];
const WEEKEND: [Weekday; 2] = [Weekday::Sat, Weekday::Sun];

#[component]
pub fn ScheduleView() -> Element {
    let selection = use_context::<Signal<OrgSelection>>();
    let org_list = use_context::<Signal<Vec<OrgMeta>>>();

    // The day-plan lives per-org; the schedule is personal, so we read
    // it from the first selected org (or the home org in "All" mode).
    let templates = use_resource(move || async move {
        let slugs = crate::orgs::selected_slugs(&selection.read(), &org_list.read());
        match slugs.first() {
            Some(slug) => crate::feeds::fetch_day_templates(slug).await,
            None => Ok(Vec::new()),
        }
    });

    // Real events are local-only for now; the user adds them on top of
    // the template outlines.
    let mut state = use_signal(CalendarState::default);
    let events = state.read().events.values().cloned().collect::<Vec<_>>();

    let template_blocks = match &*templates.read_unchecked() {
        Some(Ok(tpls)) => template_blocks_from(tpls),
        _ => Vec::new(),
    };

    let banner = match &*templates.read_unchecked() {
        Some(Err(e)) => Some(rsx! {
            div { class: "rounded-md border border-amber-400/40 bg-amber-500/10 px-3 py-1.5 text-xs text-amber-200",
                "Couldn't load the day-plan overlay: {e}"
            }
        }),
        Some(Ok(t)) if t.is_empty() => Some(rsx! {
            Text {
                variant: TextVariant::Muted,
                "No day-plan templates found under Projects/Scheduling/templates/."
            }
        }),
        _ => None,
    };

    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] lg:h-screen p-4 flex flex-col gap-3 overflow-hidden",
            if let Some(b) = banner {
                {b}
            }
            Calendar {
                events,
                template_blocks,
                initial_view: Some(ViewMode::Week),
                on_event: move |mu| apply(&mut state.write(), &mu),
            }
        }
    }
}

/// Build the overlay from the org's templates: `weekday` blocks recur
/// Mon–Fri, `weekend` blocks Sat–Sun. A lone template (any id) applies
/// to every day. Unknown ids are ignored.
fn template_blocks_from(templates: &[DayTemplate]) -> Vec<TemplateBlock> {
    if templates.len() == 1 {
        return blocks_for(&templates[0], &every_day());
    }
    let mut out = Vec::new();
    for dt in templates {
        let days: &[Weekday] = match dt.id.0.as_str() {
            "weekday" => &WEEKDAYS,
            "weekend" => &WEEKEND,
            _ => continue,
        };
        out.extend(blocks_for(dt, days));
    }
    out
}

fn every_day() -> Vec<Weekday> {
    WEEKDAYS.iter().chain(WEEKEND.iter()).copied().collect()
}

/// Convert one template's blocks to calendar overlays, splitting any
/// block that wraps past midnight (e.g. sleep 22:30–06:00) into two
/// same-day blocks so each renders on the right column.
fn blocks_for(dt: &DayTemplate, weekdays: &[Weekday]) -> Vec<TemplateBlock> {
    let mut out = Vec::new();
    for b in &dt.blocks {
        let start = b.start.minutes_since_midnight;
        let end = b.end.minutes_since_midnight;
        let color = category_color(b.category);
        if end <= start {
            if start < 1440 {
                out.push(TemplateBlock {
                    label: b.label.clone(),
                    start_min: start,
                    end_min: 1440,
                    color,
                    weekdays: weekdays.to_vec(),
                });
            }
            if end > 0 {
                out.push(TemplateBlock {
                    label: b.label.clone(),
                    start_min: 0,
                    end_min: end,
                    color,
                    weekdays: weekdays.to_vec(),
                });
            }
        } else {
            out.push(TemplateBlock {
                label: b.label.clone(),
                start_min: start,
                end_min: end,
                color,
                weekdays: weekdays.to_vec(),
            });
        }
    }
    out
}

/// Tint each category to a calendar color. The three allocatable work
/// blocks get the strongest (emerald) accent; fixed routine wrappers
/// get muted, day-appropriate tints.
fn category_color(c: BlockCategory) -> ColorTag {
    match c {
        BlockCategory::Allocatable => ColorTag::Success,
        BlockCategory::Reset | BlockCategory::Maintenance => ColorTag::Info,
        BlockCategory::Spiritual | BlockCategory::WindDown => ColorTag::Primary,
        BlockCategory::Meal => ColorTag::Warning,
        BlockCategory::Exercise => ColorTag::Danger,
        BlockCategory::Hygiene | BlockCategory::Sleep | BlockCategory::Other => ColorTag::Neutral,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use scheduling_proto::{DayTemplateId, TimeBlock, TimeBlockId, TimeOfDay};

    fn block(label: &str, start: u16, end: u16, category: BlockCategory) -> TimeBlock {
        TimeBlock {
            id: TimeBlockId(label.into()),
            start: TimeOfDay {
                minutes_since_midnight: start,
            },
            end: TimeOfDay {
                minutes_since_midnight: end,
            },
            label: label.into(),
            category,
            note: None,
        }
    }

    fn template(id: &str, blocks: Vec<TimeBlock>) -> DayTemplate {
        DayTemplate {
            id: DayTemplateId(id.into()),
            name: id.into(),
            description: None,
            blocks,
        }
    }

    #[test]
    fn midnight_crossing_block_splits_in_two() {
        // Sleep 22:30 → 06:00 wraps midnight.
        let dt = template(
            "weekday",
            vec![block("Sleep", 22 * 60 + 30, 6 * 60, BlockCategory::Sleep)],
        );
        let blocks = blocks_for(&dt, &WEEKDAYS);
        assert_eq!(blocks.len(), 2);
        // Evening segment runs to end-of-day; morning segment starts at 0.
        assert_eq!((blocks[0].start_min, blocks[0].end_min), (1350, 1440));
        assert_eq!((blocks[1].start_min, blocks[1].end_min), (0, 360));
    }

    #[test]
    fn normal_block_passes_through() {
        let dt = template(
            "weekday",
            vec![block(
                "Block 1",
                9 * 60 + 30,
                12 * 60 + 30,
                BlockCategory::Allocatable,
            )],
        );
        let blocks = blocks_for(&dt, &WEEKDAYS);
        assert_eq!(blocks.len(), 1);
        assert_eq!((blocks[0].start_min, blocks[0].end_min), (570, 750));
        assert_eq!(blocks[0].color, ColorTag::Success);
        assert_eq!(blocks[0].weekdays, WEEKDAYS.to_vec());
    }

    #[test]
    fn weekday_and_weekend_templates_map_to_their_days() {
        let tpls = vec![
            template(
                "weekday",
                vec![block("Gym", 8 * 60, 9 * 60, BlockCategory::Exercise)],
            ),
            template(
                "weekend",
                vec![block("Brunch", 10 * 60, 11 * 60, BlockCategory::Meal)],
            ),
        ];
        let blocks = template_blocks_from(&tpls);
        let gym = blocks.iter().find(|b| b.label == "Gym").unwrap();
        let brunch = blocks.iter().find(|b| b.label == "Brunch").unwrap();
        assert_eq!(gym.weekdays, WEEKDAYS.to_vec());
        assert_eq!(brunch.weekdays, WEEKEND.to_vec());
    }
}
