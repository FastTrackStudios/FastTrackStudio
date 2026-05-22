//! `/scheduling` — stub demo of the `scheduling` feature.
//!
//! Renders a seeded `DayTemplate` matching the feature brief's
//! example schedule. Event-type / availability / booking surfaces
//! land in follow-up commits.

use dioxus::prelude::*;
use fts_ui::prelude::*;
use scheduling_proto::{
    BlockCategory, DayTemplate, DayTemplateId, TimeBlock, TimeBlockId, TimeOfDay,
};
use scheduling_ui::DayTemplateView;

#[component]
pub fn SchedulingView() -> Element {
    let template = seed_weekday();
    rsx! {
        div { class: "h-[calc(100vh-3.5rem)] p-4 flex flex-col gap-3 overflow-auto",
            div { class: "flex items-baseline gap-3",
                Heading { level: HeadingLevel::H1, "Scheduling" }
                Text { variant: TextVariant::Muted,
                    "Personal day templates + cal.com-style bookable event types. v1 cut — read-only view of a seeded weekday template."
                }
            }
            DayTemplateView { template }
        }
    }
}

fn seed_weekday() -> DayTemplate {
    let block =
        |id: &str, label: &str, start: (u8, u8), end: (u8, u8), cat: BlockCategory| TimeBlock {
            id: TimeBlockId(id.to_string()),
            start: TimeOfDay::new(start.0, start.1),
            end: TimeOfDay::new(end.0, end.1),
            label: label.to_string(),
            category: cat,
            note: None,
        };

    DayTemplate {
        id: DayTemplateId("weekday".into()),
        name: "Weekday routine".into(),
        description: Some(
            "3 allocatable blocks · 30 min spiritual · 1 h gym · 3 home meals · 7.5 h sleep".into(),
        ),
        blocks: vec![
            block(
                "morning-reset",
                "Morning Reset",
                (6, 0),
                (6, 30),
                BlockCategory::Reset,
            ),
            block(
                "spiritual",
                "Spiritual Time with God",
                (6, 30),
                (7, 0),
                BlockCategory::Spiritual,
            ),
            block(
                "breakfast-prep",
                "Breakfast prep",
                (7, 0),
                (7, 30),
                BlockCategory::Meal,
            ),
            block(
                "breakfast",
                "Breakfast + quick cleanup",
                (7, 30),
                (8, 0),
                BlockCategory::Meal,
            ),
            block(
                "gym",
                "Gym: walk/run there, workout, walk/run back",
                (8, 0),
                (9, 0),
                BlockCategory::Exercise,
            ),
            block(
                "shower",
                "Shower + get ready",
                (9, 0),
                (9, 30),
                BlockCategory::Hygiene,
            ),
            block(
                "block-1",
                "Block 1: Work / Event / Free Time",
                (9, 30),
                (12, 30),
                BlockCategory::Allocatable,
            ),
            block(
                "lunch",
                "Lunch prep + lunch + quick cleanup",
                (12, 30),
                (13, 30),
                BlockCategory::Meal,
            ),
            block(
                "block-2",
                "Block 2: Work / Event / Free Time",
                (13, 30),
                (16, 30),
                BlockCategory::Allocatable,
            ),
            block(
                "maintenance",
                "Maintenance Hour",
                (16, 30),
                (17, 30),
                BlockCategory::Maintenance,
            ),
            block(
                "dinner",
                "Dinner prep + dinner + quick cleanup",
                (17, 30),
                (19, 0),
                BlockCategory::Meal,
            ),
            block(
                "block-3",
                "Block 3: Work / Event / Free Time",
                (19, 0),
                (22, 0),
                BlockCategory::Allocatable,
            ),
            block(
                "wind-down",
                "Wind down",
                (22, 0),
                (22, 30),
                BlockCategory::WindDown,
            ),
            block(
                "sleep",
                "Sleep — 7.5 hours",
                (22, 30),
                (6, 0),
                BlockCategory::Sleep,
            ),
        ],
    }
}
