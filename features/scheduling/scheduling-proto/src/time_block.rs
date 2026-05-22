//! Personal day-template types.
//!
//! A [`DayTemplate`] is an ordered list of [`TimeBlock`]s covering a
//! 24-hour day. The user's daily routine — the table from the
//! feature brief — round-trips through this shape. The vault stores
//! one template per markdown file (e.g.
//! `scheduling/templates/weekday.md`); per-day overrides live on
//! booking-style entries that point at a template + a date.

use facet::Facet;

/// Stable id for a template (uuid v4 string, minted by the consumer
/// so the proto stays serializer-agnostic).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct DayTemplateId(pub String);

/// Stable id for a single block within a template.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct TimeBlockId(pub String);

/// Time of day as minutes-since-midnight. 0..=1440. We avoid
/// `chrono::NaiveTime` on the wire so callers don't have to depend
/// on chrono just to ship a payload.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
pub struct TimeOfDay {
    pub minutes_since_midnight: u16,
}

impl TimeOfDay {
    #[must_use]
    pub const fn new(hours: u8, minutes: u8) -> Self {
        Self {
            minutes_since_midnight: hours as u16 * 60 + minutes as u16,
        }
    }

    #[must_use]
    pub const fn hours(self) -> u8 {
        (self.minutes_since_midnight / 60) as u8
    }

    #[must_use]
    pub const fn minutes(self) -> u8 {
        (self.minutes_since_midnight % 60) as u8
    }
}

/// Coarse categorization of a block. Drives color + grouping in
/// the UI. The set tracks the brief's table — every block in the
/// example schedule maps to exactly one of these.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum BlockCategory {
    /// Wake-up routine, morning reset.
    Reset,
    /// Spiritual / journaling / meditation time.
    Spiritual,
    /// Cooking, eating, cleanup. Covers breakfast / lunch / dinner.
    Meal,
    /// Movement, gym, walks.
    Exercise,
    /// Personal hygiene, grooming, getting ready.
    Hygiene,
    /// One of the three allocatable Work / Event / Free-Time blocks.
    /// The brief calls these "Block 1 / 2 / 3" — the UI labels
    /// them by ordinal.
    Allocatable,
    /// Errands, chores, low-energy administrative work.
    Maintenance,
    /// Evening wind-down, no-screens, prep for bed.
    WindDown,
    /// Sleep.
    Sleep,
    /// Catch-all for blocks that don't fit the above. The UI
    /// renders them as a neutral chip.
    Other,
}

/// One row in the day template.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct TimeBlock {
    pub id: TimeBlockId,
    pub start: TimeOfDay,
    pub end: TimeOfDay,
    pub label: String,
    pub category: BlockCategory,
    /// Optional free-text note (e.g. "Block 1 — deep work" or
    /// "Block 3 — Tuesday only").
    pub note: Option<String>,
}

/// Personal day template — an ordered list of blocks that should
/// cover the whole 24-hour day. The UI surfaces gaps + overlaps;
/// the proto leaves validation to the consumer so storing
/// partially-filled drafts works.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct DayTemplate {
    pub id: DayTemplateId,
    pub name: String,
    /// Optional description (e.g. "Weekday routine" /
    /// "Saturday — long-form work").
    pub description: Option<String>,
    pub blocks: Vec<TimeBlock>,
}
