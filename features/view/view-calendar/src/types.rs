//! Core data types — flat single-calendar model. Times are
//! `DateTime<Utc>` on the wire and rendered in the user's local
//! timezone at the component boundary (defer real TZ handling).

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use uuid::Uuid;

pub type EventId = Uuid;

/// View mode controlled by the root `Calendar` toolbar.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
#[serde(rename_all = "lowercase")]
pub enum ViewMode {
    Month,
    #[default]
    Week,
    Day,
}

impl ViewMode {
    pub fn label(self) -> &'static str {
        match self {
            Self::Month => "Month",
            Self::Week => "Week",
            Self::Day => "Day",
        }
    }
}

/// Event color tag — same vocabulary as view-kanban so consumers
/// can share status palettes. Defer per-event hex colors until
/// the design system says we need them.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Serialize, Deserialize, Default)]
#[serde(rename_all = "snake_case")]
pub enum ColorTag {
    Neutral,
    #[default]
    Primary,
    Success,
    Warning,
    Danger,
    Info,
}

impl ColorTag {
    /// Tailwind color stem (e.g. `"violet"`).
    pub fn stem(self) -> &'static str {
        match self {
            Self::Neutral => "slate",
            Self::Primary => "violet",
            Self::Success => "emerald",
            Self::Warning => "amber",
            Self::Danger => "rose",
            Self::Info => "sky",
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct CalendarEvent {
    pub id: EventId,
    pub title: String,
    /// Inclusive event start (UTC).
    pub start: DateTime<Utc>,
    /// Exclusive event end (UTC). For all-day events this is
    /// midnight of the day *after* the last day shown.
    pub end: DateTime<Utc>,
    #[serde(default)]
    pub all_day: bool,
    #[serde(default)]
    pub color: ColorTag,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
}

impl CalendarEvent {
    pub fn new(title: impl Into<String>, start: DateTime<Utc>, end: DateTime<Utc>) -> Self {
        Self {
            id: Uuid::new_v4(),
            title: title.into(),
            start,
            end,
            all_day: false,
            color: ColorTag::default(),
            description: None,
        }
    }

    /// Duration in whole minutes. Used by the time-grid views to
    /// compute the rendered height.
    pub fn duration_minutes(&self) -> i64 {
        (self.end - self.start).num_minutes().max(0)
    }
}
