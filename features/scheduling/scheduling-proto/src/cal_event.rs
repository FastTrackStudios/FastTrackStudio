//! Persisted calendar event — what the user drops onto the schedule.
//!
//! Distinct from [`crate::EventType`] (a cal.com-style bookable
//! template) and [`crate::DayPlan`] blocks (the day's structure): a
//! `CalEvent` is one concrete event on the timeline. The
//! `view-calendar` UI type maps onto this for storage. Times are
//! RFC-3339 strings and `color` is the `ColorTag` name so the proto
//! stays serializer-agnostic and `Facet`-encodable.

use facet::Facet;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Facet, Serialize, Deserialize)]
pub struct CalEvent {
    /// Stable id (uuid string).
    pub id: String,
    pub title: String,
    /// Inclusive start, RFC-3339.
    pub start: String,
    /// Exclusive end, RFC-3339.
    pub end: String,
    pub all_day: bool,
    /// `ColorTag` name (`"primary"`, `"success"`, …).
    pub color: String,
    pub description: Option<String>,
    /// RFC-5545 RRULE, if recurring.
    pub recurrence: Option<String>,
}
