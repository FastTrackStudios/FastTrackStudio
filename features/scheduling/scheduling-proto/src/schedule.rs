//! Availability schedules and slot queries.
//!
//! [`AvailabilitySchedule`] is a named bundle of weekly
//! [`AvailabilityRule`]s the user maintains ("Working hours",
//! "Open-source weekends"). Slot generation walks the rules
//! intersected with the user's existing [`crate::booking::Booking`]
//! list to produce the free [`TimeSlot`]s the public booking page
//! shows.

use facet::Facet;

use crate::time_block::TimeOfDay;

/// Stable id for an availability schedule (uuid v4 string).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct ScheduleId(pub String);

/// Day of week. Mon..Sun rather than Sun..Sat — same convention as
/// view-calendar, keeps the rest of the app consistent.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum Weekday {
    Mon,
    Tue,
    Wed,
    Thu,
    Fri,
    Sat,
    Sun,
}

/// One rule: on these days, this start..end window is available.
/// Multiple rules can apply to the same day (e.g. "Mon 9–12 + Mon
/// 13–17" for a lunchtime gap).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct AvailabilityRule {
    pub days: Vec<Weekday>,
    pub start: TimeOfDay,
    pub end: TimeOfDay,
}

/// Named bundle of rules + an optional IANA timezone identifier.
/// Slot generation interprets the rules in this timezone.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct AvailabilitySchedule {
    pub id: ScheduleId,
    /// Vault-relative path of the markdown file backing this
    /// schedule (populated by the scanner).
    pub path: String,
    pub name: String,
    /// e.g. "America/Chicago". `None` = host's local timezone.
    pub timezone: Option<String>,
    pub rules: Vec<AvailabilityRule>,
}

/// Inclusive start, exclusive end. Both are ISO-8601 UTC strings
/// (`2026-05-22T14:30:00Z`) — we use strings on the wire so
/// chrono / time stays a consumer-side concern.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct TimeSlot {
    pub start_utc: String,
    pub end_utc: String,
}

/// Slot-listing parameters for the public booking page.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct SlotQuery {
    pub event_type_id: crate::event_type::EventTypeId,
    /// Inclusive UTC start of the search window.
    pub from_utc: String,
    /// Exclusive UTC end.
    pub to_utc: String,
}
