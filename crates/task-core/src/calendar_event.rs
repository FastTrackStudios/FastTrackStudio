use chrono::{DateTime, Utc};
use facet::Facet;

/// A first-class calendar event backed by iCalendar VEVENT.
#[derive(Debug, Clone, PartialEq, Default, Facet)]
pub struct CalendarEvent {
    pub id: Option<String>,
    pub title: String,
    pub description: Option<String>,
    pub location: Option<String>,
    pub start: DateTime<Utc>,
    pub end: Option<DateTime<Utc>>,
    #[facet(default)]
    pub all_day: bool,
    pub status: CalendarEventStatus,
    pub recurrence: Option<String>,
    #[facet(default)]
    pub attendees: Vec<String>,
    pub date_created: Option<DateTime<Utc>>,
    pub date_modified: Option<DateTime<Utc>>,
    pub external_id: Option<String>,
    pub external_source: Option<String>,
    #[facet(skip)]
    #[facet(default)]
    pub body: String,
}

#[derive(Debug, Clone, PartialEq, Default, Facet)]
#[repr(u8)]
pub enum CalendarEventStatus {
    #[default]
    Confirmed,
    Tentative,
    Cancelled,
}
