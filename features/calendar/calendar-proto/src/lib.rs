//! `calendar-proto` — wire contract for calendar events.

pub use architect;

use architect::Entity;
use chrono::{DateTime, Utc};
use uuid::Uuid;

#[cfg_attr(feature = "fake", derive(::fake::Dummy))]
#[derive(Entity, ::facet::Facet, Clone, Debug, PartialEq)]
#[architect(table_name = "calendar_events", repo)]
pub struct CalendarEvent {
    #[architect(primary_key, auto_increment = false, on_create = Uuid::new_v4())]
    pub id: Uuid,

    #[architect(filterable, sortable, fulltext)]
    pub title: String,

    #[architect(fulltext)]
    pub description: Option<String>,

    #[architect(filterable, sortable)]
    pub start_at: DateTime<Utc>,

    #[architect(filterable, sortable)]
    pub end_at: DateTime<Utc>,

    #[architect(filterable)]
    pub all_day: bool,

    /// FK to `location` feature.
    #[architect(filterable)]
    pub location_id: Option<Uuid>,

    /// Free-form fallback when no `location_id` is set.
    pub location_text: Option<String>,

    /// RFC 5545 recurrence rule, free-form for now.
    pub rrule: Option<String>,

    #[architect(filterable)]
    pub organizer: Option<String>,

    pub attendees: Vec<String>,

    /// External calendar source id (caldav).
    #[architect(filterable)]
    pub calendar_id: Option<String>,

    /// Lifecycle: `"confirmed"` / `"tentative"` / `"cancelled"`.
    #[architect(filterable)]
    pub status: String,

    pub tags: Vec<String>,

    #[architect(exclude(create, update), on_create = Utc::now())]
    pub created_at: DateTime<Utc>,

    #[architect(exclude(create, update), on_create = Utc::now(), on_update = Utc::now())]
    pub updated_at: DateTime<Utc>,
}

#[derive(Debug, Clone, PartialEq, Eq, ::facet::Facet, thiserror::Error)]
#[repr(u8)]
pub enum CalendarServiceError {
    #[error("not found")]
    NotFound,
    #[error("invalid input: {0}")]
    InvalidInput(String),
    #[error("internal error: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait CalendarService {
    async fn cancel_event(&self, event_id: Uuid) -> Result<(), CalendarServiceError>;
}
