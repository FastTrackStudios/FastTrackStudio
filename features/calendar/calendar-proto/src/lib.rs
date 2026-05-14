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
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::EventTitle"))]
    pub title: String,

    #[architect(fulltext)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::lorem::en::Paragraph(1..3)")
    )]
    pub description: Option<String>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AroundNowDateTime"))]
    pub start_at: DateTime<Utc>,

    #[architect(filterable, sortable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::AroundNowDateTime"))]
    pub end_at: DateTime<Utc>,

    #[architect(filterable)]
    pub all_day: bool,

    /// FK to `location` feature.
    #[architect(filterable)]
    pub location_id: Option<Uuid>,

    /// Free-form fallback when no `location_id` is set.
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "fake::faker::address::en::CityName()")
    )]
    pub location_text: Option<String>,

    /// RFC 5545 recurrence rule, free-form for now.
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::RRule"))]
    pub rrule: Option<String>,

    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "fake::faker::name::en::Name()"))]
    pub organizer: Option<String>,

    #[architect(json)]
    #[cfg_attr(
        feature = "fake",
        dummy(faker = "(fake::faker::name::en::Name(), 1..6)")
    )]
    pub attendees: Vec<String>,

    /// External calendar source id (caldav).
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::CalendarIdRef"))]
    pub calendar_id: Option<String>,

    /// Lifecycle: `"confirmed"` / `"tentative"` / `"cancelled"`.
    #[architect(filterable)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::EventStatus"))]
    pub status: String,

    #[architect(json)]
    #[cfg_attr(feature = "fake", dummy(faker = "crate::fakers::EventTags"))]
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

#[cfg(feature = "fake")]
pub mod fakers {
    use chrono::{DateTime, Duration, Utc};
    use fake::Dummy;
    use fake::rand::{Rng, seq::IndexedRandom};

    fn pick<R: Rng + ?Sized>(rng: &mut R, values: &[&str]) -> String {
        (*values.choose(rng).unwrap()).to_string()
    }

    pub struct EventTitle;
    impl Dummy<EventTitle> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &EventTitle, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "Weekly Standup",
                    "Sprint Planning",
                    "Design Review",
                    "Roadmap Sync",
                    "Client Call",
                    "1:1 with Manager",
                    "Team Retro",
                    "Coffee Chat",
                    "Lunch & Learn",
                    "Studio Session",
                    "Mixing Session",
                    "Dentist Appointment",
                    "Doctor Appointment",
                    "Yoga Class",
                    "Flight to NYC",
                ],
            )
        }
    }

    pub struct AroundNowDateTime;
    impl Dummy<AroundNowDateTime> for DateTime<Utc> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &AroundNowDateTime, rng: &mut R) -> Self {
            // ±90 days
            let offset_min: i64 = rng.random_range(-129_600..129_600);
            Utc::now() + Duration::minutes(offset_min)
        }
    }

    pub struct RRule;
    impl Dummy<RRule> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &RRule, rng: &mut R) -> Self {
            pick(
                rng,
                &[
                    "FREQ=DAILY",
                    "FREQ=WEEKLY;BYDAY=MO",
                    "FREQ=WEEKLY;BYDAY=TU,TH",
                    "FREQ=MONTHLY;BYMONTHDAY=1",
                    "FREQ=YEARLY",
                    "FREQ=WEEKLY;BYDAY=MO,WE,FR;COUNT=10",
                ],
            )
        }
    }

    pub struct CalendarIdRef;
    impl Dummy<CalendarIdRef> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &CalendarIdRef, rng: &mut R) -> Self {
            pick(rng, &["work", "personal", "family", "shared", "holidays"])
        }
    }

    pub struct EventStatus;
    impl Dummy<EventStatus> for String {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &EventStatus, rng: &mut R) -> Self {
            pick(rng, &["confirmed", "tentative", "cancelled"])
        }
    }

    pub struct EventTags;
    impl Dummy<EventTags> for Vec<String> {
        fn dummy_with_rng<R: Rng + ?Sized>(_: &EventTags, rng: &mut R) -> Self {
            const POOL: &[&str] = &[
                "work",
                "personal",
                "travel",
                "health",
                "family",
                "urgent",
                "recurring",
                "all-hands",
            ];
            let n = rng.random_range(1..=3usize);
            POOL.choose_multiple(rng, n)
                .map(|s| s.to_string())
                .collect()
        }
    }
}
