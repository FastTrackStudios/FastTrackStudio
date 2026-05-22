//! `SchedulingService` — the canonical sync trait, decorated with
//! `#[architect::rpc]`.
//!
//! The macro derives the async vox face from this sync trait:
//! backends impl `SchedulingService` directly (zero-cost in-process
//! call sites), and remote callers reach the same surface via the
//! auto-emitted [`SchedulingServiceClient`] over vox. See
//! `architect/DESIGN.md`.
//!
//! The trait covers both halves of the feature:
//! - **Personal**: day-template CRUD.
//! - **Cal.com-style**: event-type / schedule / booking CRUD plus
//!   the slot-listing entry-point the public booking page calls.
//!
//! CalDAV sync sits *behind* this trait — a future
//! `SchedulingCaldav` backend implements `SchedulingService` and
//! mirrors writes to a remote CalDAV server, but the surface stays
//! the same shape for the UI.

use crate::booking::{Booking, BookingId, BookingStatus, NewBooking};
use crate::error::SchedulingError;
use crate::event_type::{EventType, EventTypeId};
use crate::schedule::{AvailabilitySchedule, ScheduleId, SlotQuery, TimeSlot};
use crate::time_block::{DayTemplate, DayTemplateId};

#[architect::rpc]
pub trait SchedulingService {
    // ── Personal: day templates ───────────────────────────────────
    fn list_day_templates(&self) -> Result<Vec<DayTemplate>, SchedulingError>;
    fn get_day_template(&self, id: &DayTemplateId) -> Result<DayTemplate, SchedulingError>;
    fn upsert_day_template(&self, template: &DayTemplate) -> Result<(), SchedulingError>;
    fn delete_day_template(&self, id: &DayTemplateId) -> Result<(), SchedulingError>;

    // ── Cal.com-style: event types ────────────────────────────────
    fn list_event_types(&self) -> Result<Vec<EventType>, SchedulingError>;
    fn get_event_type(&self, id: &EventTypeId) -> Result<EventType, SchedulingError>;
    fn upsert_event_type(&self, event_type: &EventType) -> Result<(), SchedulingError>;
    fn delete_event_type(&self, id: &EventTypeId) -> Result<(), SchedulingError>;

    // ── Availability schedules ────────────────────────────────────
    fn list_schedules(&self) -> Result<Vec<AvailabilitySchedule>, SchedulingError>;
    fn get_schedule(&self, id: &ScheduleId) -> Result<AvailabilitySchedule, SchedulingError>;
    fn upsert_schedule(&self, schedule: &AvailabilitySchedule) -> Result<(), SchedulingError>;
    fn delete_schedule(&self, id: &ScheduleId) -> Result<(), SchedulingError>;

    // ── Bookings ──────────────────────────────────────────────────
    /// Open slots for the event type inside `query.from_utc ..
    /// query.to_utc`. The backend intersects the event type's
    /// schedule rules with existing bookings.
    fn list_open_slots(&self, query: &SlotQuery) -> Result<Vec<TimeSlot>, SchedulingError>;
    fn list_bookings(&self) -> Result<Vec<Booking>, SchedulingError>;
    fn get_booking(&self, id: &BookingId) -> Result<Booking, SchedulingError>;
    /// Commit a new booking. Returns the persisted form (with id +
    /// path + status). Fails with `SlotUnavailable` if the slot
    /// was already taken between query + commit.
    fn create_booking(&self, booking: &NewBooking) -> Result<Booking, SchedulingError>;
    fn update_booking_status(
        &self,
        id: &BookingId,
        status: BookingStatus,
    ) -> Result<(), SchedulingError>;
}
