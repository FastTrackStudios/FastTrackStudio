//! Bookings — instances of someone reserving a slot for an event
//! type.

use facet::Facet;

use crate::event_type::EventTypeId;

/// Stable id for a booking (uuid v4 string).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct BookingId(pub String);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum BookingStatus {
    /// Awaiting host confirmation. Default for new bookings on
    /// event types that have `requires_confirmation`.
    Pending,
    /// Host accepted or the event type auto-confirms.
    Confirmed,
    /// Cancelled by either side.
    Cancelled,
    /// Marked as no-show after the slot end.
    NoShow,
    /// The event happened.
    Completed,
}

/// A booking the public page is creating. The host's vault stores
/// it as a markdown file in `scheduling/bookings/`.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct NewBooking {
    pub event_type_id: EventTypeId,
    /// ISO-8601 UTC inclusive start.
    pub start_utc: String,
    /// ISO-8601 UTC exclusive end. The host validates that
    /// `end - start` matches the event type's duration; the proto
    /// lets you over-specify so future variable-duration event
    /// types still fit.
    pub end_utc: String,
    pub attendee_name: String,
    pub attendee_email: String,
    /// Free-form note the bookee left on the booking form.
    pub note: Option<String>,
}

/// A persisted booking.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct Booking {
    pub id: BookingId,
    /// Vault path of the markdown file (set by the scanner).
    pub path: String,
    pub event_type_id: EventTypeId,
    pub start_utc: String,
    pub end_utc: String,
    pub attendee_name: String,
    pub attendee_email: String,
    pub note: Option<String>,
    pub status: BookingStatus,
    /// ISO-8601 UTC creation timestamp.
    pub created_utc: String,
}
