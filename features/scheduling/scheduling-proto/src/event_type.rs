//! Cal.com-style bookable event type.
//!
//! Mirrors the [cal.com `EventType` model][cal] surface that
//! matters for booking: title / slug / duration / location /
//! optional schedule reference. Things we *don't* carry on the
//! wire (recurring meetings, payment, custom fields) live behind
//! `metadata` so the proto stays stable while the UI evolves.
//!
//! [cal]: https://github.com/calcom/cal.com/blob/main/packages/prisma/schema.prisma

use facet::Facet;

use crate::schedule::ScheduleId;

/// Stable id for an event type (uuid v4 string).
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct EventTypeId(pub String);

/// Where the meeting happens. Inline so we can drop the dependency
/// on a Location service for the v1 cut.
#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(u8)]
pub enum EventTypeLocation {
    /// Phone call — number arrives in `metadata.phone` at booking time.
    Phone,
    /// In-person — `address` is shown to the bookee.
    InPerson { address: String },
    /// Generic conferencing URL the host pastes in. Future work
    /// adds first-party Zoom / Meet / Whereby integrations.
    Link { url: String },
    /// "We'll decide later" — the host's choice during prep.
    Tbd,
}

/// One bookable thing: a 30-minute consultation, a 60-minute
/// strategy session, etc.
#[derive(Debug, Clone, PartialEq, Facet)]
pub struct EventType {
    pub id: EventTypeId,
    /// Vault-relative path of the markdown file backing this event
    /// type. Populated by the scanner; not part of the wire write.
    pub path: String,
    pub title: String,
    /// URL-safe slug. The public booking page lives at
    /// `/book/<owner>/<slug>`.
    pub slug: String,
    pub description: Option<String>,
    /// Duration in minutes.
    pub duration_min: u16,
    /// How many minutes between consecutive bookings. Default 0;
    /// useful when the host needs a buffer.
    pub buffer_min: u16,
    pub location: EventTypeLocation,
    /// Which availability schedule controls open slots. `None`
    /// means "use the owner's default schedule".
    pub schedule_id: Option<ScheduleId>,
    /// True if the event type is published to the public booking
    /// page. Defaults true for new event types.
    pub published: bool,
}
