//! Wire contract for the scheduling feature.
//!
//! Two distinct surfaces share this proto:
//!
//! 1. **Personal scheduling** — [`DayTemplate`] / [`TimeBlock`] /
//!    [`BlockCategory`] capture the user's daily rhythm (morning
//!    reset, three allocatable work blocks, maintenance hour, etc.)
//!    as a structured template that can be edited in the UI and
//!    round-tripped through markdown frontmatter.
//!
//! 2. **Business / cal.com-style scheduling** — [`EventType`] is the
//!    bookable surface (30-min consultation, strategy session, …)
//!    backed by an [`AvailabilitySchedule`]. Public callers turn
//!    [`SchedulingService::list_open_slots`] into a booking page;
//!    [`SchedulingService::create_booking`] commits an instance.
//!
//! Both surfaces live in the same vault as plain markdown files —
//! the `scheduling` crate (sibling) owns the parse / write side.
//! `scheduling-proto` itself is the wasm-clean wire layer: facet
//! types + the `#[architect::rpc]` trait that backends implement.

pub mod booking;
pub mod error;
pub mod event_type;
pub mod schedule;
pub mod service;
pub mod time_block;

pub use booking::{Booking, BookingId, BookingStatus, NewBooking};
pub use error::SchedulingError;
pub use event_type::{EventType, EventTypeId, EventTypeLocation};
pub use schedule::{
    AvailabilityRule, AvailabilitySchedule, ScheduleId, SlotQuery, TimeSlot, Weekday,
};
pub use service::SchedulingService;
pub use time_block::{
    BlockCategory, DayTemplate, DayTemplateId, TimeBlock, TimeBlockId, TimeOfDay,
};

pub use service::SchedulingServiceRpc;

// architect-emitted vox bits from the auto-generated mirror
// trait. Aliased to shorter names so consumer mounting code
// reads consistently with `vault::sync_descriptor` /
// `vault::SyncDispatcher`.
#[cfg(feature = "vox")]
pub use service::{
    SchedulingServiceClient, SchedulingServiceRpcDispatcher as Dispatcher, Service, layer,
    scheduling_service_rpc_service_descriptor as descriptor, serve,
};
