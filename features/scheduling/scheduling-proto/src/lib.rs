// architect's Entity derive emits cfg-gated blocks; allow
// at crate scope.
#![allow(unexpected_cfgs)]

//! Wire contract for the scheduling feature.
//!
//! Two distinct surfaces share this proto:
//!
//! 1. **Personal scheduling** — [`DayTemplate`] / [`TimeBlock`] /
//!    [`BlockCategory`] capture the user's daily rhythm. The
//!    personal day-template editor binds to [`service::DayTemplates`].
//!
//! 2. **Business / cal.com-style scheduling** — [`EventType`] is
//!    the bookable surface backed by an [`AvailabilitySchedule`].
//!    The public booking page binds to [`service::EventTypes`] +
//!    [`service::Slots`] + [`service::Bookings`].
//!
//! ## Capability sub-traits
//!
//! No umbrella trait. Each capability is its own
//! `#[architect::rpc]` so function signatures express exactly
//! what they need:
//!
//! ```ignore
//! fn render_booking_page<S: EventTypes + Slots + Bookings>(s: &S, /* … */) { /* … */ }
//! fn save_template       <S: DayTemplates>                 (s: &S, /* … */) { /* … */ }
//! ```
//!
//! Backends mix + match — a `CalDAV` peer might impl just
//! `EventTypes + Slots`; the local `VaultScheduler` impls all
//! five.
//!
//! Both surfaces live in the same vault as plain markdown files —
//! the `scheduling` crate (sibling) owns the parse / write side.

pub mod booking;
pub mod day_plan;
pub mod error;
pub mod event_type;
pub mod schedule;
pub mod service;
pub mod time_block;

pub use booking::{Booking, BookingId, BookingStatus, NewBooking};
pub use day_plan::{BlockAssignment, DayPlan, PlannedBlock};
pub use error::SchedulingError;
pub use event_type::{EventType, EventTypeId, EventTypeLocation};
pub use schedule::{
    AvailabilityRule, AvailabilitySchedule, ScheduleId, SlotQuery, TimeSlot, Weekday,
};
pub use service::{Bookings, DayPlans, DayTemplates, EventTypes, Schedules, Slots};
pub use time_block::{
    BlockCategory, DayTemplate, DayTemplateId, TimeBlock, TimeBlockId, TimeOfDay,
};

// architect-emitted vox bits. Each capability gets its own
// client / dispatcher / descriptor. Consumer mount sites stitch
// the ones they need into an `architect::Services` bundle.
#[cfg(feature = "vox")]
pub use service::{
    bookings::{
        BookingsClient, BookingsRpc, BookingsRpcDispatcher, Service as BookingsService,
        layer as bookings_layer, serve as bookings_serve,
    },
    day_plans::{
        DayPlansClient, DayPlansRpc, DayPlansRpcDispatcher, Service as DayPlansService,
        layer as day_plans_layer, serve as day_plans_serve,
    },
    day_templates::{
        DayTemplatesClient, DayTemplatesRpc, DayTemplatesRpcDispatcher,
        Service as DayTemplatesService, layer as day_templates_layer, serve as day_templates_serve,
    },
    event_types::{
        EventTypesClient, EventTypesRpc, EventTypesRpcDispatcher, Service as EventTypesService,
        layer as event_types_layer, serve as event_types_serve,
    },
    schedules::{
        SchedulesClient, SchedulesRpc, SchedulesRpcDispatcher, Service as SchedulesService,
        layer as schedules_layer, serve as schedules_serve,
    },
    slots::{
        Service as SlotsService, SlotsClient, SlotsRpc, SlotsRpcDispatcher, layer as slots_layer,
        serve as slots_serve,
    },
};
