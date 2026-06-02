//! Top-level pages mounted by [`crate::routes::Route`].
//!
//! Each page is responsible for its own data wiring. The shell
//! (sidebar + headers + bottom bar) is provided by the route
//! layout; pages only render the content area.

pub mod bookings;
pub mod finances;
pub mod gantt;
pub mod goals;
pub mod home;
pub mod inbox;
pub mod invoices;
pub mod locations;
pub mod milestones;
pub mod project_detail;
pub mod projects;
pub mod schedule;
pub mod settings;
pub mod tasks;
pub mod timer;
pub mod vault;
pub mod wiki;
