//! Top-level pages mounted by [`crate::routes::Route`].
//!
//! Each page is responsible for its own data wiring. The shell
//! (sidebar + headers + bottom bar) is provided by the route
//! layout; pages only render the content area.

pub mod agents;
pub mod bases;
pub mod bookings;
pub mod connections;
pub mod contacts;
pub mod cook_mode;
pub mod email;
pub mod finances;
pub mod fitness;
pub mod gantt;
pub mod goals;
pub mod home;
pub mod inbox;
pub mod inventory;
pub mod invoices;
pub mod members;
pub mod ledger;
pub mod locations;
pub mod mealplan;
pub mod milestones;
pub mod note_header;
pub mod project_detail;
pub mod projects;
pub mod recall;
pub mod recipe_edit;
pub mod repos;
pub mod schedule;
pub mod scripture;
pub mod settings;
pub mod task_detail;
pub mod tasks;
pub mod timer;
pub mod vault;
pub mod watch;
pub mod wiki;
pub mod wiki_page;
pub mod wiki_source;
