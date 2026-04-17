//! CalDAV integration — VTODO conversion and CalDAV sync.
//!
//! Provides bidirectional conversion between vault-core `Task` and
//! iCalendar `VTODO` components, plus a CalDAV client for syncing
//! with Nextcloud and other CalDAV servers.

mod vtodo;
mod sync;

pub use vtodo::{task_to_vtodo, vtodo_to_task, task_to_ics, ics_to_task};
pub use sync::{CalDavClient, CalDavConfig};
