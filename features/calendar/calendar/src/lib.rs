//! Facade for the `calendar` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use calendar_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `CalendarRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use calendar_crdt::{CalendarEntity, CalendarRepoLoro};
    pub use calendar_db::{CalendarMigrator, SeaOrmPersistence};
    pub use crdt::{CrdtDoc, Persistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
