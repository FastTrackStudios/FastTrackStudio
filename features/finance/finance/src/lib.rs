//! Facade for the `finance` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use finance_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `FinanceRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use finance_crdt::{FinanceEntity, FinanceRepoLoro};
    pub use finance_db::{FinanceMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
