//! Facade for the `invoice` feature. Wire types are always
//! re-exported; backends + transport adapters are feature-gated.

pub use invoice_proto::*;

/// CRDT source of truth + SeaORM persistence. Construct one
/// `CrdtDoc` per collaboration boundary and hand a `InvoiceRepoLoro`
/// to the vox dispatcher.
#[cfg(feature = "server")]
pub mod server {
    pub use crdt::{CrdtDoc, Persistence};
    pub use invoice_crdt::{InvoiceEntity, InvoiceRepoLoro};
    pub use invoice_db::{InvoiceMigrator, SeaOrmPersistence};
}

#[cfg(feature = "server-axum")]
pub use architect::axum_ws;
