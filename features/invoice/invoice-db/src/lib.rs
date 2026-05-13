//! SeaORM persistence for the `invoice` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::InvoiceMigrator;
