//! SeaORM persistence for the `email` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::EmailMigrator;
