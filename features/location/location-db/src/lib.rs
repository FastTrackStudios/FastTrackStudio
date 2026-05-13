//! SeaORM persistence for the `location` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::LocationMigrator;
