//! SeaORM persistence for the `person` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::PersonMigrator;
