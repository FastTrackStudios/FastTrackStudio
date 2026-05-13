//! SeaORM persistence for the `bar` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::BarMigrator;
