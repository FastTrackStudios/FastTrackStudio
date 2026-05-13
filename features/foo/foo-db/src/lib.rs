//! SeaORM persistence for the `foo` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::FooMigrator;
