//! SeaORM persistence for the `project` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::ProjectMigrator;
