//! SeaORM persistence for the `conference` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::ConferenceMigrator;
