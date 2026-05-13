//! SeaORM persistence for the `threads` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::ThreadsMigrator;
