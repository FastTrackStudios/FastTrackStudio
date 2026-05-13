//! SeaORM persistence for the `timer` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::TimerMigrator;
