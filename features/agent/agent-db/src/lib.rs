//! SeaORM persistence for the `agent` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::AgentMigrator;
