//! SeaORM persistence for the `chat` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::ChatMigrator;
