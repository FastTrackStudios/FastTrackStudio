//! SeaORM persistence for the `inventory` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::InventoryMigrator;
