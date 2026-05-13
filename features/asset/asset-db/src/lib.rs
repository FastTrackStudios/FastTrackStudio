//! SeaORM persistence for the `asset` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::AssetMigrator;
