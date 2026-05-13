//! SeaORM persistence for the `cookbook` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::CookbookMigrator;
