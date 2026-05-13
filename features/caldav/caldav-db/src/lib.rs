//! SeaORM persistence for the `caldav` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::CaldavMigrator;
