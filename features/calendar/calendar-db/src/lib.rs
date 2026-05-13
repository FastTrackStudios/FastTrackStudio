//! SeaORM persistence for the `calendar` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::CalendarMigrator;
