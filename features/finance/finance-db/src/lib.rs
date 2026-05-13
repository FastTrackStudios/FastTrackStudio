//! SeaORM persistence for the `finance` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::FinanceMigrator;
