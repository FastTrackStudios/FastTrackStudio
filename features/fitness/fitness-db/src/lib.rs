//! SeaORM persistence for the `fitness` feature.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::FitnessMigrator;
