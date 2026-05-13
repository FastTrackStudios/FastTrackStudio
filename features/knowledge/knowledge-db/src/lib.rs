//! SeaORM persistence for the `knowledge` feature.
//!
//! Phase A ships generic CRDT migrations only — the on-disk schema
//! is just `crdt_doc` + `crdt_update` from `crdt-seaorm`. No
//! projection tables for backlinks / tags / etc.; those will come
//! when the Loro-in-memory query path is no longer fast enough.

pub use crdt_seaorm::SeaOrmPersistence;

pub mod migrations;

pub use migrations::KnowledgeMigrator;
