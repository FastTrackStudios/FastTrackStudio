//! Migrator for the `knowledge` feature. Runs crdt-seaorm's generic
//! migrations (the `crdt_doc` + `crdt_update` tables) only. Phase A
//! has no projection tables; backlinks/tags/etc. are computed from
//! Loro in-memory.

use sea_orm_migration::prelude::*;

pub struct KnowledgeMigrator;

#[async_trait::async_trait]
impl MigratorTrait for KnowledgeMigrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        // No projection migrations in Phase A — see crate docs.
        crdt_seaorm::Migrator::migrations()
    }
}
