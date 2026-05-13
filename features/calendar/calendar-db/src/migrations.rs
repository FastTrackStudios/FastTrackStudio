//! Migrator for the `calendar` feature. Runs crdt-seaorm's generic
//! migrations (the `crdt_doc` + `crdt_update` tables) by default;
//! add projection tables below when SQL-shaped queries are needed.

use sea_orm_migration::prelude::*;

pub struct CalendarMigrator;

#[async_trait::async_trait]
impl MigratorTrait for CalendarMigrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        // Start with the generic CRDT-persistence migrations; append
        // feature-specific projection migrations after them when the
        // feature needs SQL-shaped queries.
        crdt_seaorm::Migrator::migrations()
    }
}
