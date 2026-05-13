//! Migrator for the `threads` feature. Runs crdt-seaorm's generic
//! migrations (the `crdt_doc` + `crdt_update` tables) by default;
//! add projection tables below when SQL-shaped queries are needed.
//!
//! ## Phase A schema note
//!
//! The Phase A extensions (Comment.kind / action_* / anchor_json / deleted /
//! Attachment.kind / blob_url / waveform_json / transcript / ...) live in
//! the Loro doc and are encoded by `threads-crdt`. No projection tables
//! exist yet, so no additive column migration is required — the CRDT
//! tolerant-decode path covers pre-extension snapshots loaded after the
//! upgrade. When the threads feature grows SQL-shaped queries (e.g. a
//! cross-workspace `/threads` inbox query joined against tasks), append
//! projection migrations here and keep them additive.

use sea_orm_migration::prelude::*;

pub struct ThreadsMigrator;

#[async_trait::async_trait]
impl MigratorTrait for ThreadsMigrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        crdt_seaorm::Migrator::migrations()
    }
}
