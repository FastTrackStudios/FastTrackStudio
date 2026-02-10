//! Database migrations for signal storage.
//!
//! Uses sea-orm-migration for versioned, reversible schema changes.

use sea_orm_migration::prelude::*;

mod m20250101_000001_create_users;
mod m20250101_000002_create_presets;
mod m20250101_000003_create_snapshots;
mod m20250101_000004_create_module_chunks;
mod m20250101_000005_create_ratings;
mod m20250101_000006_create_sync_metadata;
mod m20250101_000007_create_preset_versions;
mod m20250101_000008_add_preset_favorite;
mod m20250101_000009_create_daw_snapshots;
mod m20250101_000010_create_block_presets;
mod m20250101_000011_create_module_presets;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20250101_000001_create_users::Migration),
            Box::new(m20250101_000002_create_presets::Migration),
            Box::new(m20250101_000003_create_snapshots::Migration),
            Box::new(m20250101_000004_create_module_chunks::Migration),
            Box::new(m20250101_000005_create_ratings::Migration),
            Box::new(m20250101_000006_create_sync_metadata::Migration),
            Box::new(m20250101_000007_create_preset_versions::Migration),
            Box::new(m20250101_000008_add_preset_favorite::Migration),
            Box::new(m20250101_000009_create_daw_snapshots::Migration),
            Box::new(m20250101_000010_create_block_presets::Migration),
            Box::new(m20250101_000011_create_module_presets::Migration),
        ]
    }
}
