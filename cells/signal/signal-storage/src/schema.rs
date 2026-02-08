//! Sea-query schema definitions for signal storage tables.
//!
//! These definitions are backend-agnostic — the same schema works with
//! both SQLite (local) and PostgreSQL (cloud). Sea-query generates the
//! appropriate SQL dialect at runtime.

use sea_query::{ColumnDef, ForeignKey, ForeignKeyAction, Iden, Index, Table};

// ─────────────────────────────────────────────────────────────────────────────
// Table identifiers
// ─────────────────────────────────────────────────────────────────────────────

/// Presets table — stores complete rig presets as JSON documents.
#[derive(Iden)]
pub enum Presets {
    Table,
    Id,
    Name,
    Description,
    AuthorId,
    Category,
    Tags,
    Rating,
    Data,
    IsPublic,
    IsDeleted,
    Version,
    CreatedAt,
    UpdatedAt,
}

/// Snapshots table — stores named snapshots within presets.
#[derive(Iden)]
pub enum Snapshots {
    Table,
    Id,
    PresetId,
    Name,
    Data,
    CreatedAt,
    UpdatedAt,
}

/// Rig configurations table.
#[derive(Iden)]
pub enum RigConfigs {
    Table,
    Id,
    Name,
    Description,
    Data,
    CreatedAt,
    UpdatedAt,
}

/// Module chunks — reusable module state snapshots.
#[derive(Iden)]
pub enum ModuleChunks {
    Table,
    Id,
    PresetId,
    ModuleType,
    Name,
    Data,
    CreatedAt,
}

/// Sync metadata — tracks last sync time per entity for delta sync.
#[derive(Iden)]
pub enum SyncMetadata {
    Table,
    Id,
    EntityType,
    EntityId,
    LastSyncAt,
    SyncStatus,
    LocalVersion,
    RemoteVersion,
}

// ─────────────────────────────────────────────────────────────────────────────
// Schema creation
// ─────────────────────────────────────────────────────────────────────────────

/// Build the CREATE TABLE statement for the presets table.
pub fn create_presets_table() -> sea_query::TableCreateStatement {
    Table::create()
        .table(Presets::Table)
        .if_not_exists()
        .col(ColumnDef::new(Presets::Id).uuid().not_null().primary_key())
        .col(ColumnDef::new(Presets::Name).string_len(255).not_null())
        .col(ColumnDef::new(Presets::Description).text())
        .col(ColumnDef::new(Presets::AuthorId).uuid())
        .col(ColumnDef::new(Presets::Category).json_binary().not_null())
        .col(ColumnDef::new(Presets::Tags).json_binary().not_null())
        .col(
            ColumnDef::new(Presets::Rating)
                .small_integer()
                .not_null()
                .default(0),
        )
        .col(ColumnDef::new(Presets::Data).json_binary().not_null())
        .col(
            ColumnDef::new(Presets::IsPublic)
                .boolean()
                .not_null()
                .default(false),
        )
        .col(
            ColumnDef::new(Presets::IsDeleted)
                .boolean()
                .not_null()
                .default(false),
        )
        .col(
            ColumnDef::new(Presets::Version)
                .big_integer()
                .not_null()
                .default(1),
        )
        .col(
            ColumnDef::new(Presets::CreatedAt)
                .timestamp_with_time_zone()
                .not_null(),
        )
        .col(
            ColumnDef::new(Presets::UpdatedAt)
                .timestamp_with_time_zone()
                .not_null(),
        )
        .to_owned()
}

/// Build the CREATE TABLE statement for the snapshots table.
pub fn create_snapshots_table() -> sea_query::TableCreateStatement {
    Table::create()
        .table(Snapshots::Table)
        .if_not_exists()
        .col(
            ColumnDef::new(Snapshots::Id)
                .uuid()
                .not_null()
                .primary_key(),
        )
        .col(ColumnDef::new(Snapshots::PresetId).uuid().not_null())
        .col(ColumnDef::new(Snapshots::Name).string_len(255).not_null())
        .col(ColumnDef::new(Snapshots::Data).json_binary().not_null())
        .col(
            ColumnDef::new(Snapshots::CreatedAt)
                .timestamp_with_time_zone()
                .not_null(),
        )
        .col(
            ColumnDef::new(Snapshots::UpdatedAt)
                .timestamp_with_time_zone()
                .not_null(),
        )
        .foreign_key(
            ForeignKey::create()
                .from(Snapshots::Table, Snapshots::PresetId)
                .to(Presets::Table, Presets::Id)
                .on_delete(ForeignKeyAction::Cascade),
        )
        .to_owned()
}

/// Build the CREATE TABLE statement for the rig_configs table.
pub fn create_rig_configs_table() -> sea_query::TableCreateStatement {
    Table::create()
        .table(RigConfigs::Table)
        .if_not_exists()
        .col(
            ColumnDef::new(RigConfigs::Id)
                .uuid()
                .not_null()
                .primary_key(),
        )
        .col(ColumnDef::new(RigConfigs::Name).string_len(255).not_null())
        .col(ColumnDef::new(RigConfigs::Description).text())
        .col(ColumnDef::new(RigConfigs::Data).json_binary().not_null())
        .col(
            ColumnDef::new(RigConfigs::CreatedAt)
                .timestamp_with_time_zone()
                .not_null(),
        )
        .col(
            ColumnDef::new(RigConfigs::UpdatedAt)
                .timestamp_with_time_zone()
                .not_null(),
        )
        .to_owned()
}

/// Build the CREATE TABLE statement for the module_chunks table.
pub fn create_module_chunks_table() -> sea_query::TableCreateStatement {
    Table::create()
        .table(ModuleChunks::Table)
        .if_not_exists()
        .col(
            ColumnDef::new(ModuleChunks::Id)
                .uuid()
                .not_null()
                .primary_key(),
        )
        .col(ColumnDef::new(ModuleChunks::PresetId).uuid())
        .col(
            ColumnDef::new(ModuleChunks::ModuleType)
                .string_len(100)
                .not_null(),
        )
        .col(
            ColumnDef::new(ModuleChunks::Name)
                .string_len(255)
                .not_null(),
        )
        .col(ColumnDef::new(ModuleChunks::Data).json_binary().not_null())
        .col(
            ColumnDef::new(ModuleChunks::CreatedAt)
                .timestamp_with_time_zone()
                .not_null(),
        )
        .to_owned()
}

/// Build the CREATE TABLE statement for the sync_metadata table.
pub fn create_sync_metadata_table() -> sea_query::TableCreateStatement {
    Table::create()
        .table(SyncMetadata::Table)
        .if_not_exists()
        .col(
            ColumnDef::new(SyncMetadata::Id)
                .uuid()
                .not_null()
                .primary_key(),
        )
        .col(
            ColumnDef::new(SyncMetadata::EntityType)
                .string_len(50)
                .not_null(),
        )
        .col(ColumnDef::new(SyncMetadata::EntityId).uuid().not_null())
        .col(
            ColumnDef::new(SyncMetadata::LastSyncAt)
                .timestamp_with_time_zone()
                .not_null(),
        )
        .col(
            ColumnDef::new(SyncMetadata::SyncStatus)
                .string_len(20)
                .not_null()
                .default("pending"),
        )
        .col(
            ColumnDef::new(SyncMetadata::LocalVersion)
                .big_integer()
                .not_null()
                .default(0),
        )
        .col(
            ColumnDef::new(SyncMetadata::RemoteVersion)
                .big_integer()
                .not_null()
                .default(0),
        )
        .to_owned()
}

// ─────────────────────────────────────────────────────────────────────────────
// Indexes
// ─────────────────────────────────────────────────────────────────────────────

/// Build indexes for common preset queries.
pub fn create_presets_indexes() -> Vec<sea_query::IndexCreateStatement> {
    vec![
        Index::create()
            .name("idx_presets_name")
            .table(Presets::Table)
            .col(Presets::Name)
            .to_owned(),
        Index::create()
            .name("idx_presets_author")
            .table(Presets::Table)
            .col(Presets::AuthorId)
            .to_owned(),
        Index::create()
            .name("idx_presets_is_public")
            .table(Presets::Table)
            .col(Presets::IsPublic)
            .to_owned(),
        Index::create()
            .name("idx_presets_created_at")
            .table(Presets::Table)
            .col(Presets::CreatedAt)
            .to_owned(),
        Index::create()
            .name("idx_presets_updated_at")
            .table(Presets::Table)
            .col(Presets::UpdatedAt)
            .to_owned(),
    ]
}

/// Build indexes for snapshot queries.
pub fn create_snapshots_indexes() -> Vec<sea_query::IndexCreateStatement> {
    vec![Index::create()
        .name("idx_snapshots_preset_id")
        .table(Snapshots::Table)
        .col(Snapshots::PresetId)
        .to_owned()]
}

/// Build index for sync metadata lookups.
pub fn create_sync_metadata_indexes() -> Vec<sea_query::IndexCreateStatement> {
    vec![Index::create()
        .name("idx_sync_entity")
        .table(SyncMetadata::Table)
        .col(SyncMetadata::EntityType)
        .col(SyncMetadata::EntityId)
        .unique()
        .to_owned()]
}

#[cfg(test)]
mod tests {
    use sea_query::PostgresQueryBuilder;

    use super::*;

    #[test]
    fn presets_table_generates_valid_sql() {
        let sql = create_presets_table().to_string(PostgresQueryBuilder);
        assert!(sql.contains("CREATE TABLE IF NOT EXISTS"));
        assert!(sql.contains("presets"));
        assert!(sql.contains("\"id\""));
        assert!(sql.contains("\"name\""));
        assert!(sql.contains("\"version\""));
    }

    #[test]
    fn snapshots_table_has_foreign_key() {
        let sql = create_snapshots_table().to_string(PostgresQueryBuilder);
        assert!(sql.contains("REFERENCES"));
        assert!(sql.contains("presets"));
        assert!(sql.contains("ON DELETE CASCADE"));
    }

    #[test]
    fn all_tables_generate_without_panic() {
        let _ = create_presets_table().to_string(PostgresQueryBuilder);
        let _ = create_snapshots_table().to_string(PostgresQueryBuilder);
        let _ = create_rig_configs_table().to_string(PostgresQueryBuilder);
        let _ = create_module_chunks_table().to_string(PostgresQueryBuilder);
        let _ = create_sync_metadata_table().to_string(PostgresQueryBuilder);
    }

    #[test]
    fn indexes_generate_without_panic() {
        for idx in create_presets_indexes() {
            let _ = idx.to_string(PostgresQueryBuilder);
        }
        for idx in create_snapshots_indexes() {
            let _ = idx.to_string(PostgresQueryBuilder);
        }
        for idx in create_sync_metadata_indexes() {
            let _ = idx.to_string(PostgresQueryBuilder);
        }
    }
}
