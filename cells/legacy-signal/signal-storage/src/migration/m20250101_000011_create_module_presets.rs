//! Migration: Create module_presets and module_snapshots tables.
//!
//! Module presets compose blocks into purpose-driven processing groups
//! (e.g., Drive Module = Boost + Drive1 + Drive2 + Drive3).
//! Module snapshots capture parameter variations across all blocks in a module.

use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // -- module_presets: composable block groups
        manager
            .create_table(
                Table::create()
                    .table(ModulePresets::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ModulePresets::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(ModulePresets::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ModulePresets::ModuleType)
                            .string_len(64)
                            .not_null(),
                    )
                    .col(ColumnDef::new(ModulePresets::Description).text().null())
                    .col(
                        ColumnDef::new(ModulePresets::Blocks)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(ModulePresets::Macros)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(ModulePresets::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(ModulePresets::IsDeleted)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(ModulePresets::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ModulePresets::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_module_presets_module_type")
                    .table(ModulePresets::Table)
                    .col(ModulePresets::ModuleType)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_module_presets_name")
                    .table(ModulePresets::Table)
                    .col(ModulePresets::Name)
                    .to_owned(),
            )
            .await?;

        // -- module_snapshots: parameter variations across a module's blocks
        manager
            .create_table(
                Table::create()
                    .table(ModuleSnapshots::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ModuleSnapshots::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(ModuleSnapshots::ModulePresetId)
                            .uuid()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ModuleSnapshots::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ModuleSnapshots::BlockOverrides)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(ModuleSnapshots::IsDefault)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(ModuleSnapshots::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(ModuleSnapshots::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ModuleSnapshots::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_module_snapshots_module_preset")
                            .from(ModuleSnapshots::Table, ModuleSnapshots::ModulePresetId)
                            .to(ModulePresets::Table, ModulePresets::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_module_snapshots_module_preset_id")
                    .table(ModuleSnapshots::Table)
                    .col(ModuleSnapshots::ModulePresetId)
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(ModuleSnapshots::Table).to_owned())
            .await?;

        manager
            .drop_table(Table::drop().table(ModulePresets::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum ModulePresets {
    Table,
    Id,
    Name,
    ModuleType,
    Description,
    Blocks,
    Macros,
    Tags,
    IsDeleted,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum ModuleSnapshots {
    Table,
    Id,
    ModulePresetId,
    Name,
    BlockOverrides,
    IsDefault,
    Tags,
    CreatedAt,
    UpdatedAt,
}
