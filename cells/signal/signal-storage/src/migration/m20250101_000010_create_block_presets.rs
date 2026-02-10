//! Migration: Create block_presets and block_snapshots tables.
//!
//! Block presets store individual DSP unit configurations (EQ, Drive, Amp, etc.).
//! Block snapshots capture parameter variations within a block preset.

use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // -- block_presets: individual DSP unit library entries
        manager
            .create_table(
                Table::create()
                    .table(BlockPresets::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(BlockPresets::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(BlockPresets::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(BlockPresets::BlockType)
                            .string_len(64)
                            .not_null(),
                    )
                    .col(ColumnDef::new(BlockPresets::PluginId).json_binary().null())
                    .col(
                        ColumnDef::new(BlockPresets::PluginPresetName)
                            .string_len(255)
                            .null(),
                    )
                    .col(ColumnDef::new(BlockPresets::Description).text().null())
                    .col(
                        ColumnDef::new(BlockPresets::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(BlockPresets::IsDeleted)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(BlockPresets::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(BlockPresets::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_block_presets_block_type")
                    .table(BlockPresets::Table)
                    .col(BlockPresets::BlockType)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_block_presets_name")
                    .table(BlockPresets::Table)
                    .col(BlockPresets::Name)
                    .to_owned(),
            )
            .await?;

        // -- block_snapshots: parameter variations within a block preset
        manager
            .create_table(
                Table::create()
                    .table(BlockSnapshots::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(BlockSnapshots::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(BlockSnapshots::BlockPresetId)
                            .uuid()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(BlockSnapshots::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(BlockSnapshots::Parameters)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(ColumnDef::new(BlockSnapshots::DawChunkData).text().null())
                    .col(
                        ColumnDef::new(BlockSnapshots::IsDefault)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(BlockSnapshots::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(BlockSnapshots::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_block_snapshots_block_preset")
                            .from(BlockSnapshots::Table, BlockSnapshots::BlockPresetId)
                            .to(BlockPresets::Table, BlockPresets::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_block_snapshots_block_preset_id")
                    .table(BlockSnapshots::Table)
                    .col(BlockSnapshots::BlockPresetId)
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(BlockSnapshots::Table).to_owned())
            .await?;

        manager
            .drop_table(Table::drop().table(BlockPresets::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum BlockPresets {
    Table,
    Id,
    Name,
    BlockType,
    PluginId,
    PluginPresetName,
    Description,
    Tags,
    IsDeleted,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum BlockSnapshots {
    Table,
    Id,
    BlockPresetId,
    Name,
    Parameters,
    DawChunkData,
    IsDefault,
    CreatedAt,
    UpdatedAt,
}
