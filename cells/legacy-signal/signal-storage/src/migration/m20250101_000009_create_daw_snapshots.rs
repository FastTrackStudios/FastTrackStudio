//! Migration: Create daw_parameter_snapshots and daw_state_chunk_snapshots tables.
//!
//! These tables store DAW FX chain snapshots keyed by track GUID,
//! independent of the preset system's snapshot table.

use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // -- daw_parameter_snapshots: Snapshooter-style parameter captures
        manager
            .create_table(
                Table::create()
                    .table(DawParameterSnapshots::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(DawParameterSnapshots::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(DawParameterSnapshots::TrackGuid)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(DawParameterSnapshots::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(DawParameterSnapshots::Data)
                            .json_binary()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(DawParameterSnapshots::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_daw_param_snapshots_track_guid")
                    .table(DawParameterSnapshots::Table)
                    .col(DawParameterSnapshots::TrackGuid)
                    .to_owned(),
            )
            .await?;

        // -- daw_state_chunk_snapshots: Track Snapshot-style full state captures
        manager
            .create_table(
                Table::create()
                    .table(DawStateChunkSnapshots::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(DawStateChunkSnapshots::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(DawStateChunkSnapshots::TrackGuid)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(DawStateChunkSnapshots::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(DawStateChunkSnapshots::Data)
                            .json_binary()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(DawStateChunkSnapshots::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_daw_state_chunk_snapshots_track_guid")
                    .table(DawStateChunkSnapshots::Table)
                    .col(DawStateChunkSnapshots::TrackGuid)
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(
                Table::drop()
                    .table(DawStateChunkSnapshots::Table)
                    .to_owned(),
            )
            .await?;

        manager
            .drop_table(Table::drop().table(DawParameterSnapshots::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum DawParameterSnapshots {
    Table,
    Id,
    TrackGuid,
    Name,
    Data,
    CreatedAt,
}

#[derive(DeriveIden)]
enum DawStateChunkSnapshots {
    Table,
    Id,
    TrackGuid,
    Name,
    Data,
    CreatedAt,
}
