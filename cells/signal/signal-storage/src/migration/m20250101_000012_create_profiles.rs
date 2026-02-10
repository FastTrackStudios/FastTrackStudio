//! Migration: Create profiles and scene_templates tables.
//!
//! Profiles are named collections of scene templates tied to a rig.
//! Scene templates define which preset+snapshot to use for each scene position.

use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // -- profiles: named rig configurations (Worship, Blues, Rock, etc.)
        manager
            .create_table(
                Table::create()
                    .table(Profiles::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Profiles::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Profiles::Name).string_len(255).not_null())
                    .col(ColumnDef::new(Profiles::RigId).uuid().not_null())
                    .col(ColumnDef::new(Profiles::Description).text().null())
                    .col(
                        ColumnDef::new(Profiles::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(Profiles::Metadata)
                            .json_binary()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(Profiles::DefaultSceneTemplateId)
                            .uuid()
                            .null(),
                    )
                    .col(
                        ColumnDef::new(Profiles::IsDeleted)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(Profiles::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Profiles::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_profiles_name")
                    .table(Profiles::Table)
                    .col(Profiles::Name)
                    .to_owned(),
            )
            .await?;

        // -- scene_templates: ordered scene definitions within a profile
        manager
            .create_table(
                Table::create()
                    .table(SceneTemplates::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(SceneTemplates::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(SceneTemplates::ProfileId).uuid().not_null())
                    .col(
                        ColumnDef::new(SceneTemplates::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(ColumnDef::new(SceneTemplates::PresetId).uuid().not_null())
                    .col(ColumnDef::new(SceneTemplates::SnapshotId).uuid().null())
                    .col(
                        ColumnDef::new(SceneTemplates::ModuleOverrides)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(SceneTemplates::BlockOverrides)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(SceneTemplates::ParameterState)
                            .json_binary()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(SceneTemplates::SortOrder)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(
                        ColumnDef::new(SceneTemplates::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(SceneTemplates::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(SceneTemplates::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_scene_templates_profile")
                            .from(SceneTemplates::Table, SceneTemplates::ProfileId)
                            .to(Profiles::Table, Profiles::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_scene_templates_profile_id")
                    .table(SceneTemplates::Table)
                    .col(SceneTemplates::ProfileId)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_scene_templates_sort_order")
                    .table(SceneTemplates::Table)
                    .col(SceneTemplates::ProfileId)
                    .col(SceneTemplates::SortOrder)
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(SceneTemplates::Table).to_owned())
            .await?;

        manager
            .drop_table(Table::drop().table(Profiles::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum Profiles {
    Table,
    Id,
    Name,
    RigId,
    Description,
    Tags,
    Metadata,
    DefaultSceneTemplateId,
    IsDeleted,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum SceneTemplates {
    Table,
    Id,
    ProfileId,
    Name,
    PresetId,
    SnapshotId,
    ModuleOverrides,
    BlockOverrides,
    ParameterState,
    SortOrder,
    Tags,
    CreatedAt,
    UpdatedAt,
}
