use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(PresetVersions::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(PresetVersions::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(PresetVersions::PresetId).uuid().not_null())
                    .col(
                        ColumnDef::new(PresetVersions::Version)
                            .big_integer()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(PresetVersions::Data)
                            .json_binary()
                            .not_null(),
                    )
                    .col(ColumnDef::new(PresetVersions::ChangeSummary).text())
                    .col(ColumnDef::new(PresetVersions::CreatedBy).uuid())
                    .col(
                        ColumnDef::new(PresetVersions::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .from(PresetVersions::Table, PresetVersions::PresetId)
                            .to(Presets::Table, Presets::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .from(PresetVersions::Table, PresetVersions::CreatedBy)
                            .to(Users::Table, Users::Id)
                            .on_delete(ForeignKeyAction::SetNull),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_preset_versions_preset_id")
                    .table(PresetVersions::Table)
                    .col(PresetVersions::PresetId)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_preset_versions_preset_version")
                    .table(PresetVersions::Table)
                    .col(PresetVersions::PresetId)
                    .col(PresetVersions::Version)
                    .unique()
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(PresetVersions::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum Users {
    Table,
    Id,
}

#[derive(DeriveIden)]
enum Presets {
    Table,
    Id,
}

#[derive(DeriveIden)]
enum PresetVersions {
    Table,
    Id,
    PresetId,
    Version,
    Data,
    ChangeSummary,
    CreatedBy,
    CreatedAt,
}
