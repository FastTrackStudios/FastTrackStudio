use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
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
                    .col(
                        ColumnDef::new(Snapshots::Name)
                            .string_len(255)
                            .not_null(),
                    )
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
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_snapshots_preset_id")
                    .table(Snapshots::Table)
                    .col(Snapshots::PresetId)
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Snapshots::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum Presets {
    Table,
    Id,
}

#[derive(DeriveIden)]
enum Snapshots {
    Table,
    Id,
    PresetId,
    Name,
    Data,
    CreatedAt,
    UpdatedAt,
}
