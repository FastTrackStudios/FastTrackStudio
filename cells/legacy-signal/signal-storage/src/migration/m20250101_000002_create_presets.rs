use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Presets::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Presets::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Presets::Name).string_len(255).not_null())
                    .col(ColumnDef::new(Presets::Description).text())
                    .col(ColumnDef::new(Presets::AuthorId).uuid())
                    .col(ColumnDef::new(Presets::Category).json_binary().not_null())
                    .col(ColumnDef::new(Presets::Tags).json_binary().not_null())
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
                    .foreign_key(
                        ForeignKey::create()
                            .from(Presets::Table, Presets::AuthorId)
                            .to(Users::Table, Users::Id)
                            .on_delete(ForeignKeyAction::SetNull),
                    )
                    .to_owned(),
            )
            .await?;

        // Indexes for common query patterns
        manager
            .create_index(
                Index::create()
                    .name("idx_presets_name")
                    .table(Presets::Table)
                    .col(Presets::Name)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_presets_author_id")
                    .table(Presets::Table)
                    .col(Presets::AuthorId)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_presets_is_public")
                    .table(Presets::Table)
                    .col(Presets::IsPublic)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_presets_updated_at")
                    .table(Presets::Table)
                    .col(Presets::UpdatedAt)
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Presets::Table).to_owned())
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
    Name,
    Description,
    AuthorId,
    Category,
    Tags,
    Data,
    IsPublic,
    IsDeleted,
    Version,
    CreatedAt,
    UpdatedAt,
}
