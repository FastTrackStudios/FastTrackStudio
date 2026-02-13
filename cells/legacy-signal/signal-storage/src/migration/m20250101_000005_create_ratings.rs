use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Ratings::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Ratings::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Ratings::UserId).uuid().not_null())
                    .col(ColumnDef::new(Ratings::PresetId).uuid().not_null())
                    .col(
                        ColumnDef::new(Ratings::Score)
                            .small_integer()
                            .not_null()
                            .default(0),
                    )
                    .col(ColumnDef::new(Ratings::Review).text())
                    .col(
                        ColumnDef::new(Ratings::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Ratings::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .from(Ratings::Table, Ratings::UserId)
                            .to(Users::Table, Users::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .from(Ratings::Table, Ratings::PresetId)
                            .to(Presets::Table, Presets::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .to_owned(),
            )
            .await?;

        // One rating per user per preset
        manager
            .create_index(
                Index::create()
                    .name("idx_ratings_user_preset")
                    .table(Ratings::Table)
                    .col(Ratings::UserId)
                    .col(Ratings::PresetId)
                    .unique()
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Ratings::Table).to_owned())
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
enum Ratings {
    Table,
    Id,
    UserId,
    PresetId,
    Score,
    Review,
    CreatedAt,
    UpdatedAt,
}
