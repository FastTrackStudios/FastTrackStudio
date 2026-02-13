use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(Presets::Table)
                    .add_column(
                        ColumnDef::new(Presets::IsFavorite)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_presets_is_favorite")
                    .table(Presets::Table)
                    .col(Presets::IsFavorite)
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_index(
                Index::drop()
                    .name("idx_presets_is_favorite")
                    .table(Presets::Table)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Presets::Table)
                    .drop_column(Presets::IsFavorite)
                    .to_owned(),
            )
            .await
    }
}

#[derive(DeriveIden)]
enum Presets {
    Table,
    IsFavorite,
}
