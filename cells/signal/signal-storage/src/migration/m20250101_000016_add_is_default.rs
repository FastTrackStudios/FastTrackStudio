use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // Add is_default to snapshots (preset-level snapshots)
        manager
            .alter_table(
                Table::alter()
                    .table(Snapshots::Table)
                    .add_column(
                        ColumnDef::new(Snapshots::IsDefault)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .to_owned(),
            )
            .await?;

        // Add is_default to song_scenes
        manager
            .alter_table(
                Table::alter()
                    .table(SongScenes::Table)
                    .add_column(
                        ColumnDef::new(SongScenes::IsDefault)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(SongScenes::Table)
                    .drop_column(SongScenes::IsDefault)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Snapshots::Table)
                    .drop_column(Snapshots::IsDefault)
                    .to_owned(),
            )
            .await
    }
}

#[derive(DeriveIden)]
enum Snapshots {
    Table,
    IsDefault,
}

#[derive(DeriveIden)]
enum SongScenes {
    Table,
    IsDefault,
}
