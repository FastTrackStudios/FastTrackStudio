//! SeaORM migrator for the prefs schema.

use sea_orm_migration::prelude::*;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![Box::new(m20260702_000001_create_prefs::Migration)]
    }
}

mod m20260702_000001_create_prefs {
    use sea_orm_migration::prelude::*;

    #[derive(DeriveMigrationName)]
    pub struct Migration;

    #[async_trait::async_trait]
    impl MigrationTrait for Migration {
        async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
            manager
                .create_table(
                    Table::create()
                        .table(UserPrefs::Table)
                        .if_not_exists()
                        .col(
                            ColumnDef::new(UserPrefs::UserId)
                                .uuid()
                                .not_null()
                                .primary_key(),
                        )
                        .col(ColumnDef::new(UserPrefs::DefaultPage).string().not_null())
                        .col(ColumnDef::new(UserPrefs::TasksActive).boolean().not_null())
                        .col(
                            ColumnDef::new(UserPrefs::TasksRelevant)
                                .boolean()
                                .not_null(),
                        )
                        .col(ColumnDef::new(UserPrefs::Location).string().not_null())
                        .to_owned(),
                )
                .await
        }

        async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
            manager
                .drop_table(Table::drop().table(UserPrefs::Table).if_exists().to_owned())
                .await
        }
    }

    // ── Iden ──────────────────────────────────────────────────────────

    #[derive(Iden)]
    enum UserPrefs {
        Table,
        UserId,
        DefaultPage,
        TasksActive,
        TasksRelevant,
        Location,
    }
}
