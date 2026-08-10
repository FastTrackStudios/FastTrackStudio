//! SeaORM migrator for the runner registry.
//!
//! One table. `id` is the primary key so a re-registering runner
//! upserts rather than duplicating; `kind` is a column because
//! `backends_by_kind` filters on it; everything else rides in
//! `json`. See the crate docs for why.

use sea_orm_migration::prelude::*;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![Box::new(m20260810_000001_create_agent_backends::Migration)]
    }
}

mod m20260810_000001_create_agent_backends {
    use sea_orm_migration::prelude::*;

    #[derive(DeriveMigrationName)]
    pub struct Migration;

    #[derive(DeriveIden)]
    enum AgentBackends {
        Table,
        Id,
        Kind,
        Json,
    }

    #[async_trait::async_trait]
    impl MigrationTrait for Migration {
        async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
            manager
                .create_table(
                    Table::create()
                        .table(AgentBackends::Table)
                        .if_not_exists()
                        .col(
                            ColumnDef::new(AgentBackends::Id)
                                .string()
                                .not_null()
                                .primary_key(),
                        )
                        .col(ColumnDef::new(AgentBackends::Kind).string().not_null())
                        .col(ColumnDef::new(AgentBackends::Json).text().not_null())
                        .to_owned(),
                )
                .await?;

            manager
                .create_index(
                    Index::create()
                        .if_not_exists()
                        .name("idx_agent_backends_kind")
                        .table(AgentBackends::Table)
                        .col(AgentBackends::Kind)
                        .to_owned(),
                )
                .await
        }

        async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
            manager
                .drop_table(
                    Table::drop()
                        .table(AgentBackends::Table)
                        .if_exists()
                        .to_owned(),
                )
                .await
        }
    }
}
