use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(PropertyDefinitions::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(PropertyDefinitions::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(PropertyDefinitions::Name)
                            .text()
                            .not_null()
                            .unique_key(),
                    )
                    .col(
                        ColumnDef::new(PropertyDefinitions::Kind)
                            .text()
                            .not_null()
                            .default("text"),
                    )
                    .col(ColumnDef::new(PropertyDefinitions::Description).text())
                    .col(
                        ColumnDef::new(PropertyDefinitions::Options)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(PropertyDefinitions::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(PropertyDefinitions::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_property_definitions_name")
                    .table(PropertyDefinitions::Table)
                    .col(PropertyDefinitions::Name)
                    .to_owned(),
            )
            .await?;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(PropertyDefinitions::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum PropertyDefinitions {
    Table,
    Id,
    Name,
    Kind,
    Description,
    Options,
    CreatedAt,
    UpdatedAt,
}
