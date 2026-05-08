//! Create the `substitutions` table — swappable-food rules.

use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

fn is_duplicate_table(err: &DbErr) -> bool {
    let message = err.to_string().to_ascii_lowercase();
    message.contains("already exists") || message.contains("duplicate")
}

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        let result = manager
            .create_table(
                Table::create()
                    .table(Substitutions::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Substitutions::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Substitutions::FromFoodId).uuid().not_null())
                    .col(ColumnDef::new(Substitutions::ToFoodId).uuid().not_null())
                    .col(
                        ColumnDef::new(Substitutions::Ratio)
                            .double()
                            .not_null()
                            .default(1.0),
                    )
                    .col(ColumnDef::new(Substitutions::ConversionNote).text())
                    .col(
                        ColumnDef::new(Substitutions::AppliesWhen)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(Substitutions::Confidence)
                            .float()
                            .not_null()
                            .default(0.8),
                    )
                    .col(
                        ColumnDef::new(Substitutions::Bidirectional)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(ColumnDef::new(Substitutions::Organization).string_len(100))
                    .col(ColumnDef::new(Substitutions::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(Substitutions::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(Substitutions::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Substitutions::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await;
        match result {
            Ok(()) => {}
            Err(err) if is_duplicate_table(&err) => return Ok(()),
            Err(err) => return Err(err),
        }

        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_substitutions_from_food")
                    .table(Substitutions::Table)
                    .col(Substitutions::FromFoodId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_substitutions_to_food")
                    .table(Substitutions::Table)
                    .col(Substitutions::ToFoodId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_substitutions_org_from")
                    .table(Substitutions::Table)
                    .col(Substitutions::Organization)
                    .col(Substitutions::FromFoodId)
                    .to_owned(),
            )
            .await;
        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Substitutions::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum Substitutions {
    Table,
    Id,
    FromFoodId,
    ToFoodId,
    Ratio,
    ConversionNote,
    AppliesWhen,
    Confidence,
    Bidirectional,
    Organization,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}
