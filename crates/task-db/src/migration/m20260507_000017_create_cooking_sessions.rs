//! Create the `cooking_sessions` table — stateful per-cook walkthrough.

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
                    .table(CookingSessions::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(CookingSessions::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(CookingSessions::RecipeId).uuid().not_null())
                    .col(
                        ColumnDef::new(CookingSessions::RecipeNameSnapshot)
                            .text()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(CookingSessions::Status)
                            .string_len(16)
                            .not_null()
                            .default("active"),
                    )
                    .col(
                        ColumnDef::new(CookingSessions::CurrentStepIndex)
                            .integer()
                            .not_null()
                            .default(-1),
                    )
                    .col(ColumnDef::new(CookingSessions::ScaledServings).integer())
                    .col(
                        ColumnDef::new(CookingSessions::MiseEnPlaceState)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(CookingSessions::StepStates)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(ColumnDef::new(CookingSessions::Notes).text())
                    .col(
                        ColumnDef::new(CookingSessions::StartedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(ColumnDef::new(CookingSessions::CompletedAt).timestamp_with_time_zone())
                    .col(ColumnDef::new(CookingSessions::Organization).string_len(100))
                    .col(ColumnDef::new(CookingSessions::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(CookingSessions::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(CookingSessions::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(CookingSessions::UpdatedAt)
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
                    .name("idx_cooking_sessions_recipe")
                    .table(CookingSessions::Table)
                    .col(CookingSessions::RecipeId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_cooking_sessions_org_status")
                    .table(CookingSessions::Table)
                    .col(CookingSessions::Organization)
                    .col(CookingSessions::Status)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_cooking_sessions_started_at")
                    .table(CookingSessions::Table)
                    .col(CookingSessions::StartedAt)
                    .to_owned(),
            )
            .await;
        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(CookingSessions::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum CookingSessions {
    Table,
    Id,
    RecipeId,
    RecipeNameSnapshot,
    Status,
    CurrentStepIndex,
    ScaledServings,
    MiseEnPlaceState,
    StepStates,
    Notes,
    StartedAt,
    CompletedAt,
    Organization,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}
