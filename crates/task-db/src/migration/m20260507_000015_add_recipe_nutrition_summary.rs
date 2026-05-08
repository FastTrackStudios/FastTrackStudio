//! Add `nutrition_summary` JSON column to `recipes`.
//!
//! Caches per-recipe aggregated nutrition (whole batch + per-serving)
//! so list views don't recompute. Repopulated by
//! `CookingService::recompute_recipe_nutrition` whenever the recipe or
//! its ingredients change.
//!
//! Idempotent on duplicate-column.

use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        let result = manager
            .alter_table(
                Table::alter()
                    .table(Recipes::Table)
                    .add_column(
                        ColumnDef::new(Recipes::NutritionSummary)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .to_owned(),
            )
            .await;
        match result {
            Ok(()) => Ok(()),
            Err(err) if is_duplicate_column(&err) => Ok(()),
            Err(err) => Err(err),
        }
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(Recipes::Table)
                    .drop_column(Recipes::NutritionSummary)
                    .to_owned(),
            )
            .await?;
        Ok(())
    }
}

fn is_duplicate_column(err: &DbErr) -> bool {
    let message = err.to_string().to_ascii_lowercase();
    message.contains("duplicate column")
}

#[derive(DeriveIden)]
enum Recipes {
    Table,
    NutritionSummary,
}
