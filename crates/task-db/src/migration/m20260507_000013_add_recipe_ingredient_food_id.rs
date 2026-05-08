//! Add nullable `food_id` column to `recipe_ingredients`.
//!
//! Soft FK to `foods.id` — populated by name-match on recipe insert
//! (see `crate::food::find_food_by_name`). Idempotent on
//! duplicate-column. Per repo convention no FK constraint is declared.

use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

fn is_duplicate_column(err: &DbErr) -> bool {
    let message = err.to_string().to_ascii_lowercase();
    message.contains("duplicate column")
}

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        let result = manager
            .alter_table(
                Table::alter()
                    .table(RecipeIngredients::Table)
                    .add_column(ColumnDef::new(RecipeIngredients::FoodId).uuid())
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
                    .table(RecipeIngredients::Table)
                    .drop_column(RecipeIngredients::FoodId)
                    .to_owned(),
            )
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum RecipeIngredients {
    Table,
    FoodId,
}
