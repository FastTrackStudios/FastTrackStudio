//! Create the `food_logs` table — daily food consumption diary.
//!
//! Each row is an immutable snapshot: at log time we copy the
//! food/product's nutrition (kcal/protein/carbs/fat/…) into the row so
//! historical data doesn't drift if the underlying Food.nutrition_per_100g
//! is later corrected. Per repo convention no FKs are declared; soft
//! references are enforced at the service layer.

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
                    .table(FoodLogs::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(FoodLogs::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(FoodLogs::Date).date().not_null())
                    .col(
                        ColumnDef::new(FoodLogs::MealType)
                            .string_len(16)
                            .not_null()
                            .default("other"),
                    )
                    .col(ColumnDef::new(FoodLogs::FoodId).uuid())
                    .col(ColumnDef::new(FoodLogs::ProductId).uuid())
                    .col(ColumnDef::new(FoodLogs::FoodName).text().not_null())
                    .col(
                        ColumnDef::new(FoodLogs::QuantityGrams)
                            .double()
                            .not_null()
                            .default(0.0),
                    )
                    .col(ColumnDef::new(FoodLogs::Kcal).double())
                    .col(ColumnDef::new(FoodLogs::ProteinG).double())
                    .col(ColumnDef::new(FoodLogs::CarbsG).double())
                    .col(ColumnDef::new(FoodLogs::SugarsG).double())
                    .col(ColumnDef::new(FoodLogs::FiberG).double())
                    .col(ColumnDef::new(FoodLogs::FatG).double())
                    .col(ColumnDef::new(FoodLogs::SaturatedFatG).double())
                    .col(ColumnDef::new(FoodLogs::SodiumMg).double())
                    .col(ColumnDef::new(FoodLogs::Notes).text())
                    .col(ColumnDef::new(FoodLogs::CreatedBy).string_len(100))
                    .col(ColumnDef::new(FoodLogs::MealPlanEntryId).uuid())
                    .col(ColumnDef::new(FoodLogs::RecipeId).uuid())
                    .col(ColumnDef::new(FoodLogs::Organization).string_len(100))
                    .col(
                        ColumnDef::new(FoodLogs::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(FoodLogs::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(FoodLogs::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await;
        match result {
            Ok(()) => {}
            Err(err) if is_duplicate_table(&err) => {}
            Err(err) => return Err(err),
        }

        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_food_logs_org_date")
                    .table(FoodLogs::Table)
                    .col(FoodLogs::Organization)
                    .col(FoodLogs::Date)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_food_logs_meal_plan")
                    .table(FoodLogs::Table)
                    .col(FoodLogs::MealPlanEntryId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_food_logs_food")
                    .table(FoodLogs::Table)
                    .col(FoodLogs::FoodId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_food_logs_recipe")
                    .table(FoodLogs::Table)
                    .col(FoodLogs::RecipeId)
                    .to_owned(),
            )
            .await;
        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(FoodLogs::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum FoodLogs {
    Table,
    Id,
    Date,
    MealType,
    FoodId,
    ProductId,
    FoodName,
    QuantityGrams,
    Kcal,
    ProteinG,
    CarbsG,
    SugarsG,
    FiberG,
    FatG,
    SaturatedFatG,
    SodiumMg,
    Notes,
    CreatedBy,
    MealPlanEntryId,
    RecipeId,
    Organization,
    Properties,
    CreatedAt,
    UpdatedAt,
}
