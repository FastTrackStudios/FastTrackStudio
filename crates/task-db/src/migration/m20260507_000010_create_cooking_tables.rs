//! Create the cooking workflow tables: recipes, recipe_ingredients,
//! recipe_steps, cookbooks, cookbook_recipes, meal_plan_entries,
//! shopping_lists, shopping_list_items.
//!
//! All `create_table` calls are idempotent on duplicate-table errors —
//! this migration is safe to re-run against a partially-applied schema.

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
        // ── recipes ─────────────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(Recipes::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Recipes::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Recipes::Name).text().not_null())
                    .col(ColumnDef::new(Recipes::Slug).text().not_null())
                    .col(ColumnDef::new(Recipes::Description).text())
                    .col(ColumnDef::new(Recipes::Organization).string_len(100))
                    .col(ColumnDef::new(Recipes::PrepTimeMinutes).integer())
                    .col(ColumnDef::new(Recipes::CookTimeMinutes).integer())
                    .col(ColumnDef::new(Recipes::TotalTimeMinutes).integer())
                    .col(ColumnDef::new(Recipes::Servings).integer())
                    .col(ColumnDef::new(Recipes::YieldLabel).string_len(64))
                    .col(ColumnDef::new(Recipes::SourceUrl).text())
                    .col(ColumnDef::new(Recipes::Rating).float())
                    .col(ColumnDef::new(Recipes::LastMade).date())
                    .col(ColumnDef::new(Recipes::Notes).text())
                    .col(ColumnDef::new(Recipes::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(Recipes::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(Recipes::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Recipes::UpdatedAt)
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
                    .name("idx_recipes_org_slug")
                    .table(Recipes::Table)
                    .col(Recipes::Organization)
                    .col(Recipes::Slug)
                    .unique()
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_recipes_name")
                    .table(Recipes::Table)
                    .col(Recipes::Name)
                    .to_owned(),
            )
            .await;

        // ── recipe_ingredients ───────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(RecipeIngredients::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(RecipeIngredients::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(RecipeIngredients::RecipeId)
                            .uuid()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(RecipeIngredients::Sequence)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(ColumnDef::new(RecipeIngredients::Quantity).double())
                    .col(ColumnDef::new(RecipeIngredients::Unit).string_len(64))
                    .col(ColumnDef::new(RecipeIngredients::Food).text().not_null())
                    .col(ColumnDef::new(RecipeIngredients::Note).text())
                    .col(
                        ColumnDef::new(RecipeIngredients::IsSection)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(RecipeIngredients::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(RecipeIngredients::UpdatedAt)
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
                    .name("idx_recipe_ingredients_recipe")
                    .table(RecipeIngredients::Table)
                    .col(RecipeIngredients::RecipeId)
                    .to_owned(),
            )
            .await;

        // ── recipe_steps ─────────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(RecipeSteps::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(RecipeSteps::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(RecipeSteps::RecipeId).uuid().not_null())
                    .col(
                        ColumnDef::new(RecipeSteps::Sequence)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(ColumnDef::new(RecipeSteps::Text).text().not_null())
                    .col(ColumnDef::new(RecipeSteps::DurationMinutes).integer())
                    .col(
                        ColumnDef::new(RecipeSteps::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(RecipeSteps::UpdatedAt)
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
                    .name("idx_recipe_steps_recipe")
                    .table(RecipeSteps::Table)
                    .col(RecipeSteps::RecipeId)
                    .to_owned(),
            )
            .await;

        // ── cookbooks ────────────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(Cookbooks::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Cookbooks::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Cookbooks::Name).text().not_null())
                    .col(ColumnDef::new(Cookbooks::Description).text())
                    .col(ColumnDef::new(Cookbooks::Organization).string_len(100))
                    .col(ColumnDef::new(Cookbooks::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(Cookbooks::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Cookbooks::UpdatedAt)
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

        // ── cookbook_recipes ─────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(CookbookRecipes::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(CookbookRecipes::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(CookbookRecipes::CookbookId)
                            .uuid()
                            .not_null(),
                    )
                    .col(ColumnDef::new(CookbookRecipes::RecipeId).uuid().not_null())
                    .col(
                        ColumnDef::new(CookbookRecipes::Sequence)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(
                        ColumnDef::new(CookbookRecipes::AddedAt)
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
                    .name("idx_cookbook_recipes_pair")
                    .table(CookbookRecipes::Table)
                    .col(CookbookRecipes::CookbookId)
                    .col(CookbookRecipes::RecipeId)
                    .unique()
                    .to_owned(),
            )
            .await;

        // ── meal_plan_entries ────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(MealPlanEntries::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(MealPlanEntries::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(MealPlanEntries::Date).date().not_null())
                    .col(
                        ColumnDef::new(MealPlanEntries::MealType)
                            .string_len(16)
                            .not_null()
                            .default("dinner"),
                    )
                    .col(ColumnDef::new(MealPlanEntries::Organization).string_len(100))
                    .col(ColumnDef::new(MealPlanEntries::RecipeId).uuid())
                    .col(ColumnDef::new(MealPlanEntries::Title).text())
                    .col(ColumnDef::new(MealPlanEntries::ServingsPlanned).integer())
                    .col(ColumnDef::new(MealPlanEntries::Notes).text())
                    .col(ColumnDef::new(MealPlanEntries::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(MealPlanEntries::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(MealPlanEntries::UpdatedAt)
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
        // Slot uniqueness — `set_meal_plan_entry` upserts on this triple.
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_meal_plan_org_date_type")
                    .table(MealPlanEntries::Table)
                    .col(MealPlanEntries::Organization)
                    .col(MealPlanEntries::Date)
                    .col(MealPlanEntries::MealType)
                    .unique()
                    .to_owned(),
            )
            .await;

        // ── shopping_lists ───────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(ShoppingLists::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ShoppingLists::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(ShoppingLists::Name).text().not_null())
                    .col(ColumnDef::new(ShoppingLists::Organization).string_len(100))
                    .col(ColumnDef::new(ShoppingLists::CompletedAt).timestamp_with_time_zone())
                    .col(ColumnDef::new(ShoppingLists::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(ShoppingLists::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(ShoppingLists::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ShoppingLists::UpdatedAt)
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

        // ── shopping_list_items ──────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(ShoppingListItems::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ShoppingListItems::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(ShoppingListItems::ListId).uuid().not_null())
                    .col(
                        ColumnDef::new(ShoppingListItems::Sequence)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(ColumnDef::new(ShoppingListItems::Quantity).double())
                    .col(ColumnDef::new(ShoppingListItems::Unit).string_len(64))
                    .col(ColumnDef::new(ShoppingListItems::Food).text().not_null())
                    .col(ColumnDef::new(ShoppingListItems::Note).text())
                    .col(ColumnDef::new(ShoppingListItems::RecipeId).uuid())
                    .col(ColumnDef::new(ShoppingListItems::MealPlanId).uuid())
                    .col(
                        ColumnDef::new(ShoppingListItems::Checked)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(ColumnDef::new(ShoppingListItems::Label).string_len(64))
                    .col(
                        ColumnDef::new(ShoppingListItems::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ShoppingListItems::UpdatedAt)
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
                    .name("idx_shopping_list_items_list")
                    .table(ShoppingListItems::Table)
                    .col(ShoppingListItems::ListId)
                    .to_owned(),
            )
            .await;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(ShoppingListItems::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(ShoppingLists::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(MealPlanEntries::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(CookbookRecipes::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Cookbooks::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(RecipeSteps::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(RecipeIngredients::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Recipes::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum Recipes {
    Table,
    Id,
    Name,
    Slug,
    Description,
    Organization,
    PrepTimeMinutes,
    CookTimeMinutes,
    TotalTimeMinutes,
    Servings,
    YieldLabel,
    SourceUrl,
    Rating,
    LastMade,
    Notes,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum RecipeIngredients {
    Table,
    Id,
    RecipeId,
    Sequence,
    Quantity,
    Unit,
    Food,
    Note,
    IsSection,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum RecipeSteps {
    Table,
    Id,
    RecipeId,
    Sequence,
    Text,
    DurationMinutes,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum Cookbooks {
    Table,
    Id,
    Name,
    Description,
    Organization,
    CreatedBy,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum CookbookRecipes {
    Table,
    Id,
    CookbookId,
    RecipeId,
    Sequence,
    AddedAt,
}

#[derive(DeriveIden)]
enum MealPlanEntries {
    Table,
    Id,
    Date,
    MealType,
    Organization,
    RecipeId,
    Title,
    ServingsPlanned,
    Notes,
    CreatedBy,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum ShoppingLists {
    Table,
    Id,
    Name,
    Organization,
    CompletedAt,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum ShoppingListItems {
    Table,
    Id,
    ListId,
    Sequence,
    Quantity,
    Unit,
    Food,
    Note,
    RecipeId,
    MealPlanId,
    Checked,
    Label,
    CreatedAt,
    UpdatedAt,
}
