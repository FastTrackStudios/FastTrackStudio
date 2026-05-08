//! Create the `foods` and `food_products` tables.
//!
//! Idempotent on duplicate-table errors. Per repo convention no FKs
//! are declared (SeaORM's API gives trouble there); the soft FK
//! `food_products.food_id -> foods.id` is enforced at the service
//! layer.

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
        // ── foods ───────────────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(Foods::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Foods::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Foods::Name).text().not_null())
                    .col(
                        ColumnDef::new(Foods::Aliases)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(ColumnDef::new(Foods::Category).string_len(64))
                    .col(ColumnDef::new(Foods::DefaultUnit).string_len(32))
                    .col(ColumnDef::new(Foods::Organization).string_len(100))
                    .col(
                        ColumnDef::new(Foods::NutritionPer100G)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(ColumnDef::new(Foods::Notes).text())
                    .col(
                        ColumnDef::new(Foods::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(ColumnDef::new(Foods::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(Foods::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Foods::UpdatedAt)
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
                    .name("idx_foods_org_name")
                    .table(Foods::Table)
                    .col(Foods::Organization)
                    .col(Foods::Name)
                    .unique()
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_foods_name")
                    .table(Foods::Table)
                    .col(Foods::Name)
                    .to_owned(),
            )
            .await;

        // ── food_products ───────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(FoodProducts::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(FoodProducts::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(FoodProducts::FoodId).uuid().not_null())
                    .col(ColumnDef::new(FoodProducts::Barcode).string_len(32))
                    .col(ColumnDef::new(FoodProducts::Brand).string_len(128))
                    .col(ColumnDef::new(FoodProducts::Name).text().not_null())
                    .col(ColumnDef::new(FoodProducts::PackageSizeG).double())
                    .col(ColumnDef::new(FoodProducts::PackageSizeLabel).string_len(64))
                    .col(
                        ColumnDef::new(FoodProducts::Source)
                            .string_len(64)
                            .not_null()
                            .default("manual"),
                    )
                    .col(ColumnDef::new(FoodProducts::ExternalId).string_len(128))
                    .col(
                        ColumnDef::new(FoodProducts::NutritionPer100G)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(ColumnDef::new(FoodProducts::ImageUrl).text())
                    .col(ColumnDef::new(FoodProducts::LastSyncedAt).timestamp_with_time_zone())
                    .col(ColumnDef::new(FoodProducts::Organization).string_len(100))
                    .col(
                        ColumnDef::new(FoodProducts::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(FoodProducts::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(FoodProducts::UpdatedAt)
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
                    .name("idx_food_products_food")
                    .table(FoodProducts::Table)
                    .col(FoodProducts::FoodId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_food_products_barcode")
                    .table(FoodProducts::Table)
                    .col(FoodProducts::Barcode)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_food_products_org_barcode")
                    .table(FoodProducts::Table)
                    .col(FoodProducts::Organization)
                    .col(FoodProducts::Barcode)
                    .unique()
                    .to_owned(),
            )
            .await;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(FoodProducts::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Foods::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum Foods {
    Table,
    Id,
    Name,
    Aliases,
    Category,
    DefaultUnit,
    Organization,
    #[sea_orm(iden = "nutrition_per_100g")]
    NutritionPer100G,
    Notes,
    Properties,
    CreatedBy,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum FoodProducts {
    Table,
    Id,
    FoodId,
    Barcode,
    Brand,
    Name,
    #[sea_orm(iden = "package_size_g")]
    PackageSizeG,
    PackageSizeLabel,
    Source,
    ExternalId,
    #[sea_orm(iden = "nutrition_per_100g")]
    NutritionPer100G,
    ImageUrl,
    LastSyncedAt,
    Organization,
    Properties,
    CreatedAt,
    UpdatedAt,
}
