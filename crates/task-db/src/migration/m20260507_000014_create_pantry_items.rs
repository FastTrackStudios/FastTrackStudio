//! Create the `pantry_items` table.
//!
//! Idempotent on duplicate-table errors. Per repo convention no FKs are
//! declared; soft FKs `food_id -> foods.id`, `product_id ->
//! food_products.id`, `location_id -> locations.uuid` are enforced at
//! the service layer.

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
                    .table(PantryItems::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(PantryItems::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(PantryItems::FoodId).uuid())
                    .col(ColumnDef::new(PantryItems::ProductId).uuid())
                    .col(ColumnDef::new(PantryItems::LocationId).uuid())
                    .col(
                        ColumnDef::new(PantryItems::Quantity)
                            .double()
                            .not_null()
                            .default(0.0),
                    )
                    .col(
                        ColumnDef::new(PantryItems::Unit)
                            .string_len(32)
                            .not_null()
                            .default(""),
                    )
                    .col(ColumnDef::new(PantryItems::ExpirationDate).date())
                    .col(ColumnDef::new(PantryItems::OpenedAt).timestamp_with_time_zone())
                    .col(ColumnDef::new(PantryItems::MinStock).double())
                    .col(ColumnDef::new(PantryItems::PurchasedAt).date())
                    .col(ColumnDef::new(PantryItems::Notes).text())
                    .col(ColumnDef::new(PantryItems::Organization).string_len(100))
                    .col(
                        ColumnDef::new(PantryItems::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(PantryItems::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(PantryItems::UpdatedAt)
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
                    .name("idx_pantry_items_food")
                    .table(PantryItems::Table)
                    .col(PantryItems::FoodId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_pantry_items_product")
                    .table(PantryItems::Table)
                    .col(PantryItems::ProductId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_pantry_items_location")
                    .table(PantryItems::Table)
                    .col(PantryItems::LocationId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_pantry_items_org_expiration")
                    .table(PantryItems::Table)
                    .col(PantryItems::Organization)
                    .col(PantryItems::ExpirationDate)
                    .to_owned(),
            )
            .await;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(PantryItems::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum PantryItems {
    Table,
    Id,
    FoodId,
    ProductId,
    LocationId,
    Quantity,
    Unit,
    ExpirationDate,
    OpenedAt,
    MinStock,
    PurchasedAt,
    Notes,
    Organization,
    Properties,
    CreatedAt,
    UpdatedAt,
}
