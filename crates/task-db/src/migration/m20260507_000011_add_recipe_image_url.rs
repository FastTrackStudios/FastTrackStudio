//! Add `image_url` column to the `recipes` table.
//!
//! Recipe import (Mealie-style) records the originating image URL on the
//! row so a follow-up step can fetch the bytes and PUT them to Nextcloud.
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
                    .add_column(ColumnDef::new(Recipes::ImageUrl).text())
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
                    .drop_column(Recipes::ImageUrl)
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
    ImageUrl,
}
