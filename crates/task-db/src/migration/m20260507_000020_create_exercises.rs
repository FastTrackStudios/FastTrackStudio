//! Create the `exercises` table — canonical fitness movement catalog.
//!
//! Idempotent on duplicate-table errors. Per repo convention no FK
//! constraints are declared.

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
                    .table(Exercises::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Exercises::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Exercises::Name).text().not_null())
                    .col(ColumnDef::new(Exercises::Slug).text().not_null())
                    .col(
                        ColumnDef::new(Exercises::Aliases)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(Exercises::Modality)
                            .string_len(16)
                            .not_null()
                            .default("strength"),
                    )
                    .col(ColumnDef::new(Exercises::PrimaryMuscle).string_len(64))
                    .col(
                        ColumnDef::new(Exercises::SecondaryMuscles)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(ColumnDef::new(Exercises::Equipment).string_len(64))
                    .col(
                        ColumnDef::new(Exercises::BodyMarkdown)
                            .text()
                            .not_null()
                            .default(""),
                    )
                    .col(ColumnDef::new(Exercises::MediaUrl).text())
                    .col(ColumnDef::new(Exercises::Organization).string_len(100))
                    .col(ColumnDef::new(Exercises::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(Exercises::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(Exercises::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Exercises::UpdatedAt)
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
                    .name("idx_exercises_org_slug")
                    .table(Exercises::Table)
                    .col(Exercises::Organization)
                    .col(Exercises::Slug)
                    .unique()
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_exercises_modality")
                    .table(Exercises::Table)
                    .col(Exercises::Modality)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_exercises_primary_muscle")
                    .table(Exercises::Table)
                    .col(Exercises::PrimaryMuscle)
                    .to_owned(),
            )
            .await;
        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Exercises::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum Exercises {
    Table,
    Id,
    Name,
    Slug,
    Aliases,
    Modality,
    PrimaryMuscle,
    SecondaryMuscles,
    Equipment,
    BodyMarkdown,
    MediaUrl,
    Organization,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}
