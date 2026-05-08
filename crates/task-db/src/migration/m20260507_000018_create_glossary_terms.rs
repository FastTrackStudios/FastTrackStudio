//! Create the `glossary_terms` table.
//!
//! Idempotent on duplicate-table errors. Per repo convention no FKs
//! are declared. Indexes:
//!   - `(organization, slug)` unique — slugs are unique within an org
//!     namespace; the global namespace (organization IS NULL) is a
//!     separate scope.
//!   - `category` for filtering by domain ("cooking" / "audio-production").
//!   - `name` for fast lookup.

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
                    .table(GlossaryTerms::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(GlossaryTerms::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(GlossaryTerms::Name).text().not_null())
                    .col(ColumnDef::new(GlossaryTerms::Slug).text().not_null())
                    .col(
                        ColumnDef::new(GlossaryTerms::BodyMarkdown)
                            .text()
                            .not_null()
                            .default(""),
                    )
                    .col(
                        ColumnDef::new(GlossaryTerms::Aliases)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(GlossaryTerms::Category)
                            .string_len(64)
                            .not_null()
                            .default("general"),
                    )
                    .col(
                        ColumnDef::new(GlossaryTerms::RelatedTermIds)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(ColumnDef::new(GlossaryTerms::Organization).string_len(100))
                    .col(ColumnDef::new(GlossaryTerms::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(GlossaryTerms::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(GlossaryTerms::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(GlossaryTerms::UpdatedAt)
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
                    .name("idx_glossary_terms_org_slug")
                    .table(GlossaryTerms::Table)
                    .col(GlossaryTerms::Organization)
                    .col(GlossaryTerms::Slug)
                    .unique()
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_glossary_terms_category")
                    .table(GlossaryTerms::Table)
                    .col(GlossaryTerms::Category)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_glossary_terms_name")
                    .table(GlossaryTerms::Table)
                    .col(GlossaryTerms::Name)
                    .to_owned(),
            )
            .await;
        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(GlossaryTerms::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum GlossaryTerms {
    Table,
    Id,
    Name,
    Slug,
    BodyMarkdown,
    Aliases,
    Category,
    RelatedTermIds,
    Organization,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}
