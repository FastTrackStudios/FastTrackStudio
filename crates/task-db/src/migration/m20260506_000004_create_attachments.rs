use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(Attachments::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(Attachments::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(Attachments::OwnerId).uuid().not_null())
                    .col(
                        ColumnDef::new(Attachments::OwnerType)
                            .string_len(50)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Attachments::Source)
                            .string_len(50)
                            .not_null(),
                    )
                    .col(ColumnDef::new(Attachments::Path).text().not_null())
                    .col(ColumnDef::new(Attachments::Label).string_len(255))
                    .col(ColumnDef::new(Attachments::Mime).string_len(255))
                    .col(ColumnDef::new(Attachments::SizeBytes).big_integer())
                    .col(ColumnDef::new(Attachments::Checksum).string_len(128))
                    .col(ColumnDef::new(Attachments::Uploader).string_len(100))
                    .col(
                        ColumnDef::new(Attachments::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Attachments::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_attachments_owner")
                    .table(Attachments::Table)
                    .col(Attachments::OwnerId)
                    .col(Attachments::OwnerType)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_attachments_source")
                    .table(Attachments::Table)
                    .col(Attachments::Source)
                    .to_owned(),
            )
            .await?;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Attachments::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum Attachments {
    Table,
    Id,
    OwnerId,
    OwnerType,
    Source,
    Path,
    Label,
    Mime,
    SizeBytes,
    Checksum,
    Uploader,
    CreatedAt,
    UpdatedAt,
}
