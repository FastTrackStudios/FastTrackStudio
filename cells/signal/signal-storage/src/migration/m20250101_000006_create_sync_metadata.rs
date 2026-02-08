use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(SyncMetadata::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(SyncMetadata::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(SyncMetadata::EntityType)
                            .string_len(50)
                            .not_null(),
                    )
                    .col(ColumnDef::new(SyncMetadata::EntityId).uuid().not_null())
                    .col(
                        ColumnDef::new(SyncMetadata::LastSyncAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(SyncMetadata::SyncStatus)
                            .string_len(20)
                            .not_null()
                            .default("pending"),
                    )
                    .col(
                        ColumnDef::new(SyncMetadata::LocalVersion)
                            .big_integer()
                            .not_null()
                            .default(0),
                    )
                    .col(
                        ColumnDef::new(SyncMetadata::RemoteVersion)
                            .big_integer()
                            .not_null()
                            .default(0),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_sync_entity")
                    .table(SyncMetadata::Table)
                    .col(SyncMetadata::EntityType)
                    .col(SyncMetadata::EntityId)
                    .unique()
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(SyncMetadata::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum SyncMetadata {
    Table,
    Id,
    EntityType,
    EntityId,
    LastSyncAt,
    SyncStatus,
    LocalVersion,
    RemoteVersion,
}
