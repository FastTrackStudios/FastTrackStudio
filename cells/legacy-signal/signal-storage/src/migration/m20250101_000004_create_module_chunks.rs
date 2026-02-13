use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .create_table(
                Table::create()
                    .table(ModuleChunks::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(ModuleChunks::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(ModuleChunks::PresetId).uuid())
                    .col(
                        ColumnDef::new(ModuleChunks::ModuleType)
                            .string_len(100)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ModuleChunks::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ModuleChunks::Data)
                            .json_binary()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(ModuleChunks::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .from(ModuleChunks::Table, ModuleChunks::PresetId)
                            .to(Presets::Table, Presets::Id)
                            .on_delete(ForeignKeyAction::SetNull),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(ModuleChunks::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum Presets {
    Table,
    Id,
}

#[derive(DeriveIden)]
enum ModuleChunks {
    Table,
    Id,
    PresetId,
    ModuleType,
    Name,
    Data,
    CreatedAt,
}
