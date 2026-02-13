use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // Add is_template to presets
        manager
            .alter_table(
                Table::alter()
                    .table(Presets::Table)
                    .add_column(
                        ColumnDef::new(Presets::IsTemplate)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .to_owned(),
            )
            .await?;

        // Add is_template to profiles
        manager
            .alter_table(
                Table::alter()
                    .table(Profiles::Table)
                    .add_column(
                        ColumnDef::new(Profiles::IsTemplate)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .to_owned(),
            )
            .await?;

        // Add is_template to performance_songs
        manager
            .alter_table(
                Table::alter()
                    .table(PerformanceSongs::Table)
                    .add_column(
                        ColumnDef::new(PerformanceSongs::IsTemplate)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .to_owned(),
            )
            .await?;

        // Add is_template to module_presets
        manager
            .alter_table(
                Table::alter()
                    .table(ModulePresets::Table)
                    .add_column(
                        ColumnDef::new(ModulePresets::IsTemplate)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .to_owned(),
            )
            .await?;

        // Add is_template to block_presets
        manager
            .alter_table(
                Table::alter()
                    .table(BlockPresets::Table)
                    .add_column(
                        ColumnDef::new(BlockPresets::IsTemplate)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(BlockPresets::Table)
                    .drop_column(BlockPresets::IsTemplate)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(ModulePresets::Table)
                    .drop_column(ModulePresets::IsTemplate)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(PerformanceSongs::Table)
                    .drop_column(PerformanceSongs::IsTemplate)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Profiles::Table)
                    .drop_column(Profiles::IsTemplate)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Presets::Table)
                    .drop_column(Presets::IsTemplate)
                    .to_owned(),
            )
            .await
    }
}

#[derive(DeriveIden)]
enum Presets {
    Table,
    IsTemplate,
}

#[derive(DeriveIden)]
enum Profiles {
    Table,
    IsTemplate,
}

#[derive(DeriveIden)]
enum PerformanceSongs {
    Table,
    IsTemplate,
}

#[derive(DeriveIden)]
enum ModulePresets {
    Table,
    IsTemplate,
}

#[derive(DeriveIden)]
enum BlockPresets {
    Table,
    IsTemplate,
}
