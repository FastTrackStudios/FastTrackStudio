use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // 1. Add instrument_type + template_id to presets (separate ALTER for SQLite)
        manager
            .alter_table(
                Table::alter()
                    .table(Presets::Table)
                    .add_column(
                        ColumnDef::new(Presets::InstrumentType)
                            .string_len(50)
                            .not_null()
                            .default("guitar"),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Presets::Table)
                    .add_column(ColumnDef::new(Presets::TemplateId).uuid().null())
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_presets_instrument_type")
                    .table(Presets::Table)
                    .col(Presets::InstrumentType)
                    .to_owned(),
            )
            .await?;

        // 2. Add instrument_type to performance_songs
        manager
            .alter_table(
                Table::alter()
                    .table(PerformanceSongs::Table)
                    .add_column(
                        ColumnDef::new(PerformanceSongs::InstrumentType)
                            .string_len(50)
                            .not_null()
                            .default("guitar"),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_songs_instrument_type")
                    .table(PerformanceSongs::Table)
                    .col(PerformanceSongs::InstrumentType)
                    .to_owned(),
            )
            .await?;

        // 3. Add instrument_type to profiles
        manager
            .alter_table(
                Table::alter()
                    .table(Profiles::Table)
                    .add_column(
                        ColumnDef::new(Profiles::InstrumentType)
                            .string_len(50)
                            .not_null()
                            .default("guitar"),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_profiles_instrument_type")
                    .table(Profiles::Table)
                    .col(Profiles::InstrumentType)
                    .to_owned(),
            )
            .await?;

        // 4. Create song_roles table
        manager
            .create_table(
                Table::create()
                    .table(SongRoles::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(SongRoles::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(SongRoles::SongId).uuid().not_null())
                    .col(ColumnDef::new(SongRoles::Name).string_len(255).not_null())
                    .col(
                        ColumnDef::new(SongRoles::IsDefault)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(SongRoles::SortOrder)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(
                        ColumnDef::new(SongRoles::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(SongRoles::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(SongRoles::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .from(SongRoles::Table, SongRoles::SongId)
                            .to(PerformanceSongs::Table, PerformanceSongs::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_song_roles_song_id")
                    .table(SongRoles::Table)
                    .col(SongRoles::SongId)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_song_roles_song_sort")
                    .table(SongRoles::Table)
                    .col(SongRoles::SongId)
                    .col(SongRoles::SortOrder)
                    .to_owned(),
            )
            .await?;

        // 5. Add role_id to song_scenes
        manager
            .alter_table(
                Table::alter()
                    .table(SongScenes::Table)
                    .add_column(ColumnDef::new(SongScenes::RoleId).uuid().null())
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .alter_table(
                Table::alter()
                    .table(SongScenes::Table)
                    .drop_column(SongScenes::RoleId)
                    .to_owned(),
            )
            .await?;

        manager
            .drop_table(Table::drop().table(SongRoles::Table).to_owned())
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Profiles::Table)
                    .drop_column(Profiles::InstrumentType)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(PerformanceSongs::Table)
                    .drop_column(PerformanceSongs::InstrumentType)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Presets::Table)
                    .drop_column(Presets::InstrumentType)
                    .to_owned(),
            )
            .await?;

        manager
            .alter_table(
                Table::alter()
                    .table(Presets::Table)
                    .drop_column(Presets::TemplateId)
                    .to_owned(),
            )
            .await
    }
}

#[derive(DeriveIden)]
enum Presets {
    Table,
    InstrumentType,
    TemplateId,
}

#[derive(DeriveIden)]
enum PerformanceSongs {
    Table,
    #[sea_orm(iden = "id")]
    Id,
    InstrumentType,
}

#[derive(DeriveIden)]
enum Profiles {
    Table,
    InstrumentType,
}

#[derive(DeriveIden)]
enum SongRoles {
    Table,
    Id,
    SongId,
    Name,
    IsDefault,
    SortOrder,
    Tags,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum SongScenes {
    Table,
    RoleId,
}
