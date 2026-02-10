//! Migration: Create performance_songs, song_scenes, setlists, and setlist_songs tables.
//!
//! Songs are ordered sequences of scenes for live performance.
//! Setlists compose songs into a show order.

use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        // -- performance_songs: songs with ordered scenes
        manager
            .create_table(
                Table::create()
                    .table(PerformanceSongs::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(PerformanceSongs::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(PerformanceSongs::Name)
                            .string_len(255)
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(PerformanceSongs::Artist)
                            .string_len(255)
                            .null(),
                    )
                    .col(
                        ColumnDef::new(PerformanceSongs::AutoAdvance)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(ColumnDef::new(PerformanceSongs::LinkedSongId).uuid().null())
                    .col(
                        ColumnDef::new(PerformanceSongs::ModuleOverrides)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(PerformanceSongs::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(PerformanceSongs::IsDeleted)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(PerformanceSongs::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(PerformanceSongs::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_performance_songs_name")
                    .table(PerformanceSongs::Table)
                    .col(PerformanceSongs::Name)
                    .to_owned(),
            )
            .await?;

        // -- song_scenes: ordered scenes within a song
        manager
            .create_table(
                Table::create()
                    .table(SongScenes::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(SongScenes::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(SongScenes::SongId).uuid().not_null())
                    .col(ColumnDef::new(SongScenes::Name).string_len(255).not_null())
                    .col(ColumnDef::new(SongScenes::PresetId).uuid().not_null())
                    .col(ColumnDef::new(SongScenes::SnapshotId).uuid().null())
                    .col(
                        ColumnDef::new(SongScenes::Transition)
                            .json_binary()
                            .not_null()
                            .default(
                                r#"{"transition_type":"Instant","fade_time_ms":0,"gap_time_ms":0}"#,
                            ),
                    )
                    .col(
                        ColumnDef::new(SongScenes::MidiTriggers)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(SongScenes::ModuleOverrides)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(SongScenes::BlockOverrides)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(SongScenes::SortOrder)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(
                        ColumnDef::new(SongScenes::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(SongScenes::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(SongScenes::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_song_scenes_song")
                            .from(SongScenes::Table, SongScenes::SongId)
                            .to(PerformanceSongs::Table, PerformanceSongs::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_song_scenes_song_id")
                    .table(SongScenes::Table)
                    .col(SongScenes::SongId)
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_song_scenes_sort_order")
                    .table(SongScenes::Table)
                    .col(SongScenes::SongId)
                    .col(SongScenes::SortOrder)
                    .to_owned(),
            )
            .await?;

        // -- setlists: ordered collections of songs
        manager
            .create_table(
                Table::create()
                    .table(Setlists::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Setlists::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Setlists::Name).string_len(255).not_null())
                    .col(
                        ColumnDef::new(Setlists::Metadata)
                            .json_binary()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(Setlists::Tags)
                            .json_binary()
                            .not_null()
                            .default("[]"),
                    )
                    .col(
                        ColumnDef::new(Setlists::IsDeleted)
                            .boolean()
                            .not_null()
                            .default(false),
                    )
                    .col(
                        ColumnDef::new(Setlists::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Setlists::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_setlists_name")
                    .table(Setlists::Table)
                    .col(Setlists::Name)
                    .to_owned(),
            )
            .await?;

        // -- setlist_songs: join table for setlist <-> song ordering
        manager
            .create_table(
                Table::create()
                    .table(SetlistSongs::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(SetlistSongs::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(SetlistSongs::SetlistId).uuid().not_null())
                    .col(ColumnDef::new(SetlistSongs::SongId).uuid().not_null())
                    .col(
                        ColumnDef::new(SetlistSongs::SortOrder)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_setlist_songs_setlist")
                            .from(SetlistSongs::Table, SetlistSongs::SetlistId)
                            .to(Setlists::Table, Setlists::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .foreign_key(
                        ForeignKey::create()
                            .name("fk_setlist_songs_song")
                            .from(SetlistSongs::Table, SetlistSongs::SongId)
                            .to(PerformanceSongs::Table, PerformanceSongs::Id)
                            .on_delete(ForeignKeyAction::Cascade),
                    )
                    .to_owned(),
            )
            .await?;

        manager
            .create_index(
                Index::create()
                    .name("idx_setlist_songs_setlist_id")
                    .table(SetlistSongs::Table)
                    .col(SetlistSongs::SetlistId)
                    .col(SetlistSongs::SortOrder)
                    .to_owned(),
            )
            .await
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(SetlistSongs::Table).to_owned())
            .await?;

        manager
            .drop_table(Table::drop().table(Setlists::Table).to_owned())
            .await?;

        manager
            .drop_table(Table::drop().table(SongScenes::Table).to_owned())
            .await?;

        manager
            .drop_table(Table::drop().table(PerformanceSongs::Table).to_owned())
            .await
    }
}

#[derive(DeriveIden)]
enum PerformanceSongs {
    Table,
    Id,
    Name,
    Artist,
    AutoAdvance,
    LinkedSongId,
    ModuleOverrides,
    Tags,
    IsDeleted,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum SongScenes {
    Table,
    Id,
    SongId,
    Name,
    PresetId,
    SnapshotId,
    Transition,
    MidiTriggers,
    ModuleOverrides,
    BlockOverrides,
    SortOrder,
    Tags,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum Setlists {
    Table,
    Id,
    Name,
    Metadata,
    Tags,
    IsDeleted,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum SetlistSongs {
    Table,
    Id,
    SetlistId,
    SongId,
    SortOrder,
}
