use sea_orm_migration::prelude::*;

#[derive(DeriveMigrationName)]
pub struct Migration;

#[async_trait::async_trait]
impl MigrationTrait for Migration {
    async fn up(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        let result = manager
            .create_table(
                Table::create()
                    .table(Tracks::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Tracks::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Tracks::ProjectId).uuid().not_null())
                    .col(ColumnDef::new(Tracks::Title).text().not_null())
                    .col(
                        ColumnDef::new(Tracks::Sequence)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(
                        ColumnDef::new(Tracks::Status)
                            .string_len(16)
                            .not_null()
                            .default("composing"),
                    )
                    .col(ColumnDef::new(Tracks::Bpm).double())
                    .col(ColumnDef::new(Tracks::Key).string_len(32))
                    .col(ColumnDef::new(Tracks::DurationMs).big_integer())
                    .col(ColumnDef::new(Tracks::TimeSignature).string_len(16))
                    .col(ColumnDef::new(Tracks::DawSessionPath).text())
                    .col(ColumnDef::new(Tracks::StemsPath).text())
                    .col(ColumnDef::new(Tracks::ReferenceUrl).text())
                    .col(ColumnDef::new(Tracks::Isrc).string_len(32))
                    .col(ColumnDef::new(Tracks::Artist).string_len(255))
                    .col(ColumnDef::new(Tracks::Notes).text())
                    .col(
                        ColumnDef::new(Tracks::RevisionNumber)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(ColumnDef::new(Tracks::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(Tracks::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(Tracks::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Tracks::UpdatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .to_owned(),
            )
            .await;
        match result {
            Ok(()) => {}
            Err(err) if is_duplicate_table(&err) => return Ok(()),
            Err(err) => return Err(err),
        }

        // Helpful indexes — non-unique so seed re-runs and out-of-order
        // inserts are fine.
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_tracks_project")
                    .table(Tracks::Table)
                    .col(Tracks::ProjectId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_tracks_project_sequence")
                    .table(Tracks::Table)
                    .col(Tracks::ProjectId)
                    .col(Tracks::Sequence)
                    .to_owned(),
            )
            .await;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(Tracks::Table).to_owned())
            .await?;
        Ok(())
    }
}

fn is_duplicate_table(err: &DbErr) -> bool {
    let message = err.to_string().to_ascii_lowercase();
    message.contains("already exists") || message.contains("duplicate")
}

#[derive(DeriveIden)]
enum Tracks {
    Table,
    Id,
    ProjectId,
    Title,
    Sequence,
    Status,
    Bpm,
    Key,
    DurationMs,
    TimeSignature,
    DawSessionPath,
    StemsPath,
    ReferenceUrl,
    Isrc,
    Artist,
    Notes,
    RevisionNumber,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}
