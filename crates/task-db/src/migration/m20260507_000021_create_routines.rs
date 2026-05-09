//! Create the `routines` and `routine_exercises` tables.
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
        // ── routines ────────────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(Routines::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(Routines::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(Routines::Name).text().not_null())
                    .col(ColumnDef::new(Routines::Slug).text().not_null())
                    .col(ColumnDef::new(Routines::Description).text())
                    .col(
                        ColumnDef::new(Routines::BodyMarkdown)
                            .text()
                            .not_null()
                            .default(""),
                    )
                    .col(ColumnDef::new(Routines::Category).string_len(64))
                    .col(ColumnDef::new(Routines::EstimatedDurationMinutes).integer())
                    .col(ColumnDef::new(Routines::Difficulty).string_len(32))
                    .col(
                        ColumnDef::new(Routines::Tags)
                            .json()
                            .not_null()
                            .default("[]"),
                    )
                    .col(ColumnDef::new(Routines::Organization).string_len(100))
                    .col(ColumnDef::new(Routines::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(Routines::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(Routines::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(Routines::UpdatedAt)
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
                    .name("idx_routines_org_slug")
                    .table(Routines::Table)
                    .col(Routines::Organization)
                    .col(Routines::Slug)
                    .unique()
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_routines_category")
                    .table(Routines::Table)
                    .col(Routines::Category)
                    .to_owned(),
            )
            .await;

        // ── routine_exercises ───────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(RoutineExercises::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(RoutineExercises::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(RoutineExercises::RoutineId)
                            .uuid()
                            .not_null(),
                    )
                    .col(ColumnDef::new(RoutineExercises::ExerciseId).uuid())
                    .col(
                        ColumnDef::new(RoutineExercises::DisplayName)
                            .text()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(RoutineExercises::Position)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(ColumnDef::new(RoutineExercises::GroupLabel).string_len(16))
                    .col(ColumnDef::new(RoutineExercises::TargetSets).integer())
                    .col(ColumnDef::new(RoutineExercises::TargetReps).integer())
                    .col(ColumnDef::new(RoutineExercises::TargetWeightKg).double())
                    .col(ColumnDef::new(RoutineExercises::TargetRestSeconds).integer())
                    .col(ColumnDef::new(RoutineExercises::TargetRpe).float())
                    .col(ColumnDef::new(RoutineExercises::Tempo).string_len(32))
                    .col(ColumnDef::new(RoutineExercises::TargetDurationSeconds).integer())
                    .col(ColumnDef::new(RoutineExercises::TargetDistanceMeters).double())
                    .col(ColumnDef::new(RoutineExercises::TargetAvgHr).integer())
                    .col(ColumnDef::new(RoutineExercises::TargetPaceSecondsPerKm).integer())
                    .col(ColumnDef::new(RoutineExercises::Notes).text())
                    .col(
                        ColumnDef::new(RoutineExercises::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(RoutineExercises::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(RoutineExercises::UpdatedAt)
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
                    .name("idx_routine_exercises_routine")
                    .table(RoutineExercises::Table)
                    .col(RoutineExercises::RoutineId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_routine_exercises_routine_position")
                    .table(RoutineExercises::Table)
                    .col(RoutineExercises::RoutineId)
                    .col(RoutineExercises::Position)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_routine_exercises_exercise")
                    .table(RoutineExercises::Table)
                    .col(RoutineExercises::ExerciseId)
                    .to_owned(),
            )
            .await;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(RoutineExercises::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(Routines::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum Routines {
    Table,
    Id,
    Name,
    Slug,
    Description,
    BodyMarkdown,
    Category,
    EstimatedDurationMinutes,
    Difficulty,
    Tags,
    Organization,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum RoutineExercises {
    Table,
    Id,
    RoutineId,
    ExerciseId,
    DisplayName,
    Position,
    GroupLabel,
    TargetSets,
    TargetReps,
    #[sea_orm(iden = "target_weight_kg")]
    TargetWeightKg,
    TargetRestSeconds,
    TargetRpe,
    Tempo,
    TargetDurationSeconds,
    TargetDistanceMeters,
    TargetAvgHr,
    TargetPaceSecondsPerKm,
    Notes,
    Properties,
    CreatedAt,
    UpdatedAt,
}
