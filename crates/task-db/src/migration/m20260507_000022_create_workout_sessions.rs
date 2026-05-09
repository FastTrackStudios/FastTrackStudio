//! Create the `workout_sessions` and `set_logs` tables — actuals layer
//! of the fitness workflow.
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
        // ── workout_sessions ─────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(WorkoutSessions::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(WorkoutSessions::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(ColumnDef::new(WorkoutSessions::RoutineId).uuid())
                    .col(
                        ColumnDef::new(WorkoutSessions::RoutineNameSnapshot)
                            .text()
                            .not_null()
                            .default(""),
                    )
                    .col(
                        ColumnDef::new(WorkoutSessions::Status)
                            .string_len(16)
                            .not_null()
                            .default("active"),
                    )
                    .col(
                        ColumnDef::new(WorkoutSessions::StartedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(ColumnDef::new(WorkoutSessions::CompletedAt).timestamp_with_time_zone())
                    .col(
                        ColumnDef::new(WorkoutSessions::Notes)
                            .text()
                            .not_null()
                            .default(""),
                    )
                    .col(ColumnDef::new(WorkoutSessions::OverallRpe).float())
                    .col(ColumnDef::new(WorkoutSessions::BodyweightKg).double())
                    .col(ColumnDef::new(WorkoutSessions::Organization).string_len(100))
                    .col(ColumnDef::new(WorkoutSessions::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(WorkoutSessions::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(WorkoutSessions::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(WorkoutSessions::UpdatedAt)
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
                    .name("idx_workout_sessions_routine")
                    .table(WorkoutSessions::Table)
                    .col(WorkoutSessions::RoutineId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_workout_sessions_org_status")
                    .table(WorkoutSessions::Table)
                    .col(WorkoutSessions::Organization)
                    .col(WorkoutSessions::Status)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_workout_sessions_started_at")
                    .table(WorkoutSessions::Table)
                    .col(WorkoutSessions::StartedAt)
                    .to_owned(),
            )
            .await;

        // ── set_logs ─────────────────────────────────────────────
        let result = manager
            .create_table(
                Table::create()
                    .table(SetLogs::Table)
                    .if_not_exists()
                    .col(ColumnDef::new(SetLogs::Id).uuid().not_null().primary_key())
                    .col(ColumnDef::new(SetLogs::WorkoutSessionId).uuid().not_null())
                    .col(ColumnDef::new(SetLogs::ExerciseId).uuid())
                    .col(
                        ColumnDef::new(SetLogs::ExerciseNameSnapshot)
                            .text()
                            .not_null()
                            .default(""),
                    )
                    .col(ColumnDef::new(SetLogs::RoutineExerciseId).uuid())
                    .col(
                        ColumnDef::new(SetLogs::Position)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(
                        ColumnDef::new(SetLogs::SetIndex)
                            .integer()
                            .not_null()
                            .default(0),
                    )
                    .col(ColumnDef::new(SetLogs::Reps).integer())
                    .col(ColumnDef::new(SetLogs::WeightKg).double())
                    .col(ColumnDef::new(SetLogs::DurationSeconds).integer())
                    .col(ColumnDef::new(SetLogs::DistanceMeters).double())
                    .col(ColumnDef::new(SetLogs::AvgHr).integer())
                    .col(ColumnDef::new(SetLogs::PaceSecondsPerKm).integer())
                    .col(ColumnDef::new(SetLogs::Rpe).float())
                    .col(ColumnDef::new(SetLogs::Notes).text())
                    .col(ColumnDef::new(SetLogs::CompletedAt).timestamp_with_time_zone())
                    .col(
                        ColumnDef::new(SetLogs::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(SetLogs::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(SetLogs::UpdatedAt)
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

        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_set_logs_session")
                    .table(SetLogs::Table)
                    .col(SetLogs::WorkoutSessionId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_set_logs_session_position")
                    .table(SetLogs::Table)
                    .col(SetLogs::WorkoutSessionId)
                    .col(SetLogs::Position)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_set_logs_exercise")
                    .table(SetLogs::Table)
                    .col(SetLogs::ExerciseId)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_set_logs_routine_exercise")
                    .table(SetLogs::Table)
                    .col(SetLogs::RoutineExerciseId)
                    .to_owned(),
            )
            .await;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(SetLogs::Table).to_owned())
            .await?;
        manager
            .drop_table(Table::drop().table(WorkoutSessions::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum WorkoutSessions {
    Table,
    Id,
    RoutineId,
    RoutineNameSnapshot,
    Status,
    StartedAt,
    CompletedAt,
    Notes,
    OverallRpe,
    BodyweightKg,
    Organization,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}

#[derive(DeriveIden)]
enum SetLogs {
    Table,
    Id,
    WorkoutSessionId,
    ExerciseId,
    ExerciseNameSnapshot,
    RoutineExerciseId,
    Position,
    SetIndex,
    Reps,
    WeightKg,
    DurationSeconds,
    DistanceMeters,
    AvgHr,
    PaceSecondsPerKm,
    Rpe,
    Notes,
    CompletedAt,
    Properties,
    CreatedAt,
    UpdatedAt,
}
