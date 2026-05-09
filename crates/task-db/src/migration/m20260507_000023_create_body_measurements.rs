//! Create the `body_measurements` table — point-in-time body metrics
//! (weight, body-fat %, circumferences, vitals).
//!
//! Idempotent on duplicate-table errors. Per repo convention no FK
//! constraints are declared. Photos hang off via the polymorphic
//! `attachments` table (`owner_type = "body_measurement"`).

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
                    .table(BodyMeasurements::Table)
                    .if_not_exists()
                    .col(
                        ColumnDef::new(BodyMeasurements::Id)
                            .uuid()
                            .not_null()
                            .primary_key(),
                    )
                    .col(
                        ColumnDef::new(BodyMeasurements::MeasuredAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(ColumnDef::new(BodyMeasurements::WeightKg).double())
                    .col(ColumnDef::new(BodyMeasurements::BodyFatPercent).float())
                    .col(ColumnDef::new(BodyMeasurements::MuscleMassKg).double())
                    .col(ColumnDef::new(BodyMeasurements::WaterPercent).float())
                    .col(ColumnDef::new(BodyMeasurements::NeckCm).double())
                    .col(ColumnDef::new(BodyMeasurements::ChestCm).double())
                    .col(ColumnDef::new(BodyMeasurements::WaistCm).double())
                    .col(ColumnDef::new(BodyMeasurements::HipCm).double())
                    .col(ColumnDef::new(BodyMeasurements::LeftThighCm).double())
                    .col(ColumnDef::new(BodyMeasurements::RightThighCm).double())
                    .col(ColumnDef::new(BodyMeasurements::LeftArmCm).double())
                    .col(ColumnDef::new(BodyMeasurements::RightArmCm).double())
                    .col(ColumnDef::new(BodyMeasurements::LeftCalfCm).double())
                    .col(ColumnDef::new(BodyMeasurements::RightCalfCm).double())
                    .col(ColumnDef::new(BodyMeasurements::RestingHr).integer())
                    .col(ColumnDef::new(BodyMeasurements::BloodPressureSystolic).integer())
                    .col(ColumnDef::new(BodyMeasurements::BloodPressureDiastolic).integer())
                    .col(
                        ColumnDef::new(BodyMeasurements::Notes)
                            .text()
                            .not_null()
                            .default(""),
                    )
                    .col(ColumnDef::new(BodyMeasurements::Organization).string_len(100))
                    .col(ColumnDef::new(BodyMeasurements::CreatedBy).string_len(100))
                    .col(
                        ColumnDef::new(BodyMeasurements::Properties)
                            .json()
                            .not_null()
                            .default("{}"),
                    )
                    .col(
                        ColumnDef::new(BodyMeasurements::CreatedAt)
                            .timestamp_with_time_zone()
                            .not_null(),
                    )
                    .col(
                        ColumnDef::new(BodyMeasurements::UpdatedAt)
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
                    .name("idx_body_measurements_org_measured")
                    .table(BodyMeasurements::Table)
                    .col(BodyMeasurements::Organization)
                    .col(BodyMeasurements::MeasuredAt)
                    .to_owned(),
            )
            .await;
        let _ = manager
            .create_index(
                Index::create()
                    .name("idx_body_measurements_measured_at")
                    .table(BodyMeasurements::Table)
                    .col(BodyMeasurements::MeasuredAt)
                    .to_owned(),
            )
            .await;

        Ok(())
    }

    async fn down(&self, manager: &SchemaManager) -> Result<(), DbErr> {
        manager
            .drop_table(Table::drop().table(BodyMeasurements::Table).to_owned())
            .await?;
        Ok(())
    }
}

#[derive(DeriveIden)]
enum BodyMeasurements {
    Table,
    Id,
    MeasuredAt,
    WeightKg,
    BodyFatPercent,
    MuscleMassKg,
    WaterPercent,
    NeckCm,
    ChestCm,
    WaistCm,
    HipCm,
    LeftThighCm,
    RightThighCm,
    LeftArmCm,
    RightArmCm,
    LeftCalfCm,
    RightCalfCm,
    RestingHr,
    BloodPressureSystolic,
    BloodPressureDiastolic,
    Notes,
    Organization,
    CreatedBy,
    Properties,
    CreatedAt,
    UpdatedAt,
}
