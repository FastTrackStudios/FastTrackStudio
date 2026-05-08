pub use sea_orm_migration::prelude::*;

mod m20260412_000001_create_tables;
mod m20260412_000002_create_auth_tables;
mod m20260506_000003_add_task_crdt_snapshot;
mod m20260506_000004_create_attachments;
mod m20260507_000001_add_task_properties;
mod m20260507_000002_add_project_properties;
mod m20260507_000003_add_calendar_event_properties;
mod m20260507_000004_add_comment_properties;
mod m20260507_000005_add_person_properties;
mod m20260507_000006_add_asset_properties;
mod m20260507_000007_add_location_properties;
mod m20260507_000008_create_property_definitions;
mod m20260507_000009_create_tracks;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20260412_000001_create_tables::Migration),
            Box::new(m20260412_000002_create_auth_tables::Migration),
            Box::new(m20260506_000003_add_task_crdt_snapshot::Migration),
            Box::new(m20260506_000004_create_attachments::Migration),
            Box::new(m20260507_000001_add_task_properties::Migration),
            Box::new(m20260507_000002_add_project_properties::Migration),
            Box::new(m20260507_000003_add_calendar_event_properties::Migration),
            Box::new(m20260507_000004_add_comment_properties::Migration),
            Box::new(m20260507_000005_add_person_properties::Migration),
            Box::new(m20260507_000006_add_asset_properties::Migration),
            Box::new(m20260507_000007_add_location_properties::Migration),
            Box::new(m20260507_000008_create_property_definitions::Migration),
            Box::new(m20260507_000009_create_tracks::Migration),
        ]
    }
}
