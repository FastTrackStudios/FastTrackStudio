pub use sea_orm_migration::prelude::*;

mod m20260412_000001_create_tables;
mod m20260412_000002_create_auth_tables;
mod m20260506_000003_add_task_crdt_snapshot;
mod m20260506_000004_create_attachments;

pub struct Migrator;

#[async_trait::async_trait]
impl MigratorTrait for Migrator {
    fn migrations() -> Vec<Box<dyn MigrationTrait>> {
        vec![
            Box::new(m20260412_000001_create_tables::Migration),
            Box::new(m20260412_000002_create_auth_tables::Migration),
            Box::new(m20260506_000003_add_task_crdt_snapshot::Migration),
            Box::new(m20260506_000004_create_attachments::Migration),
        ]
    }
}
