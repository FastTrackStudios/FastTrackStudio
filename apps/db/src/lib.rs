//! Library face of `task-db`. Re-exports the migration + seed
//! steps so other binaries (notably `apps/sync-demo`) can run them
//! in-process — necessary for in-memory sqlite where each
//! `Database::connect` gives a fresh empty store, so seed and
//! serve have to share one connection.

use crdt_seaorm::SeaOrmPersistence;
use eyre::WrapErr;
use sea_orm::Database;
use sea_orm_migration::MigratorTrait;
use uuid::{Uuid, uuid};

pub mod seed;

/// Shared workspace doc id — every seeded entity lands here. Same
/// constant as `task-ui`'s `sync::WORKSPACE_DOC_ID` so demo clients
/// see the seeded data when they connect.
pub const WORKSPACE_DOC_ID: Uuid = uuid!("00000000-0000-0000-0000-000000000001");

/// Default database URL when nothing else is set. In-memory sqlite
/// keeps the demo zero-fuss; switch to a file URL via
/// `SYNC_DEMO_DATABASE_URL=sqlite://./sync-demo.db?mode=rwc` when
/// you want state to survive process restarts.
pub fn default_database_url() -> String {
    std::env::var("SYNC_DEMO_DATABASE_URL").unwrap_or_else(|_| "sqlite::memory:".into())
}

/// Connect, run pending migrations, return the wrapped persistence
/// handle ready for repos / the sync server / the seeder.
pub async fn open_and_migrate(database_url: &str) -> eyre::Result<SeaOrmPersistence> {
    let db = Database::connect(database_url)
        .await
        .wrap_err("connect db")?;
    crdt_seaorm::Migrator::up(&db, None)
        .await
        .wrap_err("run migrations")?;
    Ok(SeaOrmPersistence::new(db))
}
