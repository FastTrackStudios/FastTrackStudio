//! Auth persistence — SeaORM entities + better-auth adapter + migrations.
//!
//! All the auth tables live here; the rest of the feature suite (the
//! per-feature `<name>-db` crates) only knows about the generic
//! `crdt_doc` / `crdt_update` tables from `crdt-seaorm`. Auth is the
//! one slice of state that is NOT local-first — sessions, account
//! credentials, and OAuth tokens are server-authoritative.

pub mod adapter;
pub mod entities;
pub mod migration;
pub(crate) mod migration_m1;

pub use adapter::SeaOrmAuthAdapter;
pub use migration::Migrator;

pub use sea_orm;
use sea_orm::{Database as SeaDatabase, DatabaseConnection, DbErr};
use sea_orm_migration::MigratorTrait;

/// Open the auth database and migrate-on-connect. Pass anything
/// SeaORM understands (`sqlite::memory:`, `sqlite:///path?mode=rwc`,
/// `postgres://...`).
pub async fn init(db_url: &str) -> Result<DatabaseConnection, DbErr> {
    let db = SeaDatabase::connect(db_url).await?;
    Migrator::up(&db, None).await?;
    Ok(db)
}

/// In-memory sqlite — for tests + the demo binary's `--memory` mode.
pub async fn init_memory() -> Result<DatabaseConnection, DbErr> {
    init("sqlite::memory:").await
}

/// File-backed sqlite at `path`. Creates if missing.
pub async fn init_file(path: &str) -> Result<DatabaseConnection, DbErr> {
    init(&format!("sqlite://{path}?mode=rwc")).await
}
