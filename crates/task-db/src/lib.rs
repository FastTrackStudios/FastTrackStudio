//! Task DB — SeaORM entities and database layer.
//!
//! ## Architecture
//!
//! - **Markdown files** = source of truth for content (bodies, notes, lyrics)
//! - **Database** = operational index for metadata, relations, comments, notifications
//! - **Sync**: file changes → DB update, DB changes → file regeneration

pub mod auth_adapter;
pub mod entities;
pub mod migration;

pub use auth_adapter::SeaOrmAuthAdapter;

pub use sea_orm;
use sea_orm::{Database as SeaDatabase, DatabaseConnection, DbErr};
use sea_orm_migration::MigratorTrait;

/// Initialize the database connection and run migrations.
pub async fn init(db_url: &str) -> Result<DatabaseConnection, DbErr> {
    let db = SeaDatabase::connect(db_url).await?;
    migration::Migrator::up(&db, None).await?;
    Ok(db)
}

/// Initialize an in-memory SQLite database (for testing/mock mode).
pub async fn init_memory() -> Result<DatabaseConnection, DbErr> {
    init("sqlite::memory:").await
}

/// Initialize a file-based SQLite database.
pub async fn init_file(path: &str) -> Result<DatabaseConnection, DbErr> {
    init(&format!("sqlite://{}?mode=rwc", path)).await
}
