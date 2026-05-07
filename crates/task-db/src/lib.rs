//! Task DB — SeaORM entities and database layer.
//!
//! ## Architecture
//!
//! - **SQLite/SeaORM** = source of truth for generated CRUD resources
//! - **Markdown files** = compatibility/import surface for vault-oriented workflows
//! - **Sync**: compatibility services can project file changes into the database

pub mod auth_adapter;
pub mod entities;
pub mod migration;
pub mod seed;

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
