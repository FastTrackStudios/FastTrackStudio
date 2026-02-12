//! Signal Storage — SeaORM entities and migrations for signal preset storage.
//!
//! Provides:
//! - [`entities`] — SeaORM entity models for all storage tables
//! - [`migration`] — Versioned database migrations via sea-orm-migration
//! - [`persist`] — Key-value persistence abstraction with SQLite and memory backends
//! - [`StorageError`] — Unified error type for storage operations

pub mod block_repo;
pub mod entities;
pub mod error;
pub mod facet_bridge;
pub mod local_config;
pub mod migration;
pub mod module_repo;
pub mod persist;
pub mod preset_repo;
pub mod profile_repo;
pub mod scene_repo;
pub mod seed;
pub mod setlist_repo;
pub mod snapshot_service;
pub mod song_repo;

pub use error::{StorageError, StorageResult};
pub use local_config::LocalConfig;
pub use migration::Migrator;
pub use persist::{load_value, save_value, MemoryBackend, Persistable, SqliteBackend};

// Re-export sea_orm types that consumers need
pub use sea_orm::DatabaseConnection;

/// Connect to a SQLite database at the given path and run all pending migrations.
///
/// Creates the database file and parent directories if they don't exist.
/// Returns the connected `DatabaseConnection` ready for use.
pub async fn connect_and_migrate(db_path: &str) -> Result<DatabaseConnection, StorageError> {
    use sea_orm::Database;
    use sea_orm_migration::MigratorTrait;

    // Ensure parent directory exists
    if let Some(parent) = std::path::Path::new(db_path).parent() {
        std::fs::create_dir_all(parent).map_err(|e| {
            StorageError::Config(format!(
                "failed to create db directory {}: {e}",
                parent.display()
            ))
        })?;
    }

    let url = format!("sqlite://{}?mode=rwc", db_path);
    let db = Database::connect(&url).await?;
    Migrator::up(&db, None)
        .await
        .map_err(|e| StorageError::Config(format!("migration failed: {e}")))?;

    Ok(db)
}

/// Connect to a SQLite database, run migrations, and seed with FTS-Guitar defaults
/// if the database is empty.
///
/// This is the recommended entry point for production startup. Use
/// [`connect_and_migrate`] directly if you want to skip seeding (e.g., in tests).
pub async fn connect_migrate_and_seed(db_path: &str) -> Result<DatabaseConnection, StorageError> {
    let db = connect_and_migrate(db_path).await?;
    seed::seed_if_empty(&db).await?;
    Ok(db)
}
