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
pub mod setlist_repo;
pub mod snapshot_service;
pub mod song_repo;

pub use error::{StorageError, StorageResult};
pub use local_config::LocalConfig;
pub use migration::Migrator;
pub use persist::{load_value, save_value, MemoryBackend, Persistable, SqliteBackend};

// Re-export sea_orm types that consumers need
pub use sea_orm::DatabaseConnection;
