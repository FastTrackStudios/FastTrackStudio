//! Signal Storage — SeaORM entities and migrations for signal preset storage.
//!
//! Provides:
//! - [`entities`] — SeaORM entity models for all storage tables
//! - [`migration`] — Versioned database migrations via sea-orm-migration
//! - [`StorageError`] — Unified error type for storage operations

pub mod entities;
pub mod error;
pub mod migration;

pub use error::{StorageError, StorageResult};
pub use migration::Migrator;
