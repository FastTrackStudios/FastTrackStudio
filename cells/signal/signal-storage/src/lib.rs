//! Signal Storage — SeaORM entities and migrations for signal preset storage.
//!
//! Provides:
//! - [`entities`] — SeaORM entity models for all storage tables
//! - [`migration`] — Versioned database migrations via sea-orm-migration
//! - [`service`] — Rating CRUD operations with business rules
//! - [`StorageError`] — Unified error type for storage operations

pub mod entities;
pub mod error;
pub mod migration;
pub mod service;

pub use error::{StorageError, StorageResult};
pub use migration::Migrator;
pub use service::{RatingInfo, RatingStats};
