//! Signal Storage — database-backed storage for presets, snapshots, and rig configurations.
//!
//! Provides a [`StorageService`] trait with implementations for:
//! - [`CloudStorage`] — PostgreSQL backend for cloud sync and sharing
//!
//! Uses [sea-query](https://docs.rs/sea-query) for database-agnostic query building
//! and [sqlx](https://docs.rs/sqlx) for async execution.

pub mod cloud;
pub mod config;
pub mod error;
pub mod schema;
pub mod service;

pub use cloud::CloudStorage;
pub use config::{CloudConfig, CloudProvider};
pub use error::{StorageError, StorageResult};
pub use service::{PresetFilter, PresetSummary, StorageService};
