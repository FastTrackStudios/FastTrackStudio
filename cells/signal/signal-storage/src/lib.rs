//! Signal Storage — database-backed storage for presets, snapshots, and rig configurations.
//!
//! Provides a [`StorageService`] trait with implementations for:
//! - [`LocalStorage`] — SQLite backend for fast local persistence with FTS5 search
//! - [`CloudStorage`] — PostgreSQL backend for cloud sync and sharing
//!
//! Uses [sea-query](https://docs.rs/sea-query) for database-agnostic query building
//! and [sqlx](https://docs.rs/sqlx) for async execution.

pub mod cloud;
pub mod config;
pub mod error;
pub mod local;
pub mod local_config;
pub mod schema;
pub mod service;

pub use cloud::CloudStorage;
pub use config::{CloudConfig, CloudProvider};
pub use error::{StorageError, StorageResult};
pub use local::LocalStorage;
pub use local_config::LocalConfig;
pub use service::{PresetFilter, PresetSummary, StorageService};
