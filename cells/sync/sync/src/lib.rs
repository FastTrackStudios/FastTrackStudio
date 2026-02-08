//! Sync Cell — OAuth2 authentication, cloud sync engine, and sync services
//!
//! This crate provides:
//! - OAuth2 login flows for GitHub and Google (`oauth` module)
//! - Secure token storage via system keychain (`token_store` module)
//! - `AuthServiceImpl` implementing the `AuthService` trait
//! - `SyncStatusServiceImpl` implementing the `SyncStatusService` trait
//! - `SyncEngine` for local ↔ cloud synchronization with conflict resolution (`engine` module)
//! - `BackgroundSync` for periodic automatic sync (`background` module)
//! - Users table DDL for cloud database schema

pub mod background;
pub mod engine;
pub mod error;
pub mod oauth;
pub mod schema;
pub mod service;
pub mod token_store;

pub use background::BackgroundSync;
pub use engine::SyncEngine;
pub use error::{AuthError, AuthResult, SyncError};
pub use service::auth::AuthServiceImpl;
pub use service::sync_status::SyncStatusServiceImpl;
pub use token_store::{MockTokenStore, TokenStore};
