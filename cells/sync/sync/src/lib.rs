//! Sync Cell — OAuth2 authentication and cloud sync services
//!
//! This crate provides:
//! - OAuth2 login flows for GitHub and Google (`oauth` module)
//! - Secure token storage via system keychain (`token_store` module)
//! - `AuthServiceImpl` implementing the `AuthService` trait
//! - `SyncStatusServiceImpl` implementing the `SyncStatusService` trait
//! - Users table DDL for cloud database schema

pub mod error;
pub mod oauth;
pub mod schema;
pub mod service;
pub mod token_store;

pub use error::{AuthError, AuthResult};
pub use service::auth::AuthServiceImpl;
pub use service::sync_status::SyncStatusServiceImpl;
pub use token_store::{MockTokenStore, TokenStore};
